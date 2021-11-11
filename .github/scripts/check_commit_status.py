#!/usr/bin/env python3

from __future__ import annotations
from asyncio import run, sleep
from asyncio.subprocess import create_subprocess_exec, PIPE, STDOUT
from dataclasses import dataclass
import json
import logging
import os
import sys
from typing import FrozenSet, Optional, Text, Tuple


log = logging.getLogger(__name__)
stream_handler = logging.StreamHandler(sys.stdout)
stream_handler.setLevel(logging.INFO)
log.setLevel(logging.INFO)
log.addHandler(stream_handler)


async def get_text(
        cmd: Text,
        stdin: Optional[Text] = None,
        error_codes: FrozenSet[int] = frozenset({0})
) -> Text:
    """Run shell command (with optional piped stdin) and return text of combined stdout/stderr."""
    if stdin is None:
        proc = await create_subprocess_exec(*cmd.split(), stdout=PIPE, stderr=STDOUT)
        text = (await proc.communicate())[0].decode()
        log.debug(f'{cmd}\n{text}')
    else:
        proc = await create_subprocess_exec(*cmd.split(), stdin=PIPE, stdout=PIPE, stderr=STDOUT)
        text = (await proc.communicate(stdin.encode()))[0].decode()
        log.debug(f'"{stdin}" | {cmd}\n{text}')

    assert proc.returncode in error_codes, f'{cmd = }\n{text = }'

    return text


async def get_lines(
        cmd: Text,
        stdin: Optional[Text] = None,
        error_codes: FrozenSet[int] = frozenset({0})
) -> Tuple[Text]:
    """Run shell command (with optional piped stdin) and return lines of combined stdout/stderr."""
    lines = tuple((await get_text(cmd=cmd, stdin=stdin, error_codes=error_codes)).splitlines())

    return lines


@dataclass(frozen=True)
class Status:
    success: bool

    @staticmethod
    async def from_env() -> Status:
        push_sha = os.environ.get('HM_PUSH_SHA', (await get_text('git rev-parse HEAD')).strip())
        tree = (await get_lines('git cat-file -p HEAD'))[0].strip()
        log.info(f'Expecting status for "{tree}"')

        while True:
            statuses = json.loads(
                await get_text(f'gh api /repos/{{owner}}/{{repo}}/commits/{push_sha}/statuses')
            )

            # Most recent status comes first in list.
            for status in statuses:
                if status['context'] != 'Hemlock Push':
                    continue
                elif not status['description'].endswith(tree):
                    # The commits in the PR were not directly based on origin/main at the time they
                    # were tested. Merging would result in a different tree than was tested.
                    log.error('The commit is not based directly on origin/main. You must rebase.')
                    return Status(success=False)
                elif status['state'] == 'failure':
                    log.error('Push tests detected failures.')
                    return Status(success=False)

                assert status['state'] == 'success', (
                    "Only 'success' and 'failure' statuses supported."
                )

                return Status(success=True)

            # We didn't find any statuses associated with a Hemlock Push. Possibly a race. Try again
            # later.
            await sleep(10)


def main() -> int:
    async def main_inner() -> int:
        status = await Status.from_env()
        if status.success == False:
            return 1

        return 0

    return run(main_inner())


if __name__ == '__main__':
    sys.exit(main())
