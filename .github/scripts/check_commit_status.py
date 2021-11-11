#!/usr/bin/env python3

from __future__ import annotations
from asyncio import run
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
    text: Text

    @staticmethod
    async def from_env() -> Status:
        push_sha = os.environ.get('HM_PUSH_SHA', (await get_text('git rev-parse HEAD')).strip())
        tree = (await get_lines('git cat-file -p HEAD'))[0].strip()
        log.info(f'Expecting "description": "{tree}"')

        statuses = json.loads(
            await get_text(f'gh api /repos/BranchTaken/Hemlock/commits/{push_sha}/statuses')
        )
        # Most recent status comes first in list.
        for status in statuses:
            if status['context'] == 'Hemlock Push':
                log.info(json.dumps(status, indent=4))
                if status['description'] != tree:
                    return Status(text='stale')
                return Status(text=status['state'])

        return Status(text='missing')


def main() -> int:
    async def main_inner() -> int:
        status = await Status.from_env()
        log.info(f'status = {status.text}')

        if status.text == 'missing':
            log.error('Test suite did not run. Run `gh push` to attach test result to commit.')
            return 1
        elif status.text == 'stale':
            log.error('Commit is not based on origin/main. Rebase.')
            return 1
        elif status.text == 'failure':
            log.error('Tests failed')
            return 1

        assert status.text == 'success'

        return 0

    return run(main_inner())


if __name__ == '__main__':
    sys.exit(main())
