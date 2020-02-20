#!/usr/bin/env python3

from __future__ import annotations
from asyncio import gather, run
from asyncio.subprocess import create_subprocess_exec, PIPE, STDOUT
from dataclasses import dataclass
import logging
from pathlib import Path
import re
import sys
from typing import FrozenSet, List, Optional, Pattern, Text, Tuple


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

    assert proc.returncode in error_codes, cmd
    
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
class Hunk:
    start: int
    stop: int

    # "git diff -U0" has the following predictable pattern for hunk headers. It shows exactly where
    # a change starts and if any additional lines were changed after that, how many.
    _header_pattern: Pattern = re.compile(r'^@@.*\+(?P<start>\d+)(,(?P<delta>\d+))? @@.*$')

    @staticmethod
    async def from_path(path: Path, /) -> Tuple[Hunk]:
        """Return Tuple[Hunk] of with start/stop of each hunk changed since repo's master branch."""

        # For each hunk header in git diff, extract start/stop of changed lines, and create Hunk.
        hunks: List[Hunk] = []
        for line in await get_lines(f'git diff remotes/origin/master -U0 {path}'):
            if match := Hunk._header_pattern.match(line):
                start = int(match.groupdict()['start'])
                stop = start + int(match.groupdict()['delta'] or '0')
                hunks.append(Hunk(start, stop))
        hunks: Tuple[Hunk] = tuple(hunks)
        
        return hunks


@dataclass(frozen=True)
class FileDiff:
    text: Text

    @staticmethod
    async def from_path(path: Path, /) -> FileDiff:
        """Return File with diff-text suggested by ocp-indent."""

        # Update file contents by running ocp-indent on every hunk of code changed since master.
        text = path.read_text()
        hunks = await Hunk.from_path(path)
        for hunk in hunks:
            text = await get_text(
                f'opam exec -- ocp-indent --lines={hunk.start}-{hunk.stop}', stdin=text,
            )

        # Get diff of file contents as suggested by ocp-indent and original file. Error codes 0/1
        # indicate no/some difference respectively.
        text = await get_text(f'diff -u {path} -', stdin=text, error_codes=frozenset({0, 1}))

        return FileDiff(text=text)


@dataclass(frozen=True)
class CommitDiff:
    text: Text

    @staticmethod
    async def from_env() -> CommitDiff:
        """Return CommitDiff with diff text suggested by ocp-indent."""

        # Repo path is needed to construct absolute paths for changed OCaml files.
        repo_path = Path((await get_text('git rev-parse --show-toplevel')).strip())

        # Ensure master is checked out for comparison.
        lines = tuple(line.strip() for line in await get_lines('git branch -a'))
        assert 'remotes/origin/master' in lines, 'local checkout of origin/master is required'

        # Names of files changed since master.
        lines = await get_lines('git diff --name-only remotes/origin/master')

        # Absolute paths of all OCaml files changed since master.
        paths = tuple(
            path for path in [repo_path / Path(line) for line in lines]
            if path.suffix in {'.ml', '.mli'}
        )

        # OCaml files with diff text as suggested by ocp-indent. These are calculated in parallel.
        file_diffs = tuple(await gather(*[FileDiff.from_path(path) for path in paths]))

        # Combined diff text of all changed OCaml files.
        text = '\n\n'.join(file_diff.text for file_diff in file_diffs if file_diff.text)

        return CommitDiff(text=text)
    

def main() -> int:
    async def main_inner() -> int:
        commit_diff = await CommitDiff.from_env()

        if commit_diff.text:
            log.error(f'ocp-indent checker suggests the following changes:\n\n{commit_diff.text}')
            return 1

        return 0
        
    return run(main_inner())


if __name__ == '__main__':
    sys.exit(main())
