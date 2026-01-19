#!/usr/bin/env bash

set -e

# This is the absolute path to `<repo-root>/bootstrap/`.
BASE_PATH="$(realpath "$(dirname "$(realpath "${0}")")"/..)"

mapfile -t FILE_PATHS < <( \
    find "${BASE_PATH}" -type f \( \
        \( \
            -regex "^${BASE_PATH}/bin/.*\.mli?$" \
            -or -regex "^${BASE_PATH}/hocc/.*\.mli?$" \
            -or -regex "^${BASE_PATH}/src/.*\.mli?$" \
            -or -regex "^${BASE_PATH}/test/.*\.mli?$" \
        \) \
        -not \( \
            -regex "^${BASE_PATH}/test/hocc/.*$" \
            -or -regex "^${BASE_PATH}/bin/hocc/Parse.ml$" \
        \) \
    \) \
)

# How many file paths have yet to be checked with ocp-indent.
TOTAL="${#FILE_PATHS[@]}"
# How many file paths have yet to be checked with ocp-indent.
MAINING="${TOTAL}"
# How many file paths have been checked with ocp-indent, and failed.
FAILED=0
# How many file paths have been checked with ocp-indent, and succeeded.
SUCCEEDED=0

for FILE_PATH in "${FILE_PATHS[@]}"; do
    REMAINING=$(("${REMAINING}" - 1))

    # The format specifiers in status lines either right-pad with whitespace or truncate so that
    # line length matches the terminal width.
    if opam exec -- ocp-indent "${FILE_PATH}" | diff -u "${FILE_PATH}" -; then
        SUCCEEDED=$(("${SUCCEEDED}" + 1))

        # Print success status line with carriage-return. Then next line will overwrite this.
        printf "%-$(tput -T xterm-256color cols).$(tput -T xterm-256color cols)b\r" \
            "- $(( ("${FAILED}" + "${SUCCEEDED}") * 100 / "${TOTAL}"))% of ${TOTAL}: (\e[31m${FAILED}✗\e[0m, \e[32m${SUCCEEDED}✔\e[0m) ← \e[32m$(realpath --relative-to "${BASE_PATH}" "${FILE_PATH}")\e[0m"
    else
        FAILED=$(("${FAILED}" + 1))

        # Print failed status line with newline. Then next line will not overwrite this.
        printf "%-$(tput cols).$(tput cols)b\n" \
            "- $(( ("${FAILED}" + "${SUCCEEDED}") * 100 / "${TOTAL}"))% of ${TOTAL}: (\e[31m${FAILED}✗\e[0m, \e[32m${SUCCEEDED}✔\e[0m) ← \e[31m$(realpath --relative-to "${BASE_PATH}" "${FILE_PATH}")\e[0m"
    fi
done

printf "\n"

if [[ "${FAILED}" -ne 0 ]]; then
    exit 1
fi

