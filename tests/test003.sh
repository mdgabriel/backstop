#!/bin/sh

#
######################################################################
#
# Test backstop -m and backstop --manual
#
. "${0%/*}/environment.sh"

sed -n -e 's/^> //p' "${0%/*}/../src/Main.hs" >"$output"

for flag in -m --manual
do
    ${COMMAND} ${flag} 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout "$(cat "$output")"
    RC=$((RC + $?))

    checkStderr ""
    RC=$((RC + $?))
done

rm -f "$output"

. "${0%/*}/finalise.sh"
