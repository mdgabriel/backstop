#!/bin/sh

#
######################################################################
#
# Test backstop -l and backstop --license
#
. "${0%/*}/environment.sh"

for flag in -l --license
do
    ${COMMAND} ${flag} 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout -r "License terms: GNU GPL version 3 or any later version"
    RC=$((RC + $?))

    checkStderr ""
    RC=$((RC + $?))
done

. "${0%/*}/finalise.sh"
