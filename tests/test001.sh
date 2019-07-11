#!/bin/sh

#
######################################################################
#
# Test backstop -h and backstop --help
#
. "${0%/*}/environment.sh"

for flag in -h --help
do
    ${COMMAND} ${flag} 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout -r "$VERSION"
    RC=$((RC + $?))

    checkStdout -r "$HELP"
    RC=$((RC + $?))

    checkStdout -r "$SUMMARY"
    RC=$((RC + $?))

    checkStderr ""
    RC=$((RC + $?))
done

. "${0%/*}/finalise.sh"
