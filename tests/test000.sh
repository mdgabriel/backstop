#!/bin/sh

#
######################################################################
#
# Test backstop -v and backstop --version
#
. "${0%/*}/environment.sh"

for flag in -v --version
do
    ${COMMAND} ${flag} 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout -r "$VERSION"
    RC=$((RC + $?))

    checkStderr ""
    RC=$((RC + $?))
done

. "${0%/*}/finalise.sh"
