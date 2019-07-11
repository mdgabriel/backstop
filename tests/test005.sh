#!/bin/sh

#
######################################################################
#
# Test backstop with less than two operands
#
. "${0%/*}/environment.sh"

for op in "" junk
do
    ${COMMAND} ${op} 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 251 $EC
    RC=$((RC + $?))

    checkStdout ""
    RC=$((RC + $?))

    checkStderr -r "${COMMAND##*/}: less than two operands"
    RC=$((RC + $?))

    checkStderr -r "$VERSION"
    RC=$((RC + $?))

    checkStderr -r "$HELP"
    RC=$((RC + $?))
done

. "${0%/*}/finalise.sh"
