#!/bin/sh

#
######################################################################
#
# Test backstop with operands that are not all directories
#
. "${0%/*}/environment.sh"

mkdir "$trgdir" "$srcdir"

for ds in "dir1 dir2" "dir1 $srcdir" "$trgdir dir2" \
          "$trgdir $srcdir dir1" "$trgdir $srcdir dir1 dir2"
do
    ${COMMAND} $ds 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 252 $EC
    RC=$((RC + $?))

    checkStdout ""
    RC=$((RC + $?))

    checkStderr -r "${COMMAND##*/}: not (directories|a directory): (.*/dir1 .*/dir2|.*/dir1|.*/dir2)"
    RC=$((RC + $?))

    checkStderr -r "$VERSION"
    RC=$((RC + $?))

    checkStderr -r "$HELP"
    RC=$((RC + $?))
done

. "${0%/*}/finalise.sh"
