#!/bin/sh

#
######################################################################
#
# Revision: 182
# % ./backstop -v
# backstop release 0.12.173
#
# This test catches a bug in which the target directory is removed at
# the end of a -pd operation.
#
. "${0%/*}/environment.sh"

_trgdir_="${trgdir##*/}"
trg="somepath1.$$/$_trgdir_"
src="somepath2.$$/$_trgdir_"

mkdir -p "$trg" "$src"

${COMMAND} -pd "$trg" "$src" 1>"$stdout" 2>"$stderr"
CNT=$((CNT + 1))

for d in "somepath1.$$" "somepath2.$$"
do
    checkRun 'rmdir "$d/$_trgdir_"'
    RC=$((RC + $?))

    checkRun 'rmdir "$d"'
    RC=$((RC + $?))
done

. "${0%/*}/finalise.sh"
