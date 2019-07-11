#!/bin/sh

#
######################################################################
#
# Check the permission and error conditions with subdirectories.
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir" "$srcdir/d/e"
touch "$srcdir/f"

${COMMAND} -p "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 0 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr ""
RC=$((RC + $?))

chmod a-w "$trgdir/d"

${COMMAND} -pd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 1 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr -r "${COMMAND##*/}:.*removeDirectory:.*$trgdir/d/e:.*permission denied"
RC=$((RC + $?))

chmod u+w "$trgdir/d"

${COMMAND} -pd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"

checkRun 'rm "$srcdir/f"'
RC=$((RC + $?))

checkRun 'rmdir "$srcdir/d/e"  "$srcdir/d"'
RC=$((RC + $?))

. "${0%/*}/finalise.sh"
