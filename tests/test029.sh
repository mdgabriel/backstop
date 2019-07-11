#!/bin/sh

#
######################################################################
#
# Check when there are multiple errors
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir" "$srcdir"

i=0; while [ $i -lt 256 ]; do touch "$srcdir/$i"; i=$((i+1)); done

${COMMAND} "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 0 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr ""
RC=$((RC + $?))

chmod a-w "$trgdir"

${COMMAND} -d "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 255 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr -r "${COMMAND##*/}:.*removeLink:.*${trgdir}/.*:.*permission denied.*"
RC=$((RC + $?))

chmod u+w "$trgdir"

${COMMAND} -d "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 0 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr ""
RC=$((RC + $?))

i=0; while [ $i -lt 256 ]; do checkRun 'rm "$srcdir/$i"'; i=$((i+1)); done
RC=$((RC + $?))

. "${0%/*}/finalise.sh"
