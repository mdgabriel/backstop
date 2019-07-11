#!/bin/sh

#
######################################################################
#
# Check when there are multiple sources
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir" "$srcdir/d" "${srcdir}1/d" "${srcdir}2/d"
touch "$srcdir/f" "${srcdir}1/f" "${srcdir}2/f"

${COMMAND} -p "$trgdir" "$srcdir" "${srcdir}1" "${srcdir}2" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 0 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr ""
RC=$((RC + $?))

${COMMAND} -pd "$trgdir" "$srcdir" "${srcdir}1" "${srcdir}2" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 0 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr ""
RC=$((RC + $?))

checkRun 'rm "$srcdir/f" "${srcdir}1/f" "${srcdir}2/f"'
RC=$((RC + $?))

checkRun 'rmdir "$srcdir/d" "${srcdir}1/d" "${srcdir}2/d" "${srcdir}1" "${srcdir}2"'
RC=$((RC + $?))

. "${0%/*}/finalise.sh"
