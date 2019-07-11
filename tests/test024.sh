#!/bin/sh

#
######################################################################
#
# This test catches a defect in de-backstopping using -pd, -pdt or
# -pdn.  A is a directory before B that is occupied and B has a
# directory.  After de-backstoppping, B remains in the target.
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir/A" "$srcdir/B/b"
touch "$trgdir/A/1"

${COMMAND} -p "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 0 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr ""
RC=$((RC + $?))

${COMMAND} -npd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 0 $EC
RC=$((RC + $?))

checkStderr ""
RC=$((RC + $?))

${COMMAND} -tpd "$trgdir" "$srcdir" 1>"$output" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 0 $EC
RC=$((RC + $?))

checkStderr ""
RC=$((RC + $?))

if [ "$(cat "$stdout")" != "$(cat "$output")" ]
then 1>&2 echo "${0##*/}: \"$(cat "$stdout")\""
     1>&2 echo " (-npd) != (-tpd) "
     1>&2 echo "\"$(cat "$output")\""
     RC=$((RC + 1))
fi

checkRun 'rm "$trgdir/A/1"'
RC=$((RC + $?))

checkRun 'rmdir "$trgdir/A" "$srcdir/B/b" "$srcdir/B"'
RC=$((RC + $?))

rm -f "$output"

. "${0%/*}/finalise.sh"
