#!/bin/sh

#
######################################################################
#
# Revision: 182
# % ./backstop -v
# backstop release 0.12.184
#
# This test catches a bug in which the -npd option adds an incorrect
# rmdir command to its output while the -tpd output is correct.
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir/1" "$srcdir/1"
touch "$trgdir/1/2"

${COMMAND} -npd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
${COMMAND} -tpd "$trgdir" "$srcdir" 1>"$output" 2>"$stderr"
CNT=$((CNT + 2))

if [ "$(cat "$stdout")" != "$(cat "$output")" ]
then 1>&2 echo "${0##*/}: \"$(cat "$stdout")\" (-npd) != \"$(cat "$output")\" (-tpd)"
     RC=$((RC + 1))
fi

checkRun 'rm "$trgdir/1/2"'
RC=$((RC + $?))

checkRun 'rmdir "$trgdir/1" "$srcdir/1"'
RC=$((RC + $?))

rm -f "$output"

. "${0%/*}/finalise.sh"
