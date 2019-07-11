#!/bin/sh

#
######################################################################
#
# Revision: 202
# % ./backstop -v
# backstop release 0.12.190.202
#
# This test catches a bug in which the -npd option is missing two
# rmdir commands in its output while the -tpd output is correct.
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir" "$srcdir/1/2"
touch "$srcdir/1/2/3"

${COMMAND} -p "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
${COMMAND} -npd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
${COMMAND} -tpd "$trgdir" "$srcdir" 1>"$output" 2>"$stderr"
CNT=$((CNT + 3))

if [ "$(cat "$stdout")" != "$(cat "$output")" ]
then 1>&2 echo "${0##*/}: \"$(cat "$stdout")\" (-npd) != \"$(cat "$output")\" (-tpd)"
     RC=$((RC + 1))
fi

checkRun 'rm "$srcdir/1/2/3"'
RC=$((RC + $?))

checkRun 'rmdir "$srcdir/1/2" "$srcdir/1"'
RC=$((RC + $?))

rm -f "$output"

. "${0%/*}/finalise.sh"
