#!/bin/sh

#
######################################################################
#
# Emptied directory parallel to not empty directory
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir/1/A" "$srcdir/1/A" "$srcdir/1/2"
touch "$trgdir/1/A/B" "$srcdir/1/A/C" "$srcdir/1/2/3"

${COMMAND} -p "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
${COMMAND} -npd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
${COMMAND} -tpd "$trgdir" "$srcdir" 1>"$output" 2>"$stderr"
CNT=$((CNT + 3))

if [ "$(cat "$stdout")" != "$(cat "$output")" ]
then 1>&2 echo "${0##*/}: \"$(cat "$stdout")\" (-npd) != \"$(cat "$output")\" (-tpd)"
     RC=$((RC + 1))
fi

checkRun 'rm "$trgdir/1/A/B" "$srcdir/1/A/C" "$srcdir/1/2/3"'
RC=$((RC + $?))

checkRun 'rmdir "$trgdir/1/A" "$srcdir/1/A" "$srcdir/1/2" "$trgdir/1" "$srcdir/1"'
RC=$((RC + $?))

rm -f "$output"

. "${0%/*}/finalise.sh"
