#!/bin/sh

#
######################################################################
#
# More complicated mixture of symlinks and populated directories
#
. "${0%/*}/environment.sh"

mkdir "$trgdir"

mkdir -p "$srcdir/X/A" "$srcdir/X/2"
touch "$srcdir/X/A/B" "$srcdir/X/2/3"

mkdir -p "$srcdir/Y/A" "$srcdir/Y/2"
touch "$srcdir/Y/A/B" "$srcdir/Y/2/3"

mkdir -p "$srcdir/Z/A" "$srcdir/Z/2"
touch "$srcdir/Z/A/B" "$srcdir/Z/2/3"

touch "$srcdir/1" "$srcdir/2" "$srcdir/3"

${COMMAND} -p "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
${COMMAND} -npd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
${COMMAND} -tpd "$trgdir" "$srcdir" 1>"$output" 2>"$stderr"
CNT=$((CNT + 3))

if [ "$(cat "$stdout")" != "$(cat "$output")" ]
then 1>&2 echo "${0##*/}: \"$(cat "$stdout")\" (-npd) != \"$(cat "$output")\" (-tpd)"
     RC=$((RC + 1))
fi

checkRun 'rm "$srcdir/X/A/B" "$srcdir/X/2/3" \
"$srcdir/Y/A/B" "$srcdir/Y/2/3" \
"$srcdir/Z/A/B" "$srcdir/Z/2/3" \
"$srcdir/1" "$srcdir/2" "$srcdir/3"'
RC=$((RC + $?))

checkRun 'rmdir "$srcdir/X/A" "$srcdir/X/2" \
"$srcdir/Y/A" "$srcdir/Y/2" \
"$srcdir/Z/A" "$srcdir/Z/2" \
"$srcdir/X" "$srcdir/Y" "$srcdir/Z"'
RC=$((RC + $?))

rm -f "$output"

. "${0%/*}/finalise.sh"
