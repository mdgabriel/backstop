#!/bin/sh

#
######################################################################
#
# Test to verify that a -dp operation does not remove empty target
# sub-directories that are not under the source directory.
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir/1" "$trgdir/2/3" "$srcdir/A" "$srcdir/B/C"

${COMMAND} -p "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
${COMMAND} -pd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
CNT=$((CNT + 2))

for d in "$trgdir/2/3"  "$trgdir/2" "$trgdir/1" \
         "$srcdir/B/C"  "$srcdir/B" "$srcdir/A"
do
    checkRun 'rmdir "$d"'
    RC=$((RC + $?))
done

. "${0%/*}/finalise.sh"
