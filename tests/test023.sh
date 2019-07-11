#!/bin/sh

#
######################################################################
#
# This test catches a defect in de-backstopping using -pdt or -pdn
# when a sub-directory chain might have been populated but contains
# some objects that are not symbolic links to the source directory.
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir/1a/2a" "$srcdir/1a/2a" "$srcdir/1a/2b"
touch "$trgdir/1a/2a/3A" "$srcdir/1a/2a/3B" "$srcdir/1a/2b/3C"

mkdir -p "$trgdir/1b/2b" "$srcdir/1b/2a" "$srcdir/1b/2b"
touch "$trgdir/1b/2b/3A" "$srcdir/1b/2a/3B" "$srcdir/1b/2b/3C"

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

checkRun 'rm "$trgdir/1a/2a/3A" "$srcdir/1a/2a/3B" "$srcdir/1a/2b/3C"'
RC=$((RC + $?))

checkRun 'rmdir "$trgdir/1a/2a" "$srcdir/1a/2a" "$srcdir/1a/2b" "$trgdir/1a" "$srcdir/1a"'
RC=$((RC + $?))

checkRun 'rm "$trgdir/1b/2b/3A" "$srcdir/1b/2a/3B" "$srcdir/1b/2b/3C"'
RC=$((RC + $?))

checkRun 'rmdir "$trgdir/1b/2b" "$srcdir/1b/2a" "$srcdir/1b/2b" "$trgdir/1b" "$srcdir/1b"'
RC=$((RC + $?))

rm -f "$output"

. "${0%/*}/finalise.sh"
