#!/bin/sh

#
######################################################################
#
# This test catches a defect when a target directory has a directory
# in the pathname that is in fact a symbolic link.  The target
# directory's pathname must be resolved in this case.  The problem
# arises with the target directory and not the source directories but
# a decision must be made as to whether or not to resolve the source
# directory or not when creating symbolic links. We choose to resolve
# symbolic links in source directory pathnames when correcting this
# defect.
#
. "${0%/*}/environment.sh"

ln -s "/tmp" "tmp"
ln -s "/var/tmp" "vartmp"

trgdir="tmp/${trgdir##*/}"
srcdir="vartmp/${srcdir##*/}"

mkdir -p "$trgdir" "$srcdir/X/A" "$srcdir/X/2"
touch "$srcdir/X/A/B" "$srcdir/X/2/3"

mkdir -p "$srcdir/Y/A" "$srcdir/Y/2"
touch "$srcdir/Y/A/B" "$srcdir/Y/2/3"

mkdir -p "$srcdir/Z/A" "$srcdir/Z/2"
touch "$srcdir/Z/A/B" "$srcdir/Z/2/3"

touch "$srcdir/1" "$srcdir/2" "$srcdir/3"

${COMMAND} -p "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
  checkSymlink "$trgdir/1" "$srcdir/1"
  RC=$((RC + $?))
  checkSymlink "$trgdir/2" "$srcdir/2"
  RC=$((RC + $?))
  checkSymlink "$trgdir/3" "$srcdir/3"
  RC=$((RC + $?))
  checkSymlink "$trgdir/X/2/3" "$srcdir/X/2/3"
  RC=$((RC + $?))
  checkSymlink "$trgdir/X/A/B" "$srcdir/X/A/B"
  RC=$((RC + $?))
  checkSymlink "$trgdir/Y/2/3" "$srcdir/Y/2/3"
  RC=$((RC + $?))
  checkSymlink "$trgdir/Y/A/B" "$srcdir/Y/A/B"
  RC=$((RC + $?))
  checkSymlink "$trgdir/Z/2/3" "$srcdir/Z/2/3"
  RC=$((RC + $?))
  checkSymlink "$trgdir/Z/A/B" "$srcdir/Z/A/B"
  RC=$((RC + $?))
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

checkRun 'rmdir "$srcdir" "$trgdir"'
RC=$((RC + $?))

checkRun 'rm "tmp" "vartmp"'
RC=$((RC + $?))

. "${0%/*}/finalise.sh"
