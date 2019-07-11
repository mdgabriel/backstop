#!/bin/sh

#
######################################################################
#
# Test backstop with directories that are not all parallel directories
#
. "${0%/*}/environment.sh"

mkdir "$trgdir" "$trgdir/A" "$trgdir/A/B" "$srcdir" "$srcdir/C"

# Canonicalized or realpath(3) paths are checked by backstop, hence
# this test "$trgdir $srcdir/D/B" which is "$trgdir $trgdir/A/B"
ln -s "../${trgdir##*/}/A" "$srcdir/D"

for ds in "$trgdir $trgdir" "$trgdir $trgdir/A" "$trgdir/A $trgdir" \
          "$trgdir $srcdir $trgdir/A" "$trgdir $srcdir $srcdir/C" \
          "$trgdir $srcdir/D/B"
do
    ${COMMAND} $ds 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 253 $EC
    RC=$((RC + $?))

    checkStdout ""
    RC=$((RC + $?))

    checkStderr -r "${COMMAND##*/}: not parallel directories:"
    RC=$((RC + $?))

    checkStderr -r "^  *\(/.*/[^/][^/]*,/.*/[^/][^/]*\)\$"
    RC=$((RC + $?))

    checkStderr -r "$VERSION"
    RC=$((RC + $?))

    checkStderr -r "$HELP"
    RC=$((RC + $?))
done

${COMMAND} "$trgdir" "$trgdir" 1>"$stdout" 2>"$stderr"
CNT=$((CNT + 1))
checkStderr -r "^  *\($trgdir,$trgdir\)$"
RC=$((RC + $?))

checkRun 'rm "$srcdir/D"'
RC=$((RC + $?))

for d in "$trgdir/A/B" "$trgdir/A" "$srcdir/C"
do
  [ -d "$d" ] && checkRun 'rmdir "$d"'
  RC=$((RC + $?))
done

. "${0%/*}/finalise.sh"
