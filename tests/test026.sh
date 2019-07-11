#!/bin/sh

#
######################################################################
#
# Check the dot (".") directory.
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir" "$srcdir/A"
ln -s "$srcdir/A" "$srcdir/B"
touch "$srcdir/C"
ln -s "$srcdir/C" "$srcdir/D"

(
    cd "$trgdir" >/dev/null

    [ ! -x ${COMMAND} ] && COMMAND=../${COMMAND}

    ${COMMAND} -r . ../"${srcdir##*/}" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout ""
    RC=$((RC + $?))

    checkStderr -r "Symlinks: *4; Dirs: *0; Calls:  *2; Depth: *0; Breadth:  *4; Errs:  *0"
    RC=$((RC + $?))

    rm -f *.tix

    ${COMMAND} -rd . ../"${srcdir##*/}" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout ""
    RC=$((RC + $?))

    checkStderr -r "Symlinks: *4; Dirs: *0; Calls:  *2; Depth: *0; Breadth:  *4; Errs:  *0"
    RC=$((RC + $?))

    rm -f *.tix
)

checkRun 'rm "$srcdir/B" "$srcdir/C" "$srcdir/D"'
RC=$((RC + $?))

checkRun 'rmdir "$srcdir/A"'
RC=$((RC + $?))

. "${0%/*}/finalise.sh"
