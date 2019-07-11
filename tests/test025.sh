#!/bin/sh

#
######################################################################
#
# If "$srcdir" is moved aside, then "$trgdir/1/2" is left alone by
# -pd.  If "$srcdir" is moved in place, then "$trgdir/1/2" and
# "$trgdir/1" are removed by -pd.
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir" "$srcdir/1/2" "$srcdir/A/B/C"
touch "$srcdir/0" "$srcdir/A/B/C/c" "$srcdir/A/B/b" "$srcdir/A/a"

${COMMAND} -rp "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 0 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr -r "Symlinks: *4; Dirs: *5; Calls:  *7; Depth: *4; Breadth:  *3; Errs:  *0"
RC=$((RC + $?))

mv "$srcdir" "$srcdir".aside

${COMMAND} -rpd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 0 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr -r "Symlinks: *4; Dirs: *3; Calls:  *7; Depth: *4; Breadth:  *3; Errs:  *0"
RC=$((RC + $?))

cat >"$output" <<EOT
.*/1
.*/1/2
EOT

find "$trgdir" | while read x
do [ "$x" != "$trgdir" ] && echo "${x#*/}"
EC=$?
done | sort 1>"$stdout" 2>"$stderr"

checkRC 0 $EC
RC=$((RC + $?))

checkStdout -r "$(cat "$output")"
RC=$((RC + $?))

checkStderr ""
RC=$((RC + $?))

mv "$srcdir".aside "$srcdir"

${COMMAND} -rpd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 0 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr -r "Symlinks: *0; Dirs: *2; Calls:  *4; Depth: *2; Breadth:  *1; Errs:  *0"
RC=$((RC + $?))

checkRun 'rm "$srcdir/0" "$srcdir/A/B/C/c" "$srcdir/A/B/b" "$srcdir/A/a"'
RC=$((RC + $?))

checkRun 'rmdir "$srcdir/1/2" "$srcdir/1" "$srcdir/A/B/C" "$srcdir/A/B" "$srcdir/A/"'
RC=$((RC + $?))

rm -f "$output"

. "${0%/*}/finalise.sh"
