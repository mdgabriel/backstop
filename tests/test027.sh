#!/bin/sh

#
######################################################################
#
# Check the permission and error conditions.
#
. "${0%/*}/environment.sh"

mkdir -p "$trgdir" "$srcdir/d"
touch "$srcdir/f"
chmod 0 "$trgdir"

${COMMAND} -rp "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 3 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr -r "\
${COMMAND##*/}:.*getDirectoryContents:.*$trgdir:.*permission denied|\
${COMMAND##*/}:.*createDirectory:.*$trgdir/d:.*permission denied|\
${COMMAND##*/}:.*createSymbolicLink:.*\.\./$srcdir/f:.*permission denied|\
Symlinks:.*0; Dirs:.*0; Calls:.*2; Depth:.*0; Breadth:.*0; Errs:.*3\
"
RC=$((RC + $?))

${COMMAND} -rpd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 1))

checkRC 1 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr -r "\
${COMMAND##*/}:.*getDirectoryContents:.*$trgdir:.*permission denied|\
Symlinks:.*0; Dirs:.*0; Calls:.*2; Depth:.*0; Breadth:.*0; Errs:.*1\
"
RC=$((RC + $?))

chmod u=rwx "$trgdir"

${COMMAND} -rp "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
chmod a-w "$trgdir"
${COMMAND} -rpd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
EC=$?
CNT=$((CNT + 2))

checkRC 2 $EC
RC=$((RC + $?))

checkStdout ""
RC=$((RC + $?))

checkStderr -r "\
${COMMAND##*/}:.*removeLink:.*$trgdir/f:.*permission denied
${COMMAND##*/}:.*removeDirectory:.*$trgdir/d:.*permission denied
Symlinks.*:.*0;.*Dirs:.*0; Calls:.*3; Depth:.*1; Breadth:.*2; Errs:.*2
"
RC=$((RC + $?))

chmod a+w "$trgdir"
${COMMAND} -rpd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"

checkRun 'rm "$srcdir/f"'
RC=$((RC + $?))

checkRun 'rmdir "$srcdir/d"'
RC=$((RC + $?))

. "${0%/*}/finalise.sh"
