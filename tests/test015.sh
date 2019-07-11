#!/bin/sh

#
######################################################################
#
# Relative paths outside of the PWD
#
# Test backstop with source directories that contain objects
#
#    5. If an object of the same name as the name of the object under
#       the root of the source directory does exists under the root of
#       the target directory and is a directory, and if the object of
#       the same name under the root of the source directory is also a
#       directory, then recursion occurs with the objects of the same
#       name under the roots of the target and source directories as
#       new root target and source directories, respectively.
#
# Tests the backstop/de-backstop options pairs
#
#    ":-d"      "-a:-d"     "-p:-pd"     "-ap:-pd"     "-p:-d"   "-ap:-d"
#
. "${0%/*}/environment.sh"

parent="${PWD##*/}"
_trgdir_="${trgdir##*/}"
_srcdir_="${srcdir##*/}"
# Relative paths outside of the PWD
trgdir=$(realpath "../$parent/$_trgdir_")
srcdir=$(realpath "../$parent/$_srcdir_")

mkdir "$trgdir" "$srcdir"
touch "$srcdir/A"
ln -s /etc/hosts "$srcdir/B"
ln -s /var/tmp "$srcdir/C"
mkdir "$srcdir/D"

touch            "$trgdir/E" && touch "$srcdir/E"
touch            "$trgdir/F" && mkdir "$srcdir/F"
ln -s /etc/hosts "$trgdir/G" && touch "$srcdir/G"
ln -s /etc/hosts "$trgdir/H" && mkdir "$srcdir/H"
ln -s /var/tmp   "$trgdir/I" && touch "$srcdir/I"
ln -s /var/tmp   "$trgdir/J" && mkdir "$srcdir/J"
ln -s /etc/hosts "$trgdir/K" && ln -s /etc/hosts "$srcdir/K"
ln -s /var/tmp   "$trgdir/L" && ln -s /var/tmp "$srcdir/L"

mkdir "$trgdir/M" && touch "$srcdir/M"
mkdir "$trgdir/N" && ln -s /etc/hosts "$srcdir/N"
mkdir "$trgdir/O" && ln -s /var/tmp "$srcdir/O"

touch "$trgdir/1"
ln -s /etc/hosts "$trgdir/2"
ln -s /var/tmp "$trgdir/3"
mkdir "$trgdir/4"

(\cd "$trgdir" >/dev/null && mkdir recurse && find * -depth | cpio -pdm recurse) 2>/dev/null
(\cd "$srcdir" >/dev/null && mkdir recurse && find * -depth | cpio -pdm recurse) 2>/dev/null

for flag in ":-d" "-a:-d" "-p:-pd" "-ap:-pd" "-p:-d" "-ap:-d"
do
    ${COMMAND} ${flag%:*} "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout ""
    RC=$((RC + $?))

    checkStderr ""
    RC=$((RC + $?))

    checkSymlink "$trgdir/A" "$srcdir/A"
    RC=$((RC + $?))

    checkSymlink "$trgdir/B" "$srcdir/B"
    RC=$((RC + $?))

    checkSymlink "$trgdir/C" "$srcdir/C"
    RC=$((RC + $?))

    checkSymlink "$trgdir/recurse/A" "$srcdir/recurse/A"
    RC=$((RC + $?))

    checkSymlink "$trgdir/recurse/B" "$srcdir/recurse/B"
    RC=$((RC + $?))

    checkSymlink "$trgdir/recurse/C" "$srcdir/recurse/C"
    RC=$((RC + $?))

    if [ "${flag%:*}" != "-p" ] && [ "${flag%:*}" != "-ap" ]
    then checkSymlink "$trgdir/D" "$srcdir/D"
         checkSymlink "$trgdir/recurse/D" "$srcdir/recurse/D"
    else checkDir "$trgdir/D" "$srcdir/D"
         checkDir "$trgdir/recurse/D" "$srcdir/recurse/D"
    fi
    RC=$((RC + $?))

    ${COMMAND} "${flag#*:}" "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout ""
    RC=$((RC + $?))

    checkStderr ""
    RC=$((RC + $?))

    if
        [ "$flag" = "-p:-d" ] || [ "$flag" = "-ap:-d" ]
    then
        checkRun 'rmdir "$trgdir/D"'
        RC=$((RC + $?))
        checkRun 'rmdir "$trgdir/recurse/D"'
        RC=$((RC + $?))
    fi
done

for d in "$srcdir/recurse" "$srcdir"
do
    (
	\cd "$d" >/dev/null
        checkRun 'rm [A-C]'
        checkRun 'rmdir D'
        checkRun 'rm [E,G,I,K,L]'
        checkRun 'rmdir [F,H,J]'
        checkRun 'rm [M-O]'
    )
done
RC=$((RC + $?))

for d in "$trgdir/recurse" "$trgdir"
do
    (
        \cd "$d" >/dev/null
        checkRun 'rm [1-3]'
        checkRun 'rmdir 4'
        checkRun 'rm [E-L]'
        checkRun 'rmdir [M-O]'
    )
done
RC=$((RC + $?))

checkRun 'rmdir "$trgdir/recurse/recurse" "$srcdir/recurse/recurse"'
RC=$((RC + $?))

checkRun 'rmdir "$trgdir/recurse" "$srcdir/recurse"'
RC=$((RC + $?))

. "${0%/*}/finalise.sh"
