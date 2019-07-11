#!/bin/sh

#
######################################################################
#
# Test backstop with source directories that contain objects
#
#    2. If an object of the same name as the name of the object under
#       the root of the source directory does not exists under the
#       root of the target directory, then a symbolic link, relative
#       or absolute as required, is created under the root of the
#       target directory to the object under the root of the source
#       directory.  If populating (-p), then a sub-directory
#       is created instead.
#
# Tests the backstop/de-backstop options pairs
#
#    ":-d"      "-a:-d"     "-p:-pd"     "-ap:-pd"     "-p:-d"   "-ap:-d"
#    "-r:-rd"   "-ra:-rd"   "-rp:-rpd"   "-rap:-rpd"   "-rp:-rd" "-rap:-rd"
#    "-n:-nd"   "-rn:-rnd"  "-an:-nd"    "-ran:-rnd"
#    "-t:-td"   "-rt:-rtd"  "-at:-td"    "-rat:-rtd"
#    "-np:-npd" "-anp:-npd" "-rnp:-rnpd" "-ranp:-rnpd" "-np:-nd" "-anp:-nd" "-rnp:-rnd" "-ranp:-rnd"
#    "-tp:-tpd" "-atp:-tpd" "-rtp:-rtpd" "-ratp:-rtpd" "-tp:-td" "-atp:-td" "-rtp:-rtd" "-ratp:-rtd"
#
. "${0%/*}/environment.sh"

mkdir "$trgdir" "$srcdir"
touch "$srcdir/A"
ln -s /etc/hosts "$srcdir/B"
ln -s /tmp "$srcdir/C"
mkdir "$srcdir/D"

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

    if [ "${flag%:*}" != "-p" ] && [ "${flag%:*}" != "-ap" ]
    then checkSymlink "$trgdir/D" "$srcdir/D"
    else checkDir "$trgdir/D" "$srcdir/D"
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
    fi
    checkIfEmpty "$trgdir"
    RC=$((RC + $?))
done

for flag in "-r:-rd" "-ra:-rd" "-rp:-rpd" "-rap:-rpd" "-rp:-rd" "-rap:-rd"
do
    ${COMMAND} ${flag%:*} "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout ""
    RC=$((RC + $?))

    checkStderr -r "$(report "(4|3)" "(0|1)" "(2|3)" "(0|1)" "4" "0")"
    RC=$((RC + $?))

    ${COMMAND} "${flag#*:}" "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout ""
    RC=$((RC + $?))

    checkStderr -r "$(report "(4|3)" "(0|1)" "(2|3)" "(0|1)" "4" "0")"
    RC=$((RC + $?))

    if
        [ "$flag" = "-rp:-rd" ] || [ "$flag" = "-rap:-rd" ]
    then
        rmdir "$trgdir/D"
    fi
done

dash_n_stdout="ln -s \"../${srcdir##*/}/A\" \"$trgdir/A\"
ln -s \"../${srcdir##*/}/B\" \"$trgdir/B\"
ln -s \"../${srcdir##*/}/C\" \"$trgdir/C\"
ln -s \"../${srcdir##*/}/D\" \"$trgdir/D\""

dash_an_stdout="ln -s \"$srcdir/A\" \"$trgdir/A\"
ln -s \"$srcdir/B\" \"$trgdir/B\"
ln -s \"$srcdir/C\" \"$trgdir/C\"
ln -s \"$srcdir/D\" \"$trgdir/D\""

dash__d_stdout="rm -f \"$trgdir/A\"
rm -f \"$trgdir/B\"
rm -f \"$trgdir/C\"
rm -f \"$trgdir/D\""

for flag in "-n:-nd" "-rn:-rnd"  "-an:-nd" "-ran:-rnd"
do
    ${COMMAND} ${flag%:*} "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    if [ "${flag%:*}" = "-n" ] || [ "${flag%:*}" = "-rn" ]
    then checkStdout "$dash_n_stdout"
    else checkStdout "$dash_an_stdout"
    fi
    RC=$((RC + $?))

    if [ "${flag%:*}" = "-rn" ] || [ "${flag%:*}" = "-ran" ]
    then checkStderr -r "$(report "(4|3)" "(0|1)" "(2|3)" "(0|1)" "4" "0")"
    else checkStderr ""
    fi
    RC=$((RC + $?))

    ${COMMAND} "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
        ${COMMAND} "${flag#*:}" "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
        EC=$?
        CNT=$((CNT + 1))

        checkRC 0 $EC
        RC=$((RC + $?))

        checkStdout "$dash__d_stdout"
        RC=$((RC + $?))

        if [ "${flag#*:}" = "-rnd" ]
        then checkStderr -r "$(report "(4|3)" "(0|1)" "(2|3)" "(0|1)" "4" "0")"
        else checkStderr ""
        fi
        RC=$((RC + $?))
    ${COMMAND} -d "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"

    checkIfEmpty "$trgdir"
    RC=$((RC + $?))
done

for flag in "-t:-td" "-rt:-rtd"  "-at:-td" "-rat:-rtd"
do
    ${COMMAND} ${flag%:*} "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    if [ "${flag%:*}" = "-t" ] || [ "${flag%:*}" = "-rt" ]
    then checkStdout "$dash_n_stdout"
    else checkStdout "$dash_an_stdout"
    fi
    RC=$((RC + $?))

    if [ "${flag%:*}" = "-rt" ] || [ "${flag%:*}" = "-rat" ]
    then checkStderr -r "$(report "(4|3)" "(0|1)" "(2|3)" "(0|1)" "4" "0")"
    else checkStderr ""
    fi
    RC=$((RC + $?))

    ${COMMAND} "${flag#*:}" "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout "$dash__d_stdout"
    RC=$((RC + $?))

    if [ "${flag#*:}" = "-rtd" ]
    then checkStderr -r "$(report "(4|3)" "(0|1)" "(2|3)" "(0|1)" "4" "0")"
    else checkStderr ""
    fi
    RC=$((RC + $?))
done

dash_np_stdout="ln -s \"../${srcdir##*/}/A\" \"$trgdir/A\"
ln -s \"../${srcdir##*/}/B\" \"$trgdir/B\"
ln -s \"../${srcdir##*/}/C\" \"$trgdir/C\"
mkdir \"$trgdir/D\""

dash_anp_stdout="ln -s \"$srcdir/A\" \"$trgdir/A\"
ln -s \"$srcdir/B\" \"$trgdir/B\"
ln -s \"$srcdir/C\" \"$trgdir/C\"
mkdir \"$trgdir/D\""

dash__npd_stdout="rm -f \"$trgdir/A\"
rm -f \"$trgdir/B\"
rm -f \"$trgdir/C\"
rmdir \"$trgdir/D\""

dash__n_d_stdout="rm -f \"$trgdir/A\"
rm -f \"$trgdir/B\"
rm -f \"$trgdir/C\""

for flag in "-np:-npd" "-anp:-npd" "-rnp:-rnpd" "-ranp:-rnpd" \
            "-np:-nd" "-anp:-nd" "-rnp:-rnd" "-ranp:-rnd"
do
    ${COMMAND} ${flag%:*} "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    if [ "${flag%:*}" = "-np" ] || [ "${flag%:*}" = "-rnp" ]
    then checkStdout "$dash_np_stdout"
    else checkStdout "$dash_anp_stdout"
    fi
    RC=$((RC + $?))

    if [ "${flag%:*}" = "-rnp" ] || [ "${flag%:*}" = "-ranp" ]
    then checkStderr -r "$(report "(4|3)" "(0|1)" "(2|3)" "(0|1)" "4" "0")"
    else checkStderr ""
    fi
    RC=$((RC + $?))

    ${COMMAND} -p "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
        ${COMMAND} "${flag#*:}" "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
        EC=$?
        CNT=$((CNT + 1))

        checkRC 0 $EC
        RC=$((RC + $?))

        if [ "${flag#*:}" = "-nd" ] || [ "${flag#*:}" = "-rnd" ]
        then checkStdout "$dash__n_d_stdout"
        else checkStdout "$dash__npd_stdout"
        fi
        RC=$((RC + $?))

        if [ "${flag#*:}" = "-rnpd" ] || [ "${flag#*:}" = "-rnd" ]
        then checkStderr -r "$(report "(4|3)" "(0|1)" "(2|3)" "(0|1)" "4" "0")"
        else checkStderr ""
        fi
        RC=$((RC + $?))
    ${COMMAND} -pd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"

    checkIfEmpty "$trgdir"
    RC=$((RC + $?))
done

for flag in "-tp:-tpd" "-atp:-tpd" "-rtp:-rtpd" "-ratp:-rtpd" \
            "-tp:-td" "-atp:-td" "-rtp:-rtd" "-ratp:-rtd"
do
    ${COMMAND} ${flag%:*} "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    if [ "${flag%:*}" = "-tp" ] || [ "${flag%:*}" = "-rtp" ]
    then checkStdout "$dash_np_stdout"
    else checkStdout "$dash_anp_stdout"
    fi
    RC=$((RC + $?))

    if [ "${flag%:*}" = "-rtp" ] || [ "${flag%:*}" = "-ratp" ]
    then checkStderr -r "$(report "(4|3)" "(0|1)" "(2|3)" "(0|1)" "4" "0")"
    else checkStderr ""
    fi
    RC=$((RC + $?))

    ${COMMAND} "${flag#*:}" "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    if [ "${flag#*:}" = "-td" ] || [ "${flag#*:}" = "-rtd" ]
    then checkStdout "$dash__n_d_stdout"
    else checkStdout "$dash__npd_stdout"
    fi
    RC=$((RC + $?))

    if [ "${flag#*:}" = "-rtpd" ] || [ "${flag#*:}" = "-rtd" ]
    then checkStderr -r "$(report "(4|3)" "(0|1)" "(2|3)" "(0|1)" "4" "0")"
    else checkStderr ""
    fi
    RC=$((RC + $?))
    ${COMMAND} -pd "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"

    checkIfEmpty "$trgdir"
    RC=$((RC + $?))
done

checkRun 'rm "$srcdir/A" "$srcdir/B" "$srcdir/C"'
checkRun 'rmdir "$srcdir/D"'

. "${0%/*}/finalise.sh"
