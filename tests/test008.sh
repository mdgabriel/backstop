#!/bin/sh

#
######################################################################
#
# Test backstop with two empty, parallel directories
#
#    1. The object is either dot (.) or double-dot (..) whereupon no
#       action is taken.

. "${0%/*}/environment.sh"

mkdir "$trgdir" "$srcdir"

for flag in "" -d -n -nd -t -td -p -pd        \
            -nt -ntd -np -npd -pt -ptd        \
            -a -ad -an -and -at -atd -ap -apd \
            -ant -antd -anp -anpd -apt -aptd
do
    ${COMMAND} $flag "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout ""
    RC=$((RC + $?))

    checkStderr ""
    RC=$((RC + $?))
done

for flag in -r -rd -rn -rnd -rt -rtd -rp -rpd         \
            -rnt -rntd -rnp -rnpd -rpt -rptd          \
            -ra -rad -ran -rand -rat -ratd -rap -rapd \
            -rant -rantd -ranp -ranpd -rapt -raptd
do
    ${COMMAND} $flag "$trgdir" "$srcdir" 1>"$stdout" 2>"$stderr"
    EC=$?
    CNT=$((CNT + 1))

    checkRC 0 $EC
    RC=$((RC + $?))

    checkStdout ""
    RC=$((RC + $?))

    checkStderr -r "$(report 0 0 2 0 0 0)"
    RC=$((RC + $?))
done

. "${0%/*}/finalise.sh"
