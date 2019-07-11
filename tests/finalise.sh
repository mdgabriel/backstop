#!/bin/sh
#
[ "${0##*/}" != "run-tests.sh" ] && checkRun 'rm -f "$stdout" "$stderr"'

for d in "$trgdir" "$srcdir" "$chgdir/$trgdir" "$chgdir/$srcdir" "$chgdir" 
do
    if [ -d "$d" ]
    then checkRun 'rmdir "$d"'
    else : Ok
    fi
    RC=$((RC + $?))
done

[ "${0##*/}" != "run-tests.sh" ] && echo "$CNT"

if [ "$RC" -gt 255 ]
then exit 255
else exit $RC
fi
