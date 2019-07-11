#!/bin/sh
#
######################################################################
#
# If a defect is found in backstop, one writes a test first to catch
# the defect, and then one repairs the defect.
#
# Example of combination of flags to test:
#     ""   -d      and    -r    -rd
#     -a   -d      and    -ra   -rd
#     -p   -pd     and    -rp   -rpd
#     -ap  -pd     and    -rap  -rpd
#
#     -n   -nd     and    -rn   -rnd
#     -t   -td     and    -rt   -rtd
#
#     -np  -npd    and    -rnp  -rnpd
#     -tp  -tpd    and    -rtp  -rtpd
#
#     -an  -nd     and    -ran  -rnd
#     -at  -td     and    -rat  -rtd
#
#     -anp -npd    and    -ranp -rnpd
#     -atp -tpd    and    -ratp -rtpd
#
######################################################################
#
# Run the tests
#
. "${0%/*}/environment.sh"

cp "${COMMAND}" "${COMMAND}.$$"

for t in "${0%/*}"/test[0-9][0-9][0-9].sh
do
  CNT1=$("$t" "${COMMAND}")   ; RC=$((RC + $?)); CNT=$((CNT + CNT1))

  testno="${t##*/}"; testno="${testno#test}"; testno="${testno%.sh}"
  if [ "$testno" -lt "$TESTNOLIMIT" ]
  then CNT2=$("$t" "${COMMAND}.$$"); RC=$((RC + $?)); CNT=$((CNT + CNT2))
  fi
done

rm -f "${COMMAND}.$$"

1>&2 echo "${0##*/}: RC  = $RC errors"
1>&2 echo "${0##*/}: CNT = $CNT backstop tests"

. "${0%/*}/finalise.sh"
