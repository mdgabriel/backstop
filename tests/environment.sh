#!/bin/sh

${VERBOSE:-false} && 1>&2 echo "${0##*/}"

PATH=/bin:/usr/bin:$PATH

#
######################################################################
#
# Standard variables and functions
#
stdout="/tmp/${0##*/}.stdout.$$"
stderr="/tmp/${0##*/}.stderr.$$"
output="/tmp/${0##*/}.output.$$"
chgdir="/tmp/${0##*/}.chgdir.$$"
trgdir=$(realpath "trgdir.$$")
srcdir=$(realpath "srcdir.$$")

checkIO() {
    # $1 = string that is either STDOUT, STDERR, or an OUTPUT MESSAGE
    # $2 = -r followed by regex or string literal to check against $3
    # $3 = string to check against
    local regex="false"
    local msg="$1"; shift

    if [ "$1" = "-r" ]
    then regex=true; shift
    fi

    if [ X"$1" = X"$2" ]; then return 0; fi

    if [ X"$1" = X ]
    then if [ X"$2" = X ]
         then return 0
         else 1>&2 echo "${0##*/}: $msg not null: was \"$2\""
              return 1
         fi
    fi

    if "$regex"
    then if
             echo "$2" | egrep "$1" > /dev/null
         then
             return 0
         else
             1>&2 echo "${0##*/}: $msg incorrect: was \"$2\": should be \"$1\""
             return 1
         fi
    else 1>&2 echo "${0##*/}: $msg incorrect: was \"$2\": should be \"$1\""
         return 1
    fi
}

checkStdout() {
    # $1 or $2 = regex to check actual STDOUT

    checkIO STDOUT "$@" "$(cat $stdout)"
}

checkStderr() {
    # $1 or $2 = regex to check actual STDERR

    checkIO STDERR "$@" "$(cat $stderr)"
}

checkRC() {
    # $1 = what Return Code (RC) should be
    # $2 = actual RC

    if [ "$1" -eq "$2" ]
    then return 0
    else 1>&2 echo "${0##*/}: return code incorrect: was \"$2\": should be \"$1\""
         return 1
    fi
}

checkRun() {
    # $1 = String to be evaluated

    local checkRunResults=""

    if
        checkRunResults="$(2>&1 eval "$1")"
    then
        return 0
    else
        1>&2 echo "${0##*/}: \"$1\" incorrect: $checkRunResults"
        return 1
    fi
}

followSymlink() {
    # Read the symlink
    # $1 = Target symlink

    ls -lL "$1" > /dev/null

    return $?
}

checkSymlink() {
    # $1 = Target symlink
    # $2 = Source object

    followSymlink "$1" || return 1

    local ptr="$(ls -l "$1" | sed -e 's/^.* -> \(.*\)/\1/')"

    if
        [ ! -h "$1" ]
    then
        1>&2 echo "${0##*/}: not a symlink: $1"
        return 1
    fi

    if
        ! (echo "$ptr" | egrep '^/' >/dev/null)
    then
        ptr="${1%/*}/$ptr"
    fi
    
    if
        [ ! -e "${1%/*}/$prt" ]
    then
        1>&2 echo "${0##*/}: broken: $1 -> $prt"
        return 1
    fi

    if
        [ "$(canonicalise "$2")" != "$(canonicalise "$ptr")" ]
    then
        1>&2 echo "${0##*/}: broken: $1 -> $prt != $2"
        return 1
    fi

    return 0
}

checkDir() {
    # $1 = Target dir
    # $2 = Source dir
 
    for d in "$1" "$2"
    do
        if
            [ ! -d "$d" ] && [ ! -h "$d" ] 
        then
            1>&2 echo "${0##*/}: not a dir: $d"
            return 1
        fi
    done

    return 0
}

checkIfEmpty() {
    # $1 = Empty dir

    if
        [ ! -d "$1" ] && [ ! -h "$1" ] 
    then
        1>&2 echo "${0##*/}: not a directory: $1"
        return 1
    fi

    if
        [ "$(find "$1" | wc -l)" -ne 1 ]
    then
        1>&2 echo "${0##*/}: not empty: $1"
        return 1
    fi

    return 0
}

canonicalise() {
    # $1 = pathname to echo in canonical form

    echo $(realpath "$1")
}

report() {
    echo "Symlinks:  *$1; Dirs:  *$2; Calls:  *$3; Depth:  *$4; Breadth:  *$5; Errs:  *$6"
}

#
######################################################################
#
# Variables used potentially by each test
#
COMMAND="${backstop}"

for x
do
    # Take the first one found
    if [ -x "${x}" ]
    then
        COMMAND="${x}"
        break
    fi
done

VERSION="${COMMAND##*/} .*release [0-9][0-9]*\\.[0-9][0-9]*\\.[0-9][0-9]*"

HELP="usage: ${COMMAND##*/} {-{h\|l\|m\|v\|y}\|\[-nprt\] \[-a\|-d\] trgdir srcdir \[srcdir\\.\\.\\.\]}"

SUMMARY="\
  -a  --absolute     Make absolute symbolic links|\
  -d  --de-backstop  De-backstop target given sources|\
  -h  --help         Help synopsis|\
  -l  --license      License terms|\
  -m  --manual       Manual page|\
  -n  --no-action    No-action trace|\
  -p  --populate     Populate by making sub-directories|\
  -r  --report       Report final statistics|\
  -t  --trace        Trace program execution|\
  -v  --version      Version of backstop|\
  -y  --summary      Manual summary\
"

#
######################################################################
#
# Initialise
#
unset PAGER

TESTNOLIMIT=8
CNT=0
RC=0
