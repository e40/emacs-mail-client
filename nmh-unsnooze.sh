#! /usr/bin/env bash

# Uncomment to save actions of this script to this file
logfile=$HOME/.unsnooze.log

# -E allows -e to work with 'trap ... ERR'
set -ueE -o pipefail

srcfolder="$(mhparam Snoozed-Folder)"
dateheader="$(mhparam Snoozed-Header-Date)"
folderheader="$(mhparam Snoozed-Header-Folder)"
unseen="$(mhparam Unseen-Sequence)"

prog="$(basename "$0")"

function errordie {
    [ "${*-}" ] && echo "$prog: $*" 1>&2
    exit 1
}

[ "$srcfolder" ]    || errordie Snoozed-Folder from MH profile is null
[ "$dateheader" ]   || errordie Snoozed-Header-Date from MH profile is null
[ "$folderheader" ] || errordie Snoozed-Header-Folder from MH profile is null
[ "$unseen" ]       || errordie Unseen-Sequence from MH profile is null

function usage {
    [ "${*-}" ] && echo "$prog: $*" 1>&2
    cat 1>&2 <<EOF
Usage: $prog

Run from cron, check for messages which should be unsnoozed.
The messages are in the folder ${srcfolder} and have a header
${dateheader} which has a date and time of the format:
YYYY-MM-DD,HH:MM.  When the current date and time is past that
date and time, move the message into the folder indicated by te
${folderheader} header.  Both headers are removed from the message,
before moving it to the final destination and the message is as
marked as new by putting it into the "$unseen" sequence.
EOF
    exit 1
}

function d {
    if [ "$debug" ]; then
	echo "would: $*"
    else
        "$@"
    fi
}

###############################################################################

report=
debug=
verbose=

while [ $# -gt 0 ]; do
    case $1 in
	-d|--debug)   debug="$1"; verbose="$1" ;;
	-v|--verbose) verbose="$1" ;;
        -r|--report)  report="$1" ;;
        *)            usage unknown arguments: $* ;;
    esac
    shift
done

###############################################################################

# ensure that only one copy of this script is running at any given time
lockfile="/tmp/${prog}.lock"
lockfile -r 0 $lockfile || errordie $prog is already running

tempfile="/tmp/${prog}temp$$"
rm -f $tempfile

function exit_cleanup {
    /bin/rm -f $lockfile $tempfile
}
function err_report {
    echo "Error on line $(caller)" 1>&2
}
trap err_report   ERR
trap exit_cleanup EXIT

now=$(date +%s)

function logit {
    if [ "$verbose" ]; then
	echo $*
	return
    elif [ ! "${logfile-}" ]; then
	return
    fi
    echo $* >> "${logfile}"
}

# In the case of -r, this is used:
cat > $tempfile <<EOF
%2(msg) \
%{X-snooze-date} \
%<(mymbox{from})%<{to}To:%14(decode(friendly{to}))%>%> \
%(decode{subject})
EOF

{

    if ! scan $srcfolder -format "%(msg)" > $tempfile 2>/dev/null; then
	logit $(date) no messages in $srcfolder
	exit 0
    fi

    if [ "$report" ]; then
	scan $srcfolder -form $tempfile
	exit 0
    fi

    logit $(date)

    for msg in $(cat $tempfile); do
	path="$(mhpath $srcfolder $msg)"
	[ -f "$path" ] || errodie $path does not exist
	if temp=$(grep -E "^${dateheader}: " "$path"); then
	    if [[ $temp =~ .*:\ (....-..-..),(..:..)$ ]]; then
		mdate=${BASH_REMATCH[1]}
		mtime=${BASH_REMATCH[2]}
	    else
		errordie could not parse ${dateheader}: $temp
	    fi
	else
	    errordie could not find $dateheader in $path
	fi
	if temp=$(grep -E "^${folderheader}: " "$path"); then
	    if [[ $temp =~ .*:\ (.*)$ ]]; then
		destfolder=${BASH_REMATCH[1]}
	    else
		errordie could not parse ${folderheader}: $temp
	    fi
	else
	    errordie could not find $folderheader in $path
	fi

	target=$(date --date="$mdate $mtime" +%s)

	if (( now > target )); then
	    :
	else
	    continue
	fi

	logit ${dateheader}: $mdate $mtime
	logit ${srcfolder}: $(scan $srcfolder $msg)

	# remove the header
	d anno -nodate -component "$dateheader"   -delete
	d anno -nodate -component "$folderheader" -delete

	# make it new again
	d mark $srcfolder $msg -sequence $unseen -add 

	# refile it, preserving sequences
	d refile -retainsequences $msg -src $srcfolder $destfolder
    done

    logit ""

    exit 0
}
