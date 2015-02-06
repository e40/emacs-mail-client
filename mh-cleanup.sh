#! /bin/bash
#
# Clean up various folders, making sure they don't get too big.

if test -f cleanup.lock; then
    echo cleanup.lock exists.  Exiting...
    exit 0
fi

trap "folder +inbox; rm -f cleanup.lock" 0 1 2 3 14 15
date > cleanup.lock

function nmessages() {
    local output=$(folder $1)
    if echo $output | grep -q "has no messages"; then
	echo 0
    else
	echo `echo $output | sed 's/.* has \([0-9]*\) message.*/\1/'`
    fi
}

function min {
    echo $(( $1>$2 ? $2 : $1 ))
}

maxrm=998

function clean_folder() {
    # remove messages from folder ($1) until their number is $2
    # and never remove more than 998 at a time (the max for nmh).
    local folder=$1
    local max=$2
    local n=`nmessages $folder`

    echo $folder has $n messages

    if [ "$n" -gt "$max" ]; then
	m=$(($n - $max))
	echo "removing first $m messages from $folder"

	if [ "$m" -gt "$maxrm" ]; then
	    while [ "$m" -gt 0 ]; do
		rmm $folder first:$(min $m $maxrm)
		m=$(( $m - $maxrm ))
	    done
	else
	    rmm $folder first:$m
	fi

	folder $folder -pack
    fi
    echo ""
}

#            folder  max messages
clean_folder +spam   9999
clean_folder +trash  9999
clean_folder +outbox 9999

echo remove garbage...
find . -name '#*' -print | xargs rm -f
echo ""

echo running index.sh script...
./index.sh

echo cleaning old conversations...
/usr/fi/mailstatus -cleanup

exit 0
