#! /bin/bash
#
# Clean up various folders, making sure they don't get too big.

if test -f cleanup.lock; then
    echo cleanup.lock exists.  Exiting...
    exit 0
fi

trap "folder +inbox; rm -f cleanup.lock" 0 1 2 3 14 15
date > cleanup.lock

nmessages()
{
    output=$(folder $1)
    if echo $output | grep -q "has no messages"; then
	echo 0
    else
	echo `echo $output | sed 's/.* has \([0-9]*\) message.*/\1/'`
    fi
}

clean_folder()
{
    folder=$1
    max=$2

    n=`nmessages $folder`

    echo $folder has $n messages
    if [ "$n" -gt "$max" ]; then
	m=$(($n - $max))
	echo "removing first $m messages from $folder"
	rmm $folder first:$m
    fi
    echo ""
}

#            folder  max messages
clean_folder +spam   20000
clean_folder +trash  15000
clean_folder +outbox 10000

echo remove garbage...
find . -name '#*' -print | xargs rm -f
echo ""

echo running index.sh script...
./index.sh

exit 0
