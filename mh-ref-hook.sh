#! /bin/bash
#
# Use the refile hook to make sure messages moved in the `important' sequence
# retain membership in that sequence in their destination folder.

prefix=$(mhpath +)

to=${2#${prefix}/}

case $to in
    *trash*) ;;
    *)  from=${1#${prefix}/}

	fromf=${from%/*}
	tof=${to%/*}

	# In the case of a repack, sequences are handled properly, so exit
	if [ "$fromf" = "$tof" ]; then
	    exit 0
	fi

	fromm=${from##*/}
	tom=${to##*/}

	if pick +$fromf important -list 2>&1 | egrep -q "^${fromm}\$"; then
	    mark +$tof $tom -add -sequence important
	    # Need to set the current folder back to $fromf!
	    folder +$fromf > /dev/null
	fi
	;;
esac
