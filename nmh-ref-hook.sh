#! /bin/bash
#
# Use the refile hook to make sure messages moved in the `important' sequence
# retain membership in that sequence in their destination folder.

prefix=$(mhpath +inbox | sed 's,/inbox$,,')

to=$(echo $2 | sed -e "s|^$prefix/||")

case $to in
    *trash*) ;;
    *)  from=$(echo $1 | sed -e "s|^$prefix/||")

	fromf=$(echo $from | sed -e 's,\(.*\)/\(.*\),\1,')
	tof=$(echo $to | sed -e 's,\(.*\)/\(.*\),\1,')

	# In the case of a repack, sequences are handled properly, so exit
	if [ "$fromf" = "$tof" ]; then
	    exit 0
	fi

	fromm=$(echo $from | sed -e 's,\(.*\)/\(.*\),\2,')
	tom=$(echo $to | sed -e 's,\(.*\)/\(.*\),\2,')

	if pick +$fromf important -list 2>&1 | egrep -q "^${fromm}\$"; then
	    mark +$tof $tom -add -sequence important
	    # Need to set the current folder back to $fromf!
	    folder +$fromf > /dev/null
	fi
	;;
esac
