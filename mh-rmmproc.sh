#! /bin/sh
#
# Use this script in .mh_profile as rmmproc to move deleted messages
# to +trash.

fromfolder="`folder -fast`"

case "$fromfolder" in
    inbox-junk|inbox-git|trash)
	rm -f $*
	;;
    inbox-spam)
	refile -normmproc -src +$fromfolder +spam $*
	;;
    *)	refile -normmproc -src +$fromfolder +trash $*
	;;
esac
