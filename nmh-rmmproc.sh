#! /bin/sh
#
# Use this script in .mh_profile as rmmproc to move deleted messages
# to +trash.

fromfolder="`folder -fast`"

case "$fromfolder" in
inbox-spam|inbox-junk|inbox-git|trash)
	rm -f $*
	;;
*)	refile -normmproc -src +$fromfolder +trash $*
	;;
esac
