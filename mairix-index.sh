#! /bin/bash

mairix --purge 2>&1 | egrep -v "mtime failed for"
# http://www.mail-archive.com/mairix-users@lists.sourceforge.net/msg00100.html
