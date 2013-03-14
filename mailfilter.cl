
(in-package :user)

(defparameter *ruletest-debug* nil)

(defparameter *mailstatus-inbox-folder-order*
    '("x"
      "gmail"
      "spam"
      "junk"

      :newline
      (:header "*********** TODO inboxes ********************")
      "a"
      "q"
      "fin"
      "todo"
      "sa"
      "pbx"
      "adrian"
      "someday"
      "pend"

      :newline
      (:header "*********** Hobby inboxes********************")
      "music"
      "photo"
      "fun"
      "travel"

      :newline
      (:header "*********** Mailing list inboxes ************")
      "emacs"
      "git"
      "blink"
      "gnus"
      "cygwin"
      "mh"
      "nmh"
      "ps"
      ))

(defvar *whitelist*
    '(("suzanne")))

(defvar *me* '("layer" "Layer"))

(defmacro ruletest (number folder form)
  (when (not (numberp number))
    (error "ruletest: `number' should be a number: ~s." number))
  `(progn
     (when (not (stringp ,folder))
       (error "ruletest: `folder' should be a string: ~s." ,folder))
     (if* ,form
	then (when *ruletest-debug*
	       (format t "  rule ~A: returning ~A~%" ,number ,folder))
	     (return ,folder)
	else (when *ruletest-debug*
	       (format t "  rule ~A: no match~%" ,number)))))

(defclassification (minfo)
  (let* ((headers (msginfo-headers minfo))
	 (subject (msginfo-subject minfo))
	 (bhid (or (msginfo-bhid minfo)
		   (extract-bhid-from-subject subject)))
	 (spam-flag (get-header "X-Spam-Flag" headers :null-string t)))
    
    (add-header "X-Touched-By-Incfilter" "v1.0 lisp")
    ;; This is for an as yet unused way of sorting my inbox.  Not sure
    ;; where it will go or even if it will work...
    (when (not (get-header "X-Priority" headers))
      (add-header "X-DKL-Priority" "50"))

    (ruletest 40 "+inbox-x" (to-one-of
			     '("pers-wcacd-2604006302@craigslist.org"
			       "cl94610")))

    (ruletest 50  "+inbox-spam" (string= spam-flag "YES"))
    
    (ruletest 60 "+inbox-gmail"
      (or (to-one-of "kevin.layer.*@gmail.com")
	  (to-one-of "gmail@known.net")
	  (to-one-of "layer-phone@franz.com")))
    
    (let ((dest-folder (message-in-conversation-p subject bhid)))
      #+ignore
      (format t "~d: conversation ~a ~a~%   => ~s~%"
	      msg-number
	      subject bhid
	      dest-folder)
      (when dest-folder
	(ruletest 70 dest-folder dest-folder)))

    (ruletest 100 "+inbox" (from-one-of *whitelist*))
    
    (ruletest 110 "+inbox-ps" (to-one-of "parenscript-devel@common-lisp.net"))
    (ruletest 120 "+inbox-blink" (to-one-of "blink@lists.ag-projects.com"))
    (ruletest 130 "+inbox-emacs" (to-one-of "emacs-devel@gnu.org"))
    (ruletest 140 "+inbox-nmh" (to-one-of "nmh.*@nongnu.org"))
    (ruletest 150 "+inbox-samba" (to-one-of "samba@lists.samba.org"))
    (ruletest 160 "+inbox-gnus" (to-one-of "ding@gnus.org"))
    (ruletest 170 "+inbox-cygwin"
      (or (to-one-of "cygwin@sourceware.org")
	  (to-one-of "cygwin@sources.redhat.com")
	  (to-one-of "cygwin@cygwin.com")))
    (ruletest 180 "+inbox-mp3" (from-one-of "maillist@allofmp3.com"))
    (ruletest 190 "+inbox-junk"
      (and (to-one-of "git@vger.kernel.org")
	   (subject-match "\\[[^[]*PATCH")))
    (ruletest 200 "+inbox-git" (to-one-of "git@vger.kernel.org"))
    
    "+inbox"))

;;; For Gnu Emacs.
;;; Local Variables: ***
;;; eval: (put 'ruletest 'fi:common-lisp-indent-hook 2) ***
;;; eval: (put 'defclassification 'fi:common-lisp-indent-hook 1) ***
;;; End: ***
