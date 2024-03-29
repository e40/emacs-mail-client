
(defun dkl:mh-rmail (&optional arg)
  (interactive "P")
  (cond (arg (mh-rmail))
	(t (dkl:mh-inbox-summary))))

(defun dkl:mh-scan-inboxes ()
  (interactive)
  (mh-find-path)
  (let ((folder
	 (completing-read
	  "folder: "
	  (mapcar (function
		   (lambda (inbox)
		     (string-match "^inbox-\\(.*\\)$" inbox)
		     (list (substring inbox (match-beginning 1)
				      (match-end 1)))))
		  (directory-files mh-user-path nil "^inbox-.*"))
	  nil
	  t
	  nil)))
    (let ((maildrop (or (getenv "MAILDROP")
			(error "MAILDROP not defined in environment."))))
      (when (and (file-exists-p maildrop)
		 (> (or
		     ;; avoid file-size, because it reads the file!!
		     (nth 7 (file-attributes maildrop))
		     0)
		    0))
	(save-window-excursion
	  (save-excursion
	    (mh-inc-folder)))))
    (let ((folder (format "+inbox-%s" folder)))
      (mh-visit-folder folder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mh-e "move conversation" support
;;
;; dependent components:
;;  - mailfilter
;;  - modifications to ~/.mailfilter.cl
;;
;; TODO:
;;  - if I'm to use the message-id at all, incfilter will have to add
;;    message-id's to conversations.el
;;  - need to write a script to find out if conversations can be cleaned out
;;    of conversations.el...
;;  - if loading conversations.el is too slow in incfilter, consider using
;;    fasl read/write to make it faster

(defun dkl:mh-move-conversation (to-folder)
  "Refile the conversation in the current thread into TO-FOLDER."
  (interactive (list (intern (mh-prompt-for-refile-folder))))
  (when (not (string-match "^\\+inbox" (symbol-name to-folder)))
    (error "You can only move a conversation to an +inbox folder, not %s."
	   to-folder))
  (cond ((not (memq 'unthread mh-view-ops)) (error "Folder isn't threaded"))
        ((eobp) (error "No message at point"))
        (t
	 (let ((msg-list (dkl:mh-current-thread-to-msg-list)))
	   (dkl:mh-note-moved-conversation-thread mh-current-folder
						 (symbol-name to-folder)
						 msg-list)
	   (mh-thread-refile to-folder)))))

(defvar dkl:mh-mailfilter-directory
    (expand-file-name "~/.mailfilter.d/"))

(defvar dkl:mh-mailfilter-conversations-file
    (format "%sconversations.el" dkl:mh-mailfilter-directory))

(defvar dkl:mh-mailfilter-conversations-lock-file
    (format "%sconversations.el.lock" dkl:mh-mailfilter-directory))

(defun dkl:lock-file (file lock-file seconds)
  ;; make a hard link copy of FILE to LOCK-FILE, waiting SECONDS seconds
  ;; for the LOCK-FILE to not exist, then signal an error.
  (let ((n seconds))
    (while (and (file-exists-p lock-file)
		(> n 0))
      (message "waiting[%d] for %s..." n lock-file)
      (sit-for 1)
      (setq n (- n 1)))
    (when (file-exists-p lock-file)
      (error "Timed out waiting for %s" lock-file)))
  ;; This will error if someone grabbed the lock after we checked for it:
  (add-name-to-file file lock-file))

(defun dkl:mh-note-moved-conversation-thread (from-folder to-folder msg-list)
  ;; conversation data is: subject and bhid.  Signal an error if neither
  ;; are available. In the future, I'll add all Message-Id's.
  (let ((conversation-data
	 (dkl:mh-thread-conversation-data from-folder to-folder msg-list))
	(data-loss-danger nil)
	(ok nil))
    (unwind-protect
	(progn
	  (dkl:lock-file dkl:mh-mailfilter-conversations-file
			 dkl:mh-mailfilter-conversations-lock-file
			 5)
	  (when conversation-data
	    (when (not (file-directory-p dkl:mh-mailfilter-directory))
	      (make-directory dkl:mh-mailfilter-directory t))
	    (with-temp-buffer ()
	      (princ conversation-data (current-buffer))
	      (goto-char (point-max))
	      (append-to-file (point-min) (point-max)
			      dkl:mh-mailfilter-conversations-lock-file))
	    (setq data-loss-danger t)
	    (delete-file dkl:mh-mailfilter-conversations-file)
	    (rename-file dkl:mh-mailfilter-conversations-lock-file
			 dkl:mh-mailfilter-conversations-file)
	    (setq ok t)))
      (when (not ok)
	;; an error somewhere in the protected form.... be more careful
	;; when data-loss-danger is non-nil!
	(cond
	 (data-loss-danger
	  (message
	   "DANGER: conversations file is in jeopardy!  Check these:\n%s\n%s"
	   dkl:mh-mailfilter-conversations-file
	   dkl:mh-mailfilter-conversations-lock-file))
	 (t
	  ;; likely the append-to-file failed due to a full filesystem, so
	  ;; just remove the lock file and go home
	  (delete-file dkl:mh-mailfilter-conversations-lock-file)
	  (message "Conversation not saved!")))))))

(defun dkl:mh-current-thread-to-msg-list ()
  (let ((region (save-excursion (mh-thread-find-children)))
	(msg-list '()))
    (save-excursion
      (mh-iterate-on-messages-in-region ()
	  (car region)
	  (cadr region)
	(beginning-of-line)
	(when (or (looking-at mh-scan-deleted-msg-regexp)
		  (looking-at mh-scan-blocklisted-msg-regexp)
		  (looking-at mh-scan-allowlisted-msg-regexp)
		  (looking-at mh-scan-refiled-msg-regexp))
	  (error "Message has pending operations; undo before moving"))
	(push (mh-get-msg-num t) msg-list)))

    (nreverse msg-list)))

(defun dkl:mh-thread-conversation-data (from-folder to-folder msg-list)
  ;; From FOLDER and MSG-LIST, extract the conversation data and return it
  ;; to the caller.
  ;;
  ;; Some code cribbed from mh-thread-generate.
  (with-temp-buffer
    (apply
     #'call-process (expand-file-name mh-scan-prog mh-progs) nil '(t nil) nil
     "-width" "10000" "-format"
     "%(msg)\n%{subject}\n%{bh-id}\n%{message-id}\n"
     from-folder
     (mapcar #'(lambda (x) (format "%s" x)) msg-list))
    (goto-char (point-min))
    (let ((conversation-data '())
          (case-fold-search t)
	  (subjects '())
	  (bh-ids '())
	  (msgids '()))
      (block nil
        (while (not (eobp))
          (macrolet 
	   ((save-val (thing place)
		      `(progn
			 (when (and ,thing (= 0 (length ,thing)))
			   (setq ,thing nil))
			 (when ,thing 
			   (pushnew ,thing ,place :test 'equal)))))
	   (let* ((index-line
		   (prog1 (buffer-substring (point) (mh-line-end-position))
		     (forward-line)))
		  (index (string-to-number index-line))
		  (subject (prog1 (buffer-substring (point)
						    (mh-line-end-position))
			     (forward-line)))
		  (bh-id (prog1 (buffer-substring (point)
						  (mh-line-end-position))
			   (forward-line)))
		  (msgid (prog1 (buffer-substring (point)
						  (mh-line-end-position))
			   (forward-line))))
	     (unless (integerp index) (return)) ;Error message here

	     (when subject
	       ;; strip off the "Re: " prefix:
	       (when (string-match "^re: ?\\(.*\\)" subject)
		 (setq subject (match-string 1 subject))))
	     
	     (save-val subject subjects)
	     (save-val bh-id bh-ids)
	     (save-val msgid msgids)))))
      (format "(:folder %S\n :subjects %S\n :bh-ids %S\n :message-ids %S)\n"
	      to-folder subjects bh-ids msgids))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dkl:*mailstatus-program*
    (or
;;;;TESTING only:
     ;;(dkl:probe-file "~/src/mailfilter/mailstatus/mailstatus")
     (dkl:probe-file "/usr/fi/mailstatus")
     (dkl:probe-file "/usr/local/mailstatus")))

(defconst dkl:mh-inbox-summary-buffer "*Inboxes*")
(defconst dkl:mh-inbox-summary-scan-buffer "*Inboxes-scan*")
;;(defconst dkl:mh-inbox-summary-temp-buffer " *TEMP Inboxes*")

(defvar dkl:mh-inbox-summary-inc-status nil
  ;; Non-nil if an inc has already been done after see "new" in -goto
  )
(make-variable-buffer-local 'dkl:mh-inbox-summary-inc-status)

(defvar dkl::mh-inbox-summary-window-config nil)

(defvar dkl:mh-inbox-summary-buffer-extra-info-funcs nil)

;; NIL means currently visible, make invisible.
;; non-NIL means currently invisible, make visible and the value
;;   of the variable is the overlay to delete.
(make-variable-buffer-local 'dkl:mh-inbox-summary-snooze-toggle)

;; Used to hide the Snoozed messages section
(add-to-invisibility-spec 'dkl:mh-inbox-summary-hide-lines)

(defun dkl:mh-inbox-summary ()
  (interactive)
  (when (not (string= dkl:mh-inbox-summary-buffer (buffer-name)))
    ;; Don't clobber the window configuration that got us here if we do
    ;; 'g'!
    (setq dkl::mh-inbox-summary-window-config (current-window-configuration)))
  (setq dkl:mh-inbox-summary-inc-status nil)
  (switch-to-buffer (get-buffer-create dkl:mh-inbox-summary-buffer))
  (delete-other-windows (get-buffer-window dkl:mh-inbox-summary-buffer))
  (setq buffer-read-only nil)
  
  (let ((snoozed-hidden (when dkl:mh-inbox-summary-snooze-toggle t)))
    (let ((line-number (line-number-at-pos)))
      (when snoozed-hidden
	(delete-overlay dkl:mh-inbox-summary-snooze-toggle)
	(setq dkl:mh-inbox-summary-snooze-toggle nil))
      (erase-buffer)
      (message "Checking folder status...")
      (call-process dkl:*mailstatus-program*
		    nil			; infile
		    t			; current buffer
		    nil			; don't redisplay as output
		    "-l")

      (mh-snooze-summary)

      (goto-char (point-min))
      (forward-line (1- line-number)))
  
    (message "Checking folder status...done.")
    (dkl:mh-inbox-summary-mode)
  
    (when snoozed-hidden
      ;; MUST BE DONE AFTER dkl:mh-inbox-summary-mode because
      ;; that function clears local variables!
      (dkl:mh-inbox-summary-toggle-snoozes)))

  (when dkl:mh-inbox-summary-buffer-extra-info-funcs
    (let* ((nbuffers (length dkl:mh-inbox-summary-buffer-extra-info-funcs))
	   (lines (frame-height (selected-frame)))
	   (remain lines)
	   (default-height (/ lines nbuffers))
	   (splits (- nbuffers 1))
	   height
	   display-buffer)

      (dolist (item dkl:mh-inbox-summary-buffer-extra-info-funcs)
	(when (setq display-buffer (cdr (assoc :display-buffer item)))
	  (funcall display-buffer))
	
	(when (> splits 0)
	  (cond
	   ((setq height (cdr (assoc :height item)))
	    (setq height
	      (cond ((floatp height)
		      (when (> height .80) (error "too large: %s" height))
		      
		      (round (* height remain)))
		     ((or (symbolp height) (functionp height))
		      (min remain (funcall height)))
		     (t (error "can't handle: %s" height)))))
	   (t (setq height default-height)))

	  ;;(y-or-n-p (format "about to split %d " height))
	  (split-window-vertically height)
	  (setq splits (- splits 1))
	  (setq remain (- remain height)))
	
	(other-window 1))))
  )

(defun dkl:mh-inbox-summary-scan ()
  (interactive)
  (setq dkl:mh-inbox-summary-inc-status nil)
  (switch-to-buffer (get-buffer-create dkl:mh-inbox-summary-scan-buffer))
  (delete-other-windows (get-buffer-window dkl:mh-inbox-summary-scan-buffer))
  (setq buffer-read-only nil)
  (let ((line-number (line-number-at-pos)))
    (erase-buffer)
    (message "Checking folder status...")
    (call-process dkl:*mailstatus-program*
		  nil			; infile
		  t			; current buffer
		  nil			; don't redisplay as output
		  "-L" "10")
    ;; (goto-line line-number) in a program:
    (goto-char (point-min))
    (forward-line (1- line-number)))
  (message "Checking folder status...done.")
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun dkl:mh-inbox-summary-toggle-snoozes ()
  (interactive)
  (save-excursion
    (cond
     (dkl:mh-inbox-summary-snooze-toggle
      ;; currently invisible, make visible
      (delete-overlay dkl:mh-inbox-summary-snooze-toggle)
      (setq dkl:mh-inbox-summary-snooze-toggle nil))
     (t ;; currently visible, make invisible
      (goto-char (point-max))
      (let* ((end (point-max))
	     (start (re-search-backward "^Snoozed messages:" nil t))
	     (overlay (make-overlay start end)))
	(setq dkl:mh-inbox-summary-snooze-toggle overlay)
	(overlay-put dkl:mh-inbox-summary-snooze-toggle
		     'invisible 'dkl:mh-inbox-summary-hide-lines))))))


(defvar dkl:my-inbox-summary-mode-map (make-keymap))
;;
(let ((map dkl:my-inbox-summary-mode-map))
  (define-key map "."    'dkl:mh-inbox-summary-goto)
  (define-key map "f"    'dkl:mh-inbox-summary-goto)
  (define-key map "\C-m" 'dkl:mh-inbox-summary-goto)
  (define-key map "g"    'dkl:mh-inbox-summary)
  (define-key map "s"    'dkl:mh-inbox-summary-toggle-snoozes)
  (define-key map "S"    'dkl:mh-inbox-summary-scan)
  (define-key map "q"    'dkl:mh-inbox-summary-quit))

(define-derived-mode dkl:mh-inbox-summary-mode text-mode "MH-Inboxes"
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (use-local-map dkl:my-inbox-summary-mode-map))

(defun dkl:mh-inbox-summary-goto ()
  (interactive)
  (beginning-of-line)
  (when (not (looking-at "\\(\\+[^ ]+\\)"))
    (error "can't parse current line"))
  (let ((folder
	 ;; Grab folder, before we call looking-at, since it will wipe
	 ;; out the previous search results.
	 (buffer-substring (match-beginning 1) (match-end 1))))
      ;; Get new mail first.
      (when (and (null dkl:mh-inbox-summary-inc-status)
		 (looking-at ".* new "))
	(save-window-excursion (mh-rmail))
	(delete-other-windows (get-buffer-window dkl:mh-inbox-summary-buffer))
	(setq dkl:mh-inbox-summary-inc-status t))
      
      (mh-visit-folder folder)))

(defun dkl:mh-inbox-summary-quit ()
  (interactive)
  (when dkl::mh-inbox-summary-window-config
    (set-window-configuration dkl::mh-inbox-summary-window-config)
    (setq dkl::mh-inbox-summary-window-config nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'mh-iterate-on-messages-in-region 'fi:emacs-lisp-indent-hook 3)
