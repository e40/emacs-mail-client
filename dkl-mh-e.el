
;; Add one of these to ~/.emacs
;;;(custom-set-variables '(mh-identity-default "Home"))
;;;(custom-set-variables '(mh-identity-default "Work"))


;;;(setq load-path
;;;  (cons (expand-file-name "~layer/src/emacs/gnus.git/lisp/") load-path))
;;;(require 'gnus-load)

(push
 (expand-file-name "~/src/emacs/mh-e-8.5/emacs/trunk/lisp/mh-e/")
 load-path)

(dkl:safe-load "message")
(dkl:safe-load "mh-e")

(when (string= "8.4" mh-version)
  (error "MH-E 8.4 is buggy.  Don't use."))

(dkl:safe-require 'mh-comp)
(dkl:safe-require 'mh-mime)
(dkl:safe-require 'mh-utils)
(dkl:safe-require 'mh-alias)
(dkl:safe-require 'mh-junk)
;; MH-E 8.0 needs this (for mh-to-fcc):
(dkl:safe-require 'mh-letter)
(dkl:safe-require 'mh-folder)
(dkl:safe-require 'mh-utils) ;; make sure mh-folder-completion-map is defined
(dkl:safe-require 'mh-thread)

(dkl:byte-compile-file (format "%s/dkl-mh-e-fixes.el" *my-elib*) t)
(dkl:byte-compile-file (format "%s/dkl-mh-mailfilter.el" *my-elib*) t)

;;;(when (file-exists-p "/usr/bin/mh")
;;;  (custom-set-variables
;;;   '(mh-progs "/usr/bin/mh/")
;;;   '(mh-lib "/usr/lib/mh/")
;;;   '(mh-lib-progs mh-lib)))

(custom-set-variables
 ;; In search of a good renderer for HTML emails!  Oh my!
 '(mm-text-html-renderer
   ;;'w3m ;; emacs-w3m -- not installed
   ;;'w3 ;; Emacs/W3 -- not installed
   ;; 'w3m-standalone ;; w3m -- ugly, no pics, no links, fast
   ;;'links ;; ugliest, no pics, fast, links at bottom
   ;;'lynx ;; OK, no pics, no links, fast
   ;;'html2text -- unusable
   ;;nil ;; external viewer (default web browser)
   
   'shr ;; gnus, broken in 24.[12] w/o patch, sometimes hang w/patch
   )

 '(mh-identity-list
   ;; ~/.emacs sets mh-identity-default to one of these:
   (quote (("Home" (("From" . "Kevin Layer <layer@known.net>")
		    ("Fcc" . "+outbox")))
	   ("Work" (("From" . "Kevin Layer <layer@franz.com>")
		    ;;("Organization" . "Franz, Inc.")
		    ("Fcc" . "+outbox"))))))
 '(mm-attachment-file-modes 420)	; mode 644 not 600!
 (list 'mh-inc-prog
       (cond ((file-exists-p "/usr/local/incfilter") "/usr/local/incfilter")
	     ((file-exists-p "/usr/fi/incfilter") "/usr/fi/incfilter")
	     (t "incfilter")))
 (list 'mh-alias-local-users
       (if (at-work-on-unix-p)
	   "ypcat passwd"
	 ;; use /etc/passwd:
	 t))
 '(mh-invisible-header-fields-default
   ;; mh has a built-in list (mh-invisible-header-fields-internal) of
   ;; headers that it will not show.  Entries in
   ;; mh-invisible-header-fields-default will be removed from that list.
   ;; Make sure you specify removals in the same way that they are listed
   ;; in mh-invisible-header-fields-internal.  I.e., punctuation (colons)
   ;; and case matter.
   '("Message-Id:")) 
 '(mh-invisible-header-fields ;; hide 
   '("DKIM-Signature" "Comment" "DomainKey-" "X-" "In-" "Organization"
     "References" "Reply-To" "Sender" "User-" "Mail-" "Bh-Id:" "Class:"))
 '(mh-refile-preserves-sequences-flag
   ;; 9/6/04:
   ;; Done after seeing discussion on the mh-e-users mailing list of the
   ;; error message:
   ;;   refile: message 55 doesn't exist
   nil)
 '(mh-compose-forward-as-mime-flag nil) ;; don't forward as mime attachment
 '(mh-mime-save-parts-directory "~/incoming/")
 '(mh-search-program 'mairix)
 '(mh-show-use-xface-flag nil) ;; t is too slow...
 '(mh-junk-disposition
   ;; Stuff marked as `junk' will be moved to this folder
   "+inbox-spam")
 '(mh-junk-program 'spamassassin)
 '(mh-nmh-flag t)			;for Linux systems, very important
 '(mh-show-threads-flag t)
 '(mh-large-folder 500)
 '(mh-recenter-summary-flag t)
 '(mh-signature-file-name "~/.sig")
 '(mh-show-buffer-mode-line-buffer-id "%s/%d")
 '(mh-redist-full-contents
   ;; doesn't work if `t' if you've replied to the message already
   nil)
 '(mh-clean-message-header-flag t)
 '(mh-lpr-command-format "nenscript -Plw2 -h")
 '(mh-ins-buf-prefix ">> ")
 '(mh-delete-yanked-msg-window t)
 '(mh-reply-default-reply-to "cc")
 '(mh-store-default-directory "~/uu/") ;; ???????????????????
 )


(defun my-mh-compose-letter-function (to subject cc)
  ;; automatically fix bh header when report id in subject
  (when (and (fboundp 'bh=field-value)
	     (or subject
		 ;; When we do mh-edit-again, subject is nil, but it exists
		 ;; in the already composed email, so grab it:
		 (setq subject (bh=field-value "^Subject: ")))
	     (boundp 'bh-report-regexp)
	     bh-report-regexp
	     (string-match bh-report-regexp subject))
    (bh-fix-mail-header
     (downcase (substring subject (match-beginning 1)
			  (match-end 1)))))
  
  (save-excursion
    (goto-char (point-min))
    (replace-string (char-to-string 160) " "))
  )

(add-hook 'mh-compose-letter-function 'my-mh-compose-letter-function)

(defun my-mh-show-mode-hook ()
  (save-excursion
    (let ((was-read-only buffer-read-only))
      (when buffer-read-only (toggle-read-only -1))
      (goto-char (point-min))
      (while (search-forward (char-to-string 160) nil t)
	(replace-match " " nil t))
      (when was-read-only (toggle-read-only 1)))))

(add-hook 'mh-show-mode-hook 'my-mh-show-mode-hook)

;;;(defun my-mh-compose-letter-test-hook (to subject cc)
;;;  (message "foo: %s" mh-sent-from-folder)
;;;  (when (string-match "^\\+inbox$" mh-sent-from-folder)
;;;    (mh-to-fcc "+foobar")))
;;;(add-hook 'mh-compose-letter-function 'my-mh-compose-letter-test-hook)

;;;(defun my-move-to-saved ()
;;;  (interactive)
;;;  (mh-refile-a-msg nil '+saved)
;;;  (mh-next-msg t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin highlighting code

(defun my-mh-highlight-folder-hook-function ()
  (my-highlight-messages 'my-mh-highlight-folder-hook-function
			 mh-current-folder))

(add-hook 'mh-inc-folder-hook 'my-mh-highlight-folder-hook-function)

;;;(defadvice mh-scan-folder (after my-highlight-messages
;;;				 (folder range &optional dont-exec-pending)
;;;				 activate)
;;;;;;;This call is redundant
;;;  ;;(my-highlight-messages 'mh-scan-folder mh-current-folder (ad-get-arg 1))
;;;  )

(defadvice mh-visit-folder (after my-highlight-messages
				  (folder &optional range index-data)
				  activate)
  (my-highlight-messages 'mh-visit-folder mh-current-folder (ad-get-arg 1)))

(defadvice mh-pack-folder (after my-highlight-messages (range) activate)
  (my-highlight-messages 'mh-pack-folder mh-current-folder (ad-get-arg 0)))

(defadvice mh-rescan-folder (after my-highlight-messages
				   (&optional range dont-exec-pending)
				   activate)
  (my-highlight-messages 'mh-rescan-folder mh-current-folder (ad-get-arg 1)))

(defadvice mh-execute-commands (after my-highlight-messages () activate)
  (my-highlight-messages 'mh-execute-commands mh-current-folder))

;;;(defadvice mh-toggle-threads (after my-highlight-messages () activate)
;;;;;;;This call is redundant
;;;  ;;(my-highlight-messages 'mh-toggle-threads mh-current-folder)
;;;  )

(defadvice mh-widen (after my-highlight-messages () activate)
  (my-highlight-messages 'mh-widen mh-current-folder))

;;;;;;;;;

(defface my-scan-line-highlight '((t
			      ;;; one of these:
				   (:background "yellow")
				   ;;(:foreground "DarkGreen")
				   ))
  "The face I use to highlight scan lines in MH-E."
  :group 'my-faces)


(defun my-highlight-messages (from folder &optional range)
;;;;used to debug redundant calls to this function
  ;;(message "my-highlight-messages from %s" from)
  (my-highlight-by-header mh-current-folder range)
  (my-highlight-by-sequence mh-current-folder range))



(defun my-highlight-by-header (folder range)
  (condition-case ()
      (my-highlight-by-header-1
       folder range "highlight" 'my-scan-line-highlight)
    (error nil)))

(defun my-highlight-by-header-1 (folder range header face)
;;;;used to debug redundant calls to this function
  ;;(message "my-highlight-by-header-1: folder=%s range=%s" folder range)
  (let ((output
	 (if range
	     (my-mh-exec-cmd "pick" folder range "--X-Layer-Filter" header)
	   (my-mh-exec-cmd "pick" folder "--X-Layer-Filter" header)))
	messages over)
    (if (string-match "pick: no messages match specification" output)
	nil
      ;; `messages' has the message numbers which we need to highlight.
      (save-excursion
	(setq messages (dkl:read-objects-from-string output))
	(dolist (msgn messages)
	  (mh-goto-msg msgn nil t)
	  (setq over (make-overlay
		      (progn (beginning-of-line) (point))
		      (progn (end-of-line) (point))
		      nil nil nil))
	  (overlay-put over 'face face))))))

;;;;;; can't remember why I needed the following
(defun my-mh-exec-cmd (&rest command)
  (save-excursion
    (let ((temp " *my-mh-temp*"))
      (set-buffer (get-buffer-create temp))
      (erase-buffer)
      (apply 'mh-exec-cmd-output (car command) nil (cdr command))
      (buffer-string))))


(defconst my-mh-important-msg-seq 'important)


(defun my-highlight-by-sequence (folder range)
  (condition-case ()
      (my-highlight-by-sequence-1
       folder range my-mh-important-msg-seq 'my-scan-line-highlight)
    (error nil)))

(defun my-highlight-by-sequence-1 (folder range sequence face)
  (let (messages over)
    (save-excursion
      (setq messages (mh-seq-to-msgs sequence))
      (dolist (msgn messages)
	(mh-goto-msg msgn nil t)
	(setq over (make-overlay
		    (progn (beginning-of-line) (point))
		    (progn (end-of-line) (point))
		    nil nil nil))
	(overlay-put over 'face face)))))



(defun my-unhighlight-message (msgn)
  (save-excursion
    (mh-goto-msg msgn nil t)
    (remove-overlays (progn (beginning-of-line) (point))
		     (progn (end-of-line) (point)))))


(defun my-mark-important (&optional msg folder)
  (interactive (list (mh-get-msg-num t) mh-current-folder))
  (when (not (member my-mh-important-msg-seq
		     (mh-seq-containing-msg msg nil)))
    (let ((msgs (list msg)))
      (mh-add-msgs-to-seq msgs my-mh-important-msg-seq)
      (my-highlight-by-sequence folder msgs)
      (mh-next-msg nil)
      (mh-define-sequence 'cur (list (mh-get-msg-num t))))))

(defun my-mark-unimportant ()
  (interactive)
  (let ((current-message (mh-get-msg-num t)))
    (when (member my-mh-important-msg-seq
		  (mh-seq-containing-msg current-message nil))
      (mh-delete-a-msg-from-seq current-message my-mh-important-msg-seq nil)
      (my-unhighlight-message current-message)
      (mh-next-msg nil)
      (mh-define-sequence 'cur (list (mh-get-msg-num t))))))

;;;; not needed yet
;;;(defun my-message-in-sequence-p (message seq)
;;;  (memql message (mh-seq-to-msgs seq)))

(defun my-move-to-end (move-thread)
  (interactive "P")
  (cond (move-thread
	 (let ((region
		;; list of (start-position end-position)
		(mh-thread-find-children))
	       (messages '()))
	   (when (not (and (consp region)
			   (eql 2 (length region))))
	     (error "region is not a list of length 2: %s" region))
	   (save-excursion
	     (my-map-lines-region
	      (first region)
	      (second region)
	      (lambda () (push (mh-get-msg-num t) messages)))
	     (setq messages (nreverse messages))
	     (dolist (message messages)
	       (my-move-to-end-current-message nil message)))
	   (mh-rescan-folder)))
	(t (my-move-to-end-current-message t))))

(defun my-move-to-end-current-message (rescan &optional message)
  (let* ((message (or message (mh-get-msg-num t)))
	 (sequences (mh-seq-containing-msg message
					   ;; don't include `cur' and
					   ;; internal nmh/MH-E sequences
					   nil)))
    ;; move to end
    ;;   refile 257 -nolink -src +inbox +inbox
    (mh-refile-msg message
		   (intern mh-current-folder)
		   t ;; dont-update-last-destination-flag
		   )
    
    (mh-execute-commands)
    
    ;; add message to sequences
    (dolist (sequence sequences)
      (mh-add-msgs-to-seq (mh-translate-range mh-current-folder "last")
			  sequence nil t))
    
    (when rescan (mh-rescan-folder))))

;; ...end highlighting code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(defun my-my-mark-in-progress (range)
;;;  (interactive (list (mh-interactive-range "Mark in progress")))
;;;  (mh-put-msg-in-seq range 'agraph-approved))
;;;
;;;(defun my-my-cancel-in-progress (range)
;;;  (interactive (list (mh-interactive-range "Cancel in progress")))
;;;  (mh-delete-msg-from-seq range 'agraph-approved))

;;;(defun my-mh-unpack-msg ()
;;;  (interactive)
;;;  (let ((default-directory
;;;	    (cond ((at-work-on-unix-p)
;;;		   "/net/hobart/c/Documents and Settings/layer/Desktop/")
;;;		  ((at-home-on-unix-p)
;;;		   "/c/Documents and Settings/layer.OOB2/Desktop/"))))
;;;    (mh-pipe-msg "munpack" nil)))

(make-variable-buffer-local 'smooth-scroll-margin)

(defun my-mh-folder-mode-hook-function ()
  (interactive)
  (setq smooth-scroll-margin 0) ;; >0 doesn't work well with folder mode
  (cd "~/")
  (font-lock-mode))
(add-hook 'mh-folder-mode-hook 'my-mh-folder-mode-hook-function)

(defun my-mh-letter-mode-hook-function0 ()
  (run-hooks 'text-mode-hook)
  (setq comment-start ">> "))

(defun my-mh-letter-mode-hook-function2 ()
  ;; check if To: has "lifemasters.com" in it, then
  ;; 1. Cc: Powell and Fred
  ;; 2. Fcc: +inbox
  (save-excursion
    (let ((case-fold-search t))
      (when (string-match "lifemasters\\.com" (mh-get-header-field "To"))
	(let ((auto-cc '("swright@lifemasters.com"
			 "phamner@lifemasters.com"
			 "cnorvell"
			 "dm"))
	      (cc (mh-get-header-field "Cc")))
	  (dolist (acc auto-cc)
	    (when (not (string-match acc cc))
	      (my-mh-insert-fields "Cc:" acc)))
	  (my-mh-insert-fields "Fcc:" "inbox"))))))

(defun my-mh-insert-fields (&rest name-values)
  "Insert the NAME-VALUES pairs in the current buffer.
If the field exists, append the value to it.
Do not insert any pairs whose value is the empty string."
  (let ((case-fold-search t))
    (while name-values
      (let ((field-name (car name-values))
            (value (car (cdr name-values))))
        (if (not (string-match "^.*:$" field-name))
            (setq field-name (concat field-name ":")))
        (cond ((equal value "")
               nil)
              ((mh-position-on-field field-name)
;;;;DKL: change " " to ", "
               (insert ", " (or value "")))
              (t
               (insert field-name " " value "\n")))
        (setq name-values (cdr (cdr name-values)))))))

(add-hook 'mh-letter-mode-hook 'my-mh-letter-mode-hook-function0)
(add-hook 'mh-letter-mode-hook 'my-mh-letter-mode-hook-function2)

(when (and (boundp 'mh-folder-mode-map) mh-folder-mode-map)
  (let* ((escape-map (lookup-key mh-folder-mode-map "\e"))
	 (thread-map (lookup-key mh-folder-mode-map "T")))
    (when escape-map
      (define-key mh-folder-mode-map "\e" nil)
      (define-key mh-folder-mode-map "\C-c" escape-map))
    
    (define-key thread-map "^" 'dkl:mh-move-conversation))

;;;  (define-key mh-folder-mode-map "@" 'my-move-to-saved)
  
  (define-key mh-folder-mode-map "a" (lookup-key mh-folder-mode-map "r"))

  (define-key mh-folder-mode-map "\C-o" nil)
  (define-key mh-folder-mode-map "\C-d" nil)
  (define-key mh-folder-mode-map "C" 'mh-catchup) ; same as my-mh-catchup
  
  ;;(define-key mh-folder-mode-map "\C-cw" 'mh-widen)
  ;;(define-key mh-folder-mode-map "\C-cn" 'mh-narrow-to-seq)
;;;  (define-key mh-folder-mode-map "\C-c|" 'my-mh-unpack-msg)

  (define-key mh-folder-mode-map "*" 'my-mark-important)
  (define-key mh-folder-mode-map "_" 'my-mark-unimportant)
  (define-key mh-folder-mode-map ")" 'my-move-to-end)

  ;; a common typo:
  (define-key mh-folder-mode-map "l" nil)
  (define-key mh-folder-mode-map "]" 'bh-mh-append-current-message)
  (define-key mh-folder-mode-map "}" 'mh-store-msg)
  (define-key mh-folder-mode-map "/" 'bh-mh-visit-report-other-window)
  
;;;  (define-key mh-folder-mode-map "P" 'mh-set-priority)
  )

(my-setup-completion-map mh-folder-completion-map)

(when (and (boundp 'mh-show-mode-map) mh-show-mode-map)
  (define-key mh-show-mode-map "\C-o" nil)
  (define-key mh-show-mode-map "\C-d" nil))

(when (and (boundp 'mh-letter-mode-map) mh-letter-mode-map)
  ;; Move mh-send-letter from C-c C-c to C-c c
  (define-key mh-letter-mode-map "\C-c\C-c"
    '(lambda (&rest foo) (interactive) (error "send with C-c c")))
  (define-key mh-letter-mode-map "\C-cc" 'mh-send-letter))

(condition-case ()
    (mh-find-path)
  (error nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aborted "priority" scheme

;;;;;(setq mh-scan-prog "scan.priority")
;;;;; which uses ~/scan.priority
;;;;; or variable: mh-scan-format-file
;;;;; scan -form ~/scan.priority | sort -n -k3 -k1
;;;;; apropos ``mh-scan.*regexp'' to see the variables I need to tweak

;;;;To turn this on, eval these 3:
;;(setq mh-adaptive-cmd-note-flag nil)
;;(setq mh-scan-format-file "~/scan.priority")
;;(setq mh-scan-prog "scan")
;;;;;;;;;(setq mh-scan-prog "~/bin/scan.priority")


;;;(defun mh-priority-sort ()
;;;  (interactive)
;;;  (let ((msg-at-point (mh-get-msg-num nil))
;;;        (old-buffer-modified-flag (buffer-modified-p))
;;;        (buffer-read-only nil))
;;;    (mh-priority-sort-1)
;;;    (when msg-at-point (mh-goto-msg msg-at-point t t))
;;;    (set-buffer-modified-p old-buffer-modified-flag)
;;;    (mh-recenter nil)))
;;;
;;;(defun mh-priority-sort-1 ()
;;;  (message "Priority sorting %s..." (buffer-name))
;;;  (goto-char (point-min))
;;;  (mh-remove-all-notation)
;;;  (let ((msg-list ()))
;;;    (mh-iterate-on-range msg (cons (point-min) (point-max))
;;;      (push msg msg-list))
;;;    (let ((range (mh-coalesce-msg-list msg-list)))
;;;      (delete-region (point-min) (point-max))
;;;;;;;HERE: output sorted order
;;;      (apply
;;;       #'call-process
;;;       (expand-file-name "~/bin/scan.priority")
;;;       nil				;INFILE
;;;       '(t nil)				;BUFFER
;;;       nil				;DISPLAY
;;;       mh-current-folder
;;;       (mapcar #'(lambda (x) (format "%s" x)) msg-list)
;;;       )
;;;;;;;
;;;      (mh-notate-user-sequences)
;;;      (mh-notate-deleted-and-refiled)
;;;      (mh-notate-cur)
;;;;;;;TODO: some weird performance problem with this:
;;;;;;      (dolist (range msg-list)
;;;;;;	(my-highlight-messages nil mh-current-folder range))
;;;      (message "Priority sorting %s...done" (buffer-name)))))
;;;
;;;(defun mh-set-priority (range priority)
;;;  (interactive
;;;   (list (mh-interactive-range "message list for setting priority")
;;;	 (read-string "message priority: " nil nil "50")))
;;;  
;;;  (mh-iterate-on-range msg range
;;;    (mh-exec-cmd "anno" mh-current-folder msg
;;;		 "-component" "x-dkl-priority"
;;;		 "-inplace" "-delete")
;;;    
;;;    (mh-exec-cmd "anno" mh-current-folder msg
;;;		 "-component" "x-dkl-priority"
;;;		 "-nodate" "-inplace" "-append"
;;;		 "-text" priority)))
