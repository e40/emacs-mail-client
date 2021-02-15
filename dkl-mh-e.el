
;; Add one of these to ~/.emacs
;;    See mh-identity-list setting below
;;;(custom-set-variables '(mh-identity-default "Home"))
;;;(custom-set-variables '(mh-identity-default "Work"))

(when (or (< emacs-major-version 27)
	  (< emacs-minor-version 1))
  (error "emacs-mail-client requires Emacs 27.1"))

(push (format "%smh-e-27.1/" (file-name-directory load-file-name))
      load-path)

(require 'mh-e)
(require 'mh-comp)
(require 'mh-mime)
(require 'mh-utils)
(require 'mh-alias)
(require 'mh-junk)
(require 'mh-letter)
(require 'mh-folder)
(require 'mh-utils)
(require 'mh-thread)

(dkl:byte-compile-file (format "%sdkl-mh-e-fixes.el"
			       (file-name-directory load-file-name))
		       t)
(dkl:byte-compile-file (format "%sdkl-mh-mailfilter.el"
			       (file-name-directory load-file-name))
		       t)

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

   ;; this is runner up to gnus-w3m, but it's sometimes SLOW, and sometimes
   ;; HANGS.
   ;;'shr

   ;; With GNU Emacs 24.4 this works best:
   'gnus-w3m
   ;; the code executed in the above case is in
   ;;    gnus/gnus-html.el:gnus-article-html
   ;; it calls the w3m program with a set of arguments that seems to be
   ;; more pleasing than other drivers that call w3m.

;;;; See mh-identity-default setting above
;;;   '(mh-identity-list
;;;     ;; ~/.emacs sets mh-identity-default to one of these:
;;;     (quote (("Home" (("From" . "Your Name <username@home>")
;;;		      ("Fcc" . "+outbox")))
;;;	     ("Work" (("From" . "Your Name <username@work>")
;;;		      ("Fcc" . "+outbox"))))))
   )
 
 ;; This stops MH-E from making TLS connections for spam and other random
 ;; emails.
 '(gnus-blocked-images ".")
 
 ;; This makes text/plain preferred over text/html!! Yay!!! 
 '(mm-discouraged-alternatives '("text/html"))
 
 ;; This is so the html part of mime messages are rendered in a pleasing
 ;; way and so lines are not wrapped unnecessarily.  It's particularly bad
 ;; for code, when the default is `70'.  See test359 for an example.
 '(gnus-html-frame-width 999)
 
 '(mm-attachment-file-modes 420)	; mode 644 not 600!
 (list 'mh-inc-prog
       (cond ((file-exists-p "/usr/local/incfilter") "/usr/local/incfilter")
	     ((file-exists-p "/usr/fi/incfilter") "/usr/fi/incfilter")
	     (t "incfilter")))
 (list 'mh-alias-local-users
       ;; "ypcat passwd"
       ;; use /etc/passwd:
       t)
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
     "References"
;;;; Don't hide this, SPR creation uses it in preference to the From,
;;;; which for some customers is really screwed up.  Monil looking at you.
     ;;"Reply-To"
     "Sender" "User-" "Mail-" "Bh-Id:" "Class:"
     "mls-" "ARC-" "msip_labels" "IronPort-" "Autocrypt"
     ))
 '(mh-refile-preserves-sequences-flag
   ;; 9/6/04:
   ;; Done after seeing discussion on the mh-e-users mailing list of the
   ;; error message:
   ;;   refile: message 55 doesn't exist
;;;;4/28/15: try turning it back on for a while:
   t)
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
 '(mh-reply-default-reply-to "cc"))


(defun my-mh-compose-letter-function (to subject cc)
  ;; automatically fix bh header when report id in subject
  ;; Fcc to the source folder, too!  Idea from Ahmon.
  (when (string= mh-version "8.6")
    ;; hopefully fixed in the next MH-E version.
    ;; Workaround from: https://sourceforge.net/p/mh-e/bugs/486/
    (mh-decode-message-header))
  (when (and
	 ;; DISALBED, use dkl:mh-fcc-inbox and bind it to a key to do it
	 ;; when desired.
	 nil
	 mh-sent-from-folder)
    (when (and mh-annotate-field
	       (string= mh-annotate-field "Replied:"))
      (my-mh-insert-fields "Fcc:" mh-sent-from-folder)))
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
  
  (goto-char (point-max)))

(add-hook 'mh-compose-letter-function 'my-mh-compose-letter-function)

(defun my-mh-show-mode-hook ()
  (save-excursion
    (let ((was-read-only buffer-read-only))
      (when buffer-read-only (setq buffer-read-only nil))
      (goto-char (point-min))
      (while (search-forward (char-to-string 160) nil t)
	(replace-match " " nil t))
      (when was-read-only (setq buffer-read-only t))
      (setq truncate-lines t))))

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

(defadvice mh-widen (after my-highlight-messages () activate)
  (my-highlight-messages 'mh-widen mh-current-folder))

;;;;;;;;;

;; NOTE: use M-x list-colors-display to find suitable colors for the
;; situation.

(cond
 ((eq 'x window-system)
  (defface my-scan-line-highlight '((t (:background "yellow")))
    "The face I use to highlight scan lines in MH-E."
    :group 'my-faces)
  (defface my-scan-line-highlight-maybe-spam '((t (:background "cyan")))
    "The face I use to highlight scan lines in MH-E."
    :group 'my-faces))
 (t ;; -nw
  (defface my-scan-line-highlight '((t (:background "brightyellow")))
    "The face I use to highlight scan lines in MH-E."
    :group 'my-faces)
  (defface my-scan-line-highlight-maybe-spam '((t (:background "brightcyan")))
    "The face I use to highlight scan lines in MH-E."
    :group 'my-faces)))

(defun my-highlight-messages (from folder &optional range)
;;;;used to debug redundant calls to this function
  ;;(message "my-highlight-messages from %s" from)
  (my-highlight-by-header mh-current-folder range)
  (my-highlight-by-sequence mh-current-folder range))


(defun my-highlight-by-header (folder range)
  (condition-case ()
      (progn
	(my-highlight-by-header-1
	 folder range "highlight" 'my-scan-line-highlight)
	(my-highlight-by-header-1
	 folder range "maybe-spam" 'my-scan-line-highlight-maybe-spam))
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
;; comparing to the original (mh-exec-cmd), my version is much smaller and
;; differs in these ways:
;;  - doesn't use mh-log-buffer (uses my own buffer)
;;  - I erase my log buffer each time
;;  - no annotation on errors
;;  - no sit-for to display it to the user
;;  - uses mh-exec-cmd-output to do the work
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

;; DISABLED:
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
;;;;DKL: changed " " to ", "
               (insert ", " (or value "")))
              (t
               (insert field-name " " value "\n")))
        (setq name-values (cdr (cdr name-values)))))))

(defun my-mh-letter-mode-hook-function3 ()
  (let* ((map mh-letter-mode-map)
	 (seq "\C-c\C-ff")
	 (seq2 "\C-c\C-f\C-f")
	 (f (lookup-key map seq)))
    (when (not (assoc (char-to-string (logior ?\C-z ?`))
		      mh-to-field-choices))
      (push '("z" . "Filter:") mh-to-field-choices))
    (when (eq 'mh-to-fcc f)
      (define-key map seq  'my-mh-to-fcc)
      (define-key map seq2 'my-mh-to-fcc))))

(defun my-mh-to-fcc (&optional folder)
  (interactive (list (mh-prompt-for-folder
                      "Fcc"
                      (or (and mh-default-folder-for-message-function
                               (save-excursion
                                 (goto-char (point-min))
                                 (funcall
                                  mh-default-folder-for-message-function)))
                          "")
                      t)))
  (let ((last-input-event (if current-prefix-arg ?\C-z ?\C-f)))
    (expand-abbrev)
    (save-excursion
      (mh-to-field)
      (insert (if (mh-folder-name-p folder)
                  (substring folder 1)
                folder)))))

(add-hook 'mh-letter-mode-hook 'my-mh-letter-mode-hook-function0)
(add-hook 'mh-letter-mode-hook 'my-mh-letter-mode-hook-function3)

(defun dkl:mh-fcc-from-folder ()
  (interactive)
  (save-excursion
    (cond
     ((and mh-sent-from-folder
;;;; Since it's manual, don't be so restrictive:
	   ;;mh-annotate-field (string= mh-annotate-field "Replied:")
	   )
      (my-mh-insert-fields "Fcc:" mh-sent-from-folder))
     (t (my-mh-insert-fields "Fcc:" "inbox")))))

(condition-case ()
    (mh-find-path)
  (error nil))

(defun dkl:mh-add-to-seq-and-refile (sequence folder)
  "Add thread to SEQUENCE and refile to FOLDER."
  (interactive (list (mh-read-seq-default "Add to" nil)
		     (intern (mh-prompt-for-refile-folder))))
  (unless (mh-valid-seq-p sequence)
    (error "Can't put message in invalid sequence %s" sequence))
  (when (not (string-match "^\\+inbox" (symbol-name folder)))
    (error "You can only refile to an +inbox folder, not %s." folder))
  (cond ((not (memq 'unthread mh-view-ops)) (error "Folder isn't threaded"))
        ((eobp) (error "No message at point"))
        (t
	 (let ((msgs (dkl:mh-current-thread-to-msg-list)))
	   (mh-add-msgs-to-seq msgs sequence)
	   (mh-thread-refile folder))
	 (kill-new (format "[%s:%s]" folder sequence)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; message priorities

;; helpful: https://rand-mh.sourceforge.io/book/mh/mhstr.html

(custom-set-variables
 ;; display value for X-Todo-Priority, if there:
 (list
  'mh-scan-format-nmh
  (concat
   "%4(msg)"
   "%<(cur)+%| %>%<{replied}-"
   "%?(nonnull(comp{to}))%<(mymbox{to})t%>"
   "%?(nonnull(comp{cc}))%<(mymbox{cc})c%>"
   "%?(nonnull(comp{bcc}))%<(mymbox{bcc})b%>"
   "%?(nonnull(comp{newsgroups}))n%>"
   "%<(zero) %>"
;;;;TODO: the [ and ] come from:
;;;;   mh-e-8.6/emacs/lisp/mh-e/mh-thread.el:mh-thread-generate-scan-lines
   ;;"%<{x-todo-priority}%-3{x-todo-priority} %|    %>"
   "%02(mon{date})/%02(mday{date})%<{date} %|*%>"
   "%<(mymbox{from})%<{to}To:%14(decode(friendly{to}))%>%>"
   "%<(zero)%17(decode(friendly{from}))%> "
   "%(decode{subject})%<{body}<<%{body}%>")))

;; Take over the current "P" keymap, since I never use those things.
(setq mh-ps-print-map (make-sparse-keymap))

;;(define-key (current-local-map) "P" mh-ps-print-map)

;;(define-key mh-ps-print-map "s" 'my-mh-set-message-priority)
;;(define-key mh-ps-print-map "d" 'my-mh-delete-message-priority)

(defvar my-mh-priority-header "X-Todo-Priority")

(defun my-mh-set-message-priority (priority)
  (interactive "nPriority: ")
  (let ((current-message (mh-get-msg-num t)))
    (my-mh-delete-message-priority current-message)
    (when (> priority 0)
      (mh-exec-cmd "anno" mh-current-folder current-message
		   "-component" my-mh-priority-header
		   "-nodate"
		   "-text" (format "%d" priority)
		   "-inplace"))))

(defun my-mh-delete-message-priority (message)
  (interactive (list (mh-get-msg-num t)))
  (mh-exec-cmd "anno" mh-current-folder message "-nodate" "-inplace"
	       "-delete" "-component" my-mh-priority-header))

(defun my-mh-learn-ham-in-folder ()
  (interactive)
  (let ((default-directory (file-name-as-directory
			    (mh-expand-file-name (buffer-name)))))
    (message "learn as ham from all messages in folder...")
    (message "%s"
	     (with-temp-buffer ()
	       (call-process-shell-command
		(format "%s --ham [1-9]*" mh-sa-learn-executable)
		nil
		t)
	       (buffer-string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snooze support

(defvar mh-snoozed-unsnooze-script "nmh-unsnooze.sh")

(defvar mh-snoozed-default-time-of-day "06:00")

(defvar mh-snoozed-folder        nil)
(defvar mh-snoozed-header-date   nil)
(defvar mh-snoozed-header-folder nil)

(defun mh-one-time-snooze-setup ()
  ;; Initialize the variabes set from MH profile components when MH-E is
  ;; set up, not at load time.
  (setq mh-snoozed-folder        (mh-profile-component "Snoozed-Folder"))
  (setq mh-snoozed-header-date   (mh-profile-component "Snoozed-Header-Date"))
  (setq mh-snoozed-header-folder (mh-profile-component "Snoozed-Header-Folder"))
  
  (remove-hook 'mh-find-path-hook 'mh-one-time-snooze-setup))

(add-hook 'mh-find-path-hook 'mh-one-time-snooze-setup)

(defvar mh-snoozed-folder-cached nil
  ;; cache the interned value of the snoozed folder, so we don't have to
  ;; keep interning it each time
  )

(defun mh-snooze-msg (msg snooze-folder snooze-date)
  "Snooze MSG to SNOOZE-FOLDER until SNOOZE-DATE.  Snoozing a message makes
it disappear from the current folder, to reappear on SNOOZE-DATE, which
also includes a time of day.  See `mh-snoozed-default-time-of-day'
for the default time of day for it to reappear in the original
folder.

Proper snoozing operation depends on:

Additions to $HOME/.mh_profile:

   Snoozed-Folder: +snoozed
   Snoozed-Header-Date: X-snooze-date
   Snoozed-Header-Folder: X-snooze-folder

Additional bindings in mh-folder-mode-map, suggested:

   (define-key mh-folder-mode-map \"s\" 'mh-snooze-msg)
   (define-key mh-folder-mode-map \"U\" 'mh-unsnooze-msg)

A crontab entry to hourly unsnooze messages, suggested:

   1 * * * *   $HOME/bin/nmh-unsnooze.sh

Cron implementations diff, so see your system documentation.

Headers are added to messages when snoozed and removed when 
unsnoozed."
  (interactive (list
		(mh-get-msg-num t)
		
		(cond
		 (mh-snoozed-folder-cached)
		 (t
		  (when (not (mh-folder-name-p mh-snoozed-folder))
		    (setq mh-snoozed-folder
		      (format "+%s" mh-snoozed-folder)))
		  (when (not (file-exists-p
			      (mh-expand-file-name mh-snoozed-folder)))
		    (my-mh-ensure-folder-exists mh-snoozed-folder-cached))
		  (setq mh-snoozed-folder-cached
		    (intern mh-snoozed-folder))))
		
		(my-get-date-and-time)))
  
  ;; annotate the message with SNOOZE-DATE
  (mh-annotate-msg msg mh-current-folder mh-note-refiled
		   "-nodate"
		   "-component" mh-snoozed-header-date
		   "-text" snooze-date)

  ;; annotate the message with the current folder, so we know where to
  ;; return the message
  (mh-annotate-msg msg mh-current-folder mh-note-refiled
		   "-nodate"
		   "-component" mh-snoozed-header-folder
		   "-text" mh-current-folder)
  
  ;; Now just refile to the snoozed folder
  (mh-refile-msg (list msg) snooze-folder t))

(defun mh-unsnooze-msg (msg)
  "Unsnooze the message at point.  The destination folder is given by
`mh-snoozed-header-folder'."
  (interactive (list (mh-get-msg-num t)))
  
  (let ((destination-folder
	 (intern
	  (or (my-mh-get-header-value-from-msg msg
					       mh-snoozed-header-folder)
	      mh-inbox))))

    ;; remove the snoozed annotation
    (mh-annotate-msg msg mh-current-folder mh-note-refiled
		     "-nodate" "-component" mh-snoozed-header-date
		     "-delete")

    ;; remove the snoozed annotation
    (mh-annotate-msg msg mh-current-folder mh-note-refiled
		     "-nodate" "-component" mh-snoozed-header-folder
		     "-delete")

    ;; make it new and refile it back to the folder it came from
    (let ((msgs (list msg)))
      (mh-add-msgs-to-seq msgs mh-unseen-seq)
      (mh-refile-msg msgs destination-folder t))))


;; Was surprised to find this wasn't already an MH-E utility
(defun my-mh-get-header-value-from-msg (msg header)
  "Retreive from MSG the string corresponding to HEADER's value.
HEADER does not end in a colon (:), unlike other MH-E functions that
deal with headers."
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (with-temp-buffer ()
      (erase-buffer)
      (insert-file-contents msg-file)
      (goto-char (point-min))
      (mh-get-header-field (format "%s:" header)))))

;; Was surprised to find this wasn't already an MH-E utility
(defun my-mh-ensure-folder-exists (folder)
  "Ensure that FOLDER exists, creating it if it does not.  Operates
silently.  FOLDER can be a symbol or a string, with or without a leading
plus (+)."
  (when (symbolp folder) (setq folder (symbol-name folder)))
  (setq folder ;; without the +
    (cond ((and (eq (aref folder 0) ?+))
	   (subseq folder 1))
	  (t folder)))
  (let ((folder-directory (format "%s%s" mh-user-path folder)))
    (or (file-exists-p mh-user-path)
	(error "mh-user-path does not exist: %s" mh-user-path))
    (when (not (file-exists-p folder-directory))
      (make-directory folder-directory))))

;; needed for org-read-date
(require 'org)

(defun my-get-date-and-time ()
  "Read a date and an optional time from the user, using 
org-read-date, and return the answer in a string of the form
YYYY-MM-DD,HH:MM.

See the documentation for `org-read-date' for information on valid
dates."
  (let ((answer (org-read-date
		 t nil nil "Snooze until: "
		 ;; the default value is the current date and the snoozed
		 ;; default time
		 (org-time-string-to-time
		  (format "%s %s"
			  (format-time-string "%F")
			  mh-snoozed-default-time-of-day)))))
    (cond ((string-match "^\\(....-..-..\\) \\(..:..\\)$" answer)
	   (format "%s,%s" (match-string 1 answer) (match-string 2 answer)))
	  ((string-match "^....-..-..$" answer)
	   ;; add default time
	   (format "%s,%s" answer mh-snoozed-default-time-of-day))
	  (t (error "can't parse: %s" answer)))))

;; Used by dkl:mh-inbox-summary.
(defun mh-snooze-summary ()
  "Insert a summary of the snoozed messages into the current buffer at
the point."
  (newline)
  (insert "Snoozed messages:")
  (newline)
  (newline)
  (call-process mh-snoozed-unsnooze-script
		nil			; infile
		t			; current buffer
		nil			; don't redisplay as output
		"-r"))
