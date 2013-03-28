
;; Redefine mh-unvisit-file so that it automatically discard
;; changes.  The reason we need this: something in MH-E is modifying the
;; buffer when they consist of certain types of HTML messages.

(require 'mh-show) ;; where the following is defined
(defun mh-unvisit-file ()
  "Separate current buffer from the message file it was visiting."
;;;;DKL: don't ask that dumb question
;;;  (or (not (buffer-modified-p))
;;;      (null buffer-file-name)           ;we've been here before
;;;      (yes-or-no-p (format "Message %s modified; discard changes? "
;;;                           (file-name-nondirectory buffer-file-name)))
;;;      (error "Changes preserved"))
  (clear-visited-file-modtime)
  (unlock-buffer)
  (setq buffer-file-name nil))

(require 'mh-comp) ;; where the following is defined
(defun mh-redistribute (to cc &optional message)
  "Redistribute a message.

This command is similar in function to forwarding mail, but it
does not allow you to edit the message, nor does it add your name
to the \"From\" header field. It appears to the recipient as if
the message had come from the original sender. When you run this
command, you are prompted for the TO and CC recipients. The
default MESSAGE is the current message.

Also investigate the command \\[mh-edit-again] for another way to
redistribute messages.

See also `mh-redist-full-contents-flag'.

The hook `mh-annotate-msg-hook' is run after annotating the
message and scan line."
  (interactive (list (mh-read-address "Redist-To: ")
                     (mh-read-address "Redist-Cc: ")
                     (mh-get-msg-num t)))
  (or message
      (setq message (mh-get-msg-num t)))
  (save-window-excursion
    (let ((folder mh-current-folder)
          (draft (mh-read-draft "redistribution"
                                (if mh-redist-full-contents-flag
                                    (mh-msg-filename message)
                                  nil)
                                nil)))
      (mh-goto-header-end 0)
;;;;;;DKL: added code:
      (when (and mh-identity-default mh-identity-list)
	(let* ((id-list (car (cdr (assoc mh-identity-default
					mh-identity-list))))
	       (from (assoc "From" id-list)))
	  (when from
	    (insert "Resent-From: " (cdr from) "\n"))))
;;;;;;...DKL
      (insert "Resent-To: " to "\n")
      (if (not (equal cc "")) (insert "Resent-cc: " cc "\n"))
      (mh-clean-msg-header
       (point-min)
       "^Message-Id:\\|^Received:\\|^Return-Path:\\|^Sender:\\|^Date:\\|^From:"
       nil)
      (save-buffer)
      (message "Redistributing...")
      (let ((env "mhdist=1"))
        ;; Setup environment...
        (setq env (concat env " mhaltmsg="
                          (if mh-redist-full-contents-flag
                              buffer-file-name
                            (mh-msg-filename message folder))))
        (unless mh-redist-full-contents-flag
          (setq env (concat env " mhannotate=1")))
        ;; Redistribute...
        (if mh-redist-background
            (mh-exec-cmd-env-daemon env mh-send-prog nil buffer-file-name)
          (mh-exec-cmd-error env mh-send-prog "-push" buffer-file-name))
        ;; Annotate...
        (mh-annotate-msg message folder mh-note-dist
                         "-component" "Resent:"
                         "-text" (format "\"%s %s\"" to cc)))
      (kill-buffer draft)
      (message "Redistributing...done"))))

;; Keep the point where it was before the yank.  This is so yanking a
;; message into a reply buffer doesn't put the point at the end.
;;
(require 'mh-letter) ;; where the following is defined
(defadvice mh-yank-cur-msg (around mh-yank-cur-msg-around () activate)
  (save-excursion ad-do-it))


(require 'mh-mime) ;; where mh-mml-attach-file is defined

;; redefined function from gnus.git/lisp/mml.el.
;;
;; we redefine it to take an optional argument for the initial-input
;;
;; used by mh-mml-attach-file
;;
(defun mml-minibuffer-read-description (&optional initial-input)
  (let ((description (read-string "One line description: " initial-input)))
    (when (string-match "\\`[ \t]*\\'" description)
      (setq description nil))
    description))


;; make the default name the same as the file
;;
(defun mh-mml-attach-file (&optional disposition)
  "Add a tag to insert a MIME message part from a file.

You are prompted for the filename containing the object, the
media type if it cannot be determined automatically, a content
description and the DISPOSITION of the attachment.

This is basically `mml-attach-file' from Gnus, modified such that a prefix
argument yields an \"inline\" disposition and Content-Type is determined
automatically."
  (let* ((file (mml-minibuffer-read-file "Attach file: "))
         (type (mh-minibuffer-read-type file))
         (description (mml-minibuffer-read-description
;;;;DKL:
		       (file-name-nondirectory file)))
         (dispos (or disposition
                     (mh-mml-minibuffer-read-disposition type))))
;;;;DKL:
    (setq mml-default-directory (file-name-directory file))
    (mml-insert-empty-tag 'part 'type type 'filename file
                          'disposition dispos 'description description)))

;; I was banned from the cygwin mailing list because of this function!
;; Can't put a "raw email address" in replies.  This is apparently
;; sourceware.com policy.
;;
(require 'mh-letter) ;; where the following is defined
(defun mh-extract-from-attribution ()
  "Extract phrase or comment from From header field."
  (save-excursion
    (if (not (mh-goto-header-field "From: "))
        nil
      (skip-chars-forward " ")
      (cond
       ((looking-at "\"\\([^\"\n]+\\)\" \\(<.+>\\)")
        (format "%s " (match-string 1)))
       ((looking-at "\\([^<\n]+\\)<.+>$")
        (format "%s" (match-string 1)))
       ((looking-at "\\([^ ]+@[^ ]+\\) +(\\(.+\\))$")
        (format "%s " (match-string 2)))
       ((looking-at " *\\(.+\\)$")
        (format "%s " (match-string 1)))))))


;; Satyaki Das: add .txt to attachment filenames for forwarded messages
;; so that piece of s*** Eudora can read my message.
;;
;; Is Eudora dead enough to remove this hack???
;;
(require 'mh-mime) ;; where the following is defined
(defadvice mh-mml-to-mime (after add-dot-txt activate)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
             "^Content-Disposition: attachment; filename=[0-9]+$"
             nil t)
      (insert ".txt"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixes http://sourceforge.net/p/mh-e/bugs/267/
;; From Ted Phelps <phelps@gnusto.com>
;; Patches to gnus/shr.el for mail display

(require 'gnus)
(require 'shr)

;; The fix defines shr-make-overlay and isn't in the gnus that is included
;; in 24.3 (released after the fix was put into gnus, btw).
(when (and (not (fboundp 'shr-make-overlay))
	   (= 24 emacs-major-version)
	   (<= 3 emacs-minor-version))

;; Make sure the module we are patching is loaded.
(when (not (fboundp 'shr-add-font))
  (error "gnus/shr is not loaded.  Internal error."))

(defun shr-make-overlay (beg end &optional buffer front-advance rear-advance)
  (let ((overlay (make-overlay beg end buffer front-advance rear-advance)))
    (overlay-put overlay 'evaporate t)
    overlay))

(defun shr-add-font (start end type)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (bolp)
	(skip-chars-forward " "))
      (let ((overlay (shr-make-overlay (point) (min (line-end-position) end))))
	(overlay-put overlay 'face type))
      (if (< (line-end-position) end)
	  (forward-line 1)
	(goto-char end)))))

(defun shr-expand-newlines (start end color)
  (save-restriction
    ;; Skip past all white space at the start and ends.
    (goto-char start)
    (skip-chars-forward " \t\n")
    (beginning-of-line)
    (setq start (point))
    (goto-char end)
    (skip-chars-backward " \t\n")
    (forward-line 1)
    (setq end (point))
    (narrow-to-region start end)
    (let ((width (shr-buffer-width))
	  column)
      (goto-char (point-min))
      (while (not (eobp))
	(end-of-line)
	(when (and (< (setq column (current-column)) width)
		   (< (setq column (shr-previous-newline-padding-width column))
		      width))
	  (let ((overlay (shr-make-overlay (point) (1+ (point)))))
	    (overlay-put overlay 'before-string
			 (concat
			  (mapconcat
			   (lambda (overlay)
			     (let ((string (plist-get
					    (overlay-properties overlay)
					    'before-string)))
			       (if (not string)
				   ""
				 (overlay-put overlay 'before-string "")
				 string)))
			   (overlays-at (point))
			   "")
			  (propertize (make-string (- width column) ? )
				      'face (list :background color))))))
	(forward-line 1)))))

(defun shr-insert-table (table widths)
  (shr-insert-table-ruler widths)
  (dolist (row table)
    (let ((start (point))
	  (height (let ((max 0))
		    (dolist (column row)
		      (setq max (max max (cadr column))))
		    max)))
      (dotimes (i height)
	(shr-indent)
	(insert shr-table-vertical-line "\n"))
      (dolist (column row)
	(goto-char start)
	(let ((lines (nth 2 column))
	      (overlay-lines (nth 3 column))
	      overlay overlay-line)
	  (dolist (line lines)
	    (setq overlay-line (pop overlay-lines))
	    (end-of-line)
	    (insert line shr-table-vertical-line)
	    (dolist (overlay overlay-line)
	      (let ((o (shr-make-overlay (- (point) (nth 0 overlay) 1)
					 (- (point) (nth 1 overlay) 1)))
		    (properties (nth 2 overlay)))
		(while properties
		  (overlay-put o (pop properties) (pop properties)))))
	    (forward-line 1))
	  ;; Add blank lines at padding at the bottom of the TD,
	  ;; possibly.
	  (dotimes (i (- height (length lines)))
	    (end-of-line)
	    (let ((start (point)))
	      (insert (make-string (string-width (car lines)) ? )
		      shr-table-vertical-line)
	      (when (nth 4 column)
		(shr-put-color start (1- (point)) :background (nth 4 column))))
	    (forward-line 1)))))
    (shr-insert-table-ruler widths)))

(defun shr-render-td (cont width fill)
  (with-temp-buffer
    (let ((bgcolor (cdr (assq :bgcolor cont)))
	  (fgcolor (cdr (assq :fgcolor cont)))
	  (style (cdr (assq :style cont)))
	  (shr-stylesheet shr-stylesheet)
	  overlays actual-colors)
      (when style
	(setq style (and (string-match "color" style)
			 (shr-parse-style style))))
      (when bgcolor
	(setq style (nconc (list (cons 'background-color bgcolor)) style)))
      (when fgcolor
	(setq style (nconc (list (cons 'color fgcolor)) style)))
      (when style
	(setq shr-stylesheet (append style shr-stylesheet)))
      (let ((cache (cdr (assoc (cons width cont) shr-content-cache))))
	(if cache
	    (progn
	      (insert (car cache))
	      (let ((end (length (car cache))))
		(dolist (overlay (cadr cache))
		  (let ((new-overlay
			 (shr-make-overlay (1+ (- end (nth 0 overlay)))
					   (1+ (- end (nth 1 overlay)))))
			(properties (nth 2 overlay)))
		    (while properties
		      (overlay-put new-overlay
				   (pop properties) (pop properties)))))))
	  (let ((shr-width width)
		(shr-indentation 0))
	    (shr-descend (cons 'td cont)))
	  ;; Delete padding at the bottom of the TDs.
	  (delete-region
	   (point)
	   (progn
	     (skip-chars-backward " \t\n")
	     (end-of-line)
	     (point)))
	  (push (list (cons width cont) (buffer-string)
		      (shr-overlays-in-region (point-min) (point-max)))
		shr-content-cache)))
      (goto-char (point-min))
      (let ((max 0))
	(while (not (eobp))
	  (end-of-line)
	  (setq max (max max (current-column)))
	  (forward-line 1))
	(when fill
	  (goto-char (point-min))
	  ;; If the buffer is totally empty, then put a single blank
	  ;; line here.
	  (if (zerop (buffer-size))
	      (insert (make-string width ? ))
	    ;; Otherwise, fill the buffer.
	    (while (not (eobp))
	      (end-of-line)
	      (when (> (- width (current-column)) 0)
		(insert (make-string (- width (current-column)) ? )))
	      (forward-line 1)))
	  (when style
	    (setq actual-colors
		  (shr-colorize-region
		   (point-min) (point-max)
		   (cdr (assq 'color shr-stylesheet))
		   (cdr (assq 'background-color shr-stylesheet))))))
	(if fill
	    (list max
		  (count-lines (point-min) (point-max))
		  (split-string (buffer-string) "\n")
		  (shr-collect-overlays)
		  (car actual-colors))
	  max)))))

)
