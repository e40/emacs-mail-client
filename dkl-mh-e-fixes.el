
;; Redefine mh-unvisit-file so that it doesn't automatically discard
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
