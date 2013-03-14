;;; mh-autoloads.el --- automatically extracted external autoloads for MH-E 
 
;; Copyright (C) 2003-2012 Free Software Foundation, Inc. 
;; Author: Bill Wohler <wohler@newt.com> 
;; Keywords: mail 
;;; Commentary: 
 
;; If you installed MH-E separately from Emacs, you can set up its entry 
;; points using the following in your .emacs file: 
 
;;   (require 'mh-autoloads) 
 
;;; Change Log: 
;;; Code:


;;;### (autoloads (mh-version) "mh-e" "mh-e.el" (20658 48570))
;;; Generated autoloads from mh-e.el

(put 'mh-progs 'risky-local-variable t)

(put 'mh-lib 'risky-local-variable t)

(put 'mh-lib-progs 'risky-local-variable t)

(autoload 'mh-version "mh-e" "\
Display version information about MH-E and the MH mail handling system.

\(fn)" t nil)

;;;***

;;;### (autoloads (mh-folder-mode mh-nmail mh-rmail) "mh-folder"
;;;;;;  "mh-folder.el" (20658 48570))
;;; Generated autoloads from mh-folder.el

(autoload 'mh-rmail "mh-folder" "\
Incorporate new mail with MH.
Scan an MH folder if ARG is non-nil.

This function is an entry point to MH-E, the Emacs interface to
the MH mail system.

\(fn &optional ARG)" t nil)

(autoload 'mh-nmail "mh-folder" "\
Check for new mail in inbox folder.
Scan an MH folder if ARG is non-nil.

This function is an entry point to MH-E, the Emacs interface to
the MH mail system.

\(fn &optional ARG)" t nil)

(autoload 'mh-folder-mode "mh-folder" "\
Major MH-E mode for \"editing\" an MH folder scan listing.\\<mh-folder-mode-map>

You can show the message the cursor is pointing to, and step through
the messages. Messages can be marked for deletion or refiling into
another folder; these commands are executed all at once with a
separate command.

Options that control this mode can be changed with
\\[customize-group]; specify the \"mh\" group. In particular, please
see the `mh-scan-format-file' option if you wish to modify scan's
format.

When a folder is visited, the hook `mh-folder-mode-hook' is run.

Ranges
======
Many commands that operate on individual messages, such as
`mh-forward' or `mh-refile-msg' take a RANGE argument. This argument
can be used in several ways.

If you provide the prefix argument (\\[universal-argument]) to
these commands, then you will be prompted for the message range.
This can be any valid MH range which can include messages,
sequences, and the abbreviations (described in the mh(1) man
page):

<num1>-<num2>
    Indicates all messages in the range <num1> to <num2>, inclusive.
    The range must be nonempty.

<num>:N
<num>:+N
<num>:-N
    Up to N messages beginning with (or ending with) message num. Num
    may be any of the predefined symbols: first, prev, cur, next or
    last.

first:N
prev:N
next:N
last:N
    The first, previous, next or last messages, if they exist.

all
    All of the messages.

For example, a range that shows all of these things is `1 2 3
5-10 last:5 unseen'.

If the option `transient-mark-mode' is set to t and you set a
region in the MH-Folder buffer, then the MH-E command will
perform the operation on all messages in that region.

\\{mh-folder-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("mh-acros.el" "mh-alias.el" "mh-buffers.el"
;;;;;;  "mh-compat.el" "mh-funcs.el" "mh-gnus.el" "mh-identity.el"
;;;;;;  "mh-inc.el" "mh-junk.el" "mh-letter.el" "mh-limit.el" "mh-loaddefs.el"
;;;;;;  "mh-mime.el" "mh-print.el" "mh-scan.el" "mh-search.el" "mh-seq.el"
;;;;;;  "mh-show.el" "mh-speed.el" "mh-thread.el" "mh-tool-bar.el"
;;;;;;  "mh-utils.el" "mh-xface.el") (20658 48636 378596))

;;;***

;;;### (autoloads (mh-fully-kill-draft mh-send-letter mh-user-agent-compose
;;;;;;  mh-smail-batch mh-smail-other-window mh-smail) "mh-comp"
;;;;;;  "mh-comp.el" (20658 48570))
;;; Generated autoloads from mh-comp.el

(autoload 'mh-smail "mh-comp" "\
Compose a message with the MH mail system.
See `mh-send' for more details on composing mail.

\(fn)" t nil)

(autoload 'mh-smail-other-window "mh-comp" "\
Compose a message with the MH mail system in other window.
See `mh-send' for more details on composing mail.

\(fn)" t nil)

(autoload 'mh-smail-batch "mh-comp" "\
Compose a message with the MH mail system.

This function does not prompt the user for any header fields, and
thus is suitable for use by programs that want to create a mail
buffer. Users should use \\[mh-smail] to compose mail.

Optional arguments for setting certain fields include TO,
SUBJECT, and OTHER-HEADERS. Additional arguments are IGNORED.

This function remains for Emacs 21 compatibility. New
applications should use `mh-user-agent-compose'.

\(fn &optional TO SUBJECT OTHER-HEADERS &rest IGNORED)" nil nil)

(define-mail-user-agent 'mh-e-user-agent 'mh-user-agent-compose 'mh-send-letter 'mh-fully-kill-draft 'mh-before-send-letter-hook)

(autoload 'mh-user-agent-compose "mh-comp" "\
Set up mail composition draft with the MH mail system.
This is the `mail-user-agent' entry point to MH-E. This function
conforms to the contract specified by `define-mail-user-agent'
which means that this function should accept the same arguments
as `compose-mail'.

The optional arguments TO and SUBJECT specify recipients and the
initial Subject field, respectively.

OTHER-HEADERS is an alist specifying additional header fields.
Elements look like (HEADER . VALUE) where both HEADER and VALUE
are strings.

CONTINUE, SWITCH-FUNCTION, YANK-ACTION, SEND-ACTIONS, and
RETURN-ACTION and any additional arguments are IGNORED.

\(fn &optional TO SUBJECT OTHER-HEADERS CONTINUE SWITCH-FUNCTION YANK-ACTION SEND-ACTIONS RETURN-ACTION &rest IGNORED)" nil nil)

(autoload 'mh-send-letter "mh-comp" "\
Save draft and send message.

When you are all through editing a message, you send it with this
command. You can give a prefix argument ARG to monitor the first stage
of the delivery; this output can be found in a buffer called \"*MH-E
Mail Delivery*\".

The hook `mh-before-send-letter-hook' is run at the beginning of
this command. For example, if you want to check your spelling in
your message before sending, add the function `ispell-message'.

Unless `mh-insert-auto-fields' had previously been called
manually, the function `mh-insert-auto-fields' is called to
insert fields based upon the recipients. If fields are added, you
are given a chance to see and to confirm these fields before the
message is actually sent. You can do away with this confirmation
by turning off the option `mh-auto-fields-prompt-flag'.

In case the MH \"send\" program is installed under a different name,
use `mh-send-prog' to tell MH-E the name.

The hook `mh-annotate-msg-hook' is run after annotating the
message and scan line.

\(fn &optional ARG)" t nil)

(autoload 'mh-fully-kill-draft "mh-comp" "\
Quit editing and delete draft message.

If for some reason you are not happy with the draft, you can use
this command to kill the draft buffer and delete the draft
message. Use the command \\[kill-buffer] if you don't want to
delete the draft message.

\(fn)" t nil)

;;;***
 
(provide 'mh-autoloads) 
;; Local Variables: 
;; version-control: never 
;; no-byte-compile: t 
;; no-update-autoloads: t 
;; End: 
;;; mh-autoloads.el ends here
