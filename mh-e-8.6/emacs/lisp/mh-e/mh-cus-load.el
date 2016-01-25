;;; mh-cus-load.el --- automatically extracted custom dependencies
 
;; Copyright (C) 2003-2016 Free Software Foundation, Inc. 
;; Author: Bill Wohler <wohler@newt.com> 
;; Keywords: mail 
;;; Commentary: 
 
;; If you installed MH-E separately from Emacs, you can set up its custom 
;; dependencies using the following in your .emacs file: 
 
;;   (require 'mh-cus-load) 
 
;;; Change Log:

;;
;;; Code:

(put 'faces 'custom-loads '(mh-e))
(put 'mail 'custom-loads '(mh-e))
(put 'mh-alias 'custom-loads '(mh-e))
(put 'mh-e 'custom-loads '(mh-e))
(put 'mh-faces 'custom-loads '(mh-e))
(put 'mh-folder 'custom-loads '(mh-e))
(put 'mh-folder-selection 'custom-loads '(mh-e))
(put 'mh-hooks 'custom-loads '(mh-e))
(put 'mh-identity 'custom-loads '(mh-e))
(put 'mh-inc 'custom-loads '(mh-e))
(put 'mh-junk 'custom-loads '(mh-e))
(put 'mh-letter 'custom-loads '(mh-e))
(put 'mh-ranges 'custom-loads '(mh-e))
(put 'mh-scan-line-formats 'custom-loads '(mh-e))
(put 'mh-search 'custom-loads '(mh-e))
(put 'mh-sending-mail 'custom-loads '(mh-e))
(put 'mh-sequences 'custom-loads '(mh-e))
(put 'mh-show 'custom-loads '(mh-e))
(put 'mh-speedbar 'custom-loads '(mh-e))
(put 'mh-thread 'custom-loads '(mh-e))
(put 'mh-tool-bar 'custom-loads '(mh-e))

;; The remainder of this file is for handling :version.
;; We provide a minimum of information so that `customize-changed-options'
;; can do its job.

;; For groups we set `custom-version', `group-documentation' and
;; `custom-tag' (which are shown in the customize buffer), so we
;; don't have to load the file containing the group.

;; This macro is used so we don't modify the information about
;; variables and groups if it's already set. (We don't know when
;; mh-cus-load.el is going to be loaded and at that time some of the
;; files might be loaded and some others might not).
(defmacro custom-put-if-not (symbol propname value)
  `(unless (get ,symbol ,propname)
     (put ,symbol ,propname ,value)))


(defvar custom-versions-load-alist nil
  "For internal use by custom.
This is an alist whose members have as car a version string, and as
elements the files that have variables or faces that contain that
version.  These files should be loaded before showing the customization
buffer that `customize-changed-options' generates.")


(provide 'mh-cus-load)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mh-cus-load.el ends here
