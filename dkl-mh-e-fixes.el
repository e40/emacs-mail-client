;; Fixes for mh-e

;;;;NOTE: other fixes inserted into the actual .el files for mh-e.

;; Keep the point where it was before the yank.  This is so yanking a
;; message into a reply buffer doesn't put the point at the end.
;;
(require 'mh-letter) ;; where the following is defined
(defadvice mh-yank-cur-msg (around mh-yank-cur-msg-around () activate)
  (save-excursion ad-do-it))
