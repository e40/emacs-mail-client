
;; Basic functions needed to run my customizations.
;; Partly here so others can use things like dkl-mh-mailfilter.el and
;; dkl-mh-e-fixes.el.

;;;; this doesn't make any sense, since I haven't started using
;;;; the cl- versions of anything yet...
;;;(condition-case nil (require 'cl-lib)
;;;  (error (require 'cl)))
(require 'cl)

(defun dkl:probe-file (file)
  (when (file-exists-p file) file))

;; used???
(defun dkl:safe-require (name)
  (condition-case ()
      (let ((debug-on-error nil)) (require name))
    (error (message "Failed to require %s" name))))

;; used???
(defun dkl:safe-load (path)
  (condition-case ()
      (let ((debug-on-error nil)) (load path))
    (error (message "Failed to load %s" path))))

(defun dkl:byte-compile-file (filename &optional load)
  (let ((elc (byte-compile-dest-file filename)))
    (cond ((file-newer-than-file-p elc filename)
	   (when load (load elc)))
	  (t (byte-compile-file filename load)))))

(defun dkl:read-objects-from-string (string)
  (let ((objects '())
	(read-start 0)
	x)
    (while (condition-case c
	       (setq x (read-from-string string read-start))
	     (end-of-file nil)
	     (error (error "%s" c)))
      (push (car x) objects)
      (setq read-start (cdr x)))
    (nreverse objects)))


(defun dkl:short-system-name ()
  (let ((s (system-name)))
    (if (string-match "\\([^.]+\\)\\(\\..*$\\)?" s)
	(match-string 1 s)
      (error "Could not determine short system-name from '%s'." s))))


(defun dkl:filter (predicate list)
  (let ((result '()))
    (dolist (item list)
      (when (funcall predicate item)
		(push item result)))
    (nreverse result)))

(defun dkl:on-macosx-p ()
  (string-match "darwin" (symbol-name system-type)))

(defun dkl:on-linux-p ()
  (string-match "linux" (symbol-name system-type)))

(defun dkl:on-windows-p ()
  (memq system-type '(windows-nt ms-windows ms-dos win386)))

