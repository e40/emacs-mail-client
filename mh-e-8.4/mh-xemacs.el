;;; mh-xemacs.el --- XEmacs MH-E support

;; Copyright (C) 2001-2006, 2011 Free Software Foundation, Inc.

;; Author: FSF and others (see below)
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Change Log:

;;; Code:

(eval-when-compile
  (if (fboundp 'define-behavior)
      (define-behavior 'mh
        "Emacs interface to the MH mail system.
MH is the Rand Mail Handler. Other implementations include nmh
and GNU mailutils."
        :short-doc "Emacs interface to the MH mail system"
        :group 'internet
        :commands
        '(["Read Mail (MH-E)..." (mh-rmail t)]))))



;;; Tool Bar Icons

;;  This section contains the tool bar icons that MH-E uses under
;;  XEmacs. The XPM files are read from the files, the background
;;  color is updated to match XEmacs' background, and the resulting
;;  images are added to the association list mh-xemacs-icon-map.

(defconst mh-xemacs-has-tool-bar-flag
  (and (featurep 'toolbar)
       (featurep 'xpm)
       (device-on-window-system-p)
       (boundp 'default-toolbar-visible-p)
       (fboundp 'toolbar-make-button-list))
  "Non-nil means that XEmacs has tool bar support.")

;; TODO Isn't there a built-in icon we can use for this?
(defconst mh-xemacs-unknown-icon
  "/* XPM */
static char * file[] = {
\"24 24 6 1\",
\": c None s backgroundToolBarColor\",
\"# c #011801180102\",
\"r c #ff6666\",
\"g c #00cc00\",
\"b c #6666ff\",
\". c #e79de79dd134\",
\"::::::::::::::::::::::::\",
\"::################::::::\",
\"::#...............#:::::\",
\"::#................#::::\",
\"::#....ggg..........#:::\",
\"::#...g..gg..........#::\",
\"::#...g.ggg#.........#::\",
\"::#...ggggg#.........#::\",
\"::#....ggg##.........#::\",
\"::#.....###bbbbbbb...#::\",
\"::#........b..bbbb#..#::\",
\"::#........b.bbbbb#..#::\",
\"::#...r....bbbbbbb#..#::\",
\"::#...rr...bbbbbbb#..#::\",
\"::#...rrr..bbbbbbb#..#::\",
\"::#...rrrr..#######..#::\",
\"::#...rrrrr..........#::\",
\"::#...rrrrrr.........#::\",
\"::#...rrrrrrr........#::\",
\"::#...########.......#::\",
\"::#..................#::\",
\"::#..................#::\",
\"::####################::\",
\":::::::::::::::::::::::::};"
  "*MH unknown-icon icon.")

(defun mh-icon-image (icon &optional background)
  "Return the XPM image for ICON.
Returns nil if ICON.xpm cannot be found in `load-path'.
Optional argument BACKGROUND can be used to override transparent
background. For tool bars, \"c None s backgroundToolBarColor\" is a
good choice."
  (let ((filename (locate-library (format "%s.xpm" icon))))
    (if filename
        (with-temp-buffer
          (insert-file-contents-literally filename)
          (when background
            (search-forward "c None")
            (replace-match background))
          (buffer-substring (point-min) (point-max)))
      mh-xemacs-unknown-icon)))

;; TODO Extract names from mh-tool-bar.el.
;; These names are duplicated in the Makefile and mh-tool-bar.el.
(defconst mh-xemacs-icon-list
  '(
    attach contact copy cut data-save delete help left-arrow
    mail/compose mail/flag-for-followup mail/inbox mail/move mail/repack
    mail/reply mail/reply-all mail/reply-from mail/reply-to mail/send
    next-page open paste preferences refresh right-arrow save search
    show spell undo zoom-out)
  "List of icons used by MH-E.")

(defvar mh-xemacs-icon-map
  (let ((load-path (mh-image-load-path-for-library "mh-e" "mh-logo.xpm"))
        (background "c None s backgroundToolBarColor"))
    (loop for icon in mh-xemacs-icon-list
          collect (cons icon
                        (toolbar-make-button-list
                         (mh-icon-image icon background)))))
  "Map of GNU Emacs icon file names to XEmacs images.")



;;; Modeline Glyph
(defvar mh-modeline-glyph
  (let* ((load-path (mh-image-load-path-for-library "mh-e" "mh-logo.xpm"))
         (file-xpm (locate-library "mh-logo.xpm"))
         (glyph (make-glyph
                 (cond ((and (featurep 'xpm)
                             (device-on-window-system-p)
                             has-modeline-p)
                        `[xpm :file ,file-xpm])
                       (t [string :data "MH-E"])))))
    (set-glyph-face glyph 'modeline-buffer-id)
    glyph)
  "Cute little logo to put in the modeline of MH-E buffers.")



(provide 'mh-xemacs)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; mh-xemacs.el ends here
