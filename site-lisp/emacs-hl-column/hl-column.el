;;; hl-column.el --- Highlight the current column -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-08-13
;; Version: 1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience
;; Homepage: https://codeberg.org/akib/emacs-hl-column

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a local minor mode (toggled by M-x hl-column-mode) and a
;; global minor mode (toggled by M-x global-hl-column-mode) to
;; highlight, on a suitable terminal, the line on which point is.

;; You probably don't really want to use the global mode; if the
;; cursor is difficult to spot, try changing its color, relying on
;; `blink-cursor-mode' or both.

;; An overlay is used.  In the non-sticky cases, this overlay is
;; active only on the selected window.  A hook is added to
;; `post-command-hook' to activate the overlay and move it to the line
;; about point.

;;; Code:

;; The above commentary was adapted from hl-line commentary.

(require 'hl-line)                                 ; For hl-line face.

(defgroup hl-column nil
  "Highlight the current column."
  :group 'convenience
  :link '(url-link "https://codeberg.org/akib/emacs-hl-column")
  :prefix "hl-column-")

(defcustom hl-column-sticky-flag nil
  "Non-nil means the Hl-Column mode highlight appears in all windows.

Otherwise Hl-Column mode will highlight only in the selected
window."
  :type 'boolean)

(defcustom hl-column-highlight-point nil
  "Nil means don't highlight point while highlighting."
  :type 'boolean)

(defface hl-column '((t :inherit hl-line))
  "Default face for highlighting the current column in Hl-Column mode.")

(defvar hl-column--state nil
  "List containing data about current highlightion state.

The value of either nil or a list of form (WINDOW-START WINDOW-END
POINT HIGHLIGHTED-COLUMN OVERLAYS).  When nil, nothing is highlighted.
HIGHLIGHTED-COLUMN is the column currently highlighted, OVERLAYS is a
list of the overlays used to highlight current column and WINDOW-START
and WINDOW-END are markers to the points returned by `window-start'
and `window-end' respectively while making the overlays.")

(defcustom hl-column-face 'hl-column
  "Face to use to highlight the current column in Hl-Column mode."
  :type 'face
  :set (lambda (symbol value)
         (set symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when hl-column--state
               (hl-column--unhighlight)
               (hl-column--highlight))))))

(defun hl-column--unhighlight ()
  "Unhighlight the current column."
  (when hl-column--state
    (mapc #'delete-overlay (nth 4 hl-column--state))
    (setq hl-column--state nil)))

(defun hl-column--highlight ()
  "Highlight the currenbt column."
  (let ((column (current-column))
        (current-line-start (line-beginning-position))
        (current-line-end (line-end-position)))
    (unless (and hl-column--state
                 (= (nth 0 hl-column--state) (window-start))
                 (= (nth 1 hl-column--state) (window-end))
                 (or hl-column-highlight-point
                     (= (nth 2 hl-column--state) (point)))
                 (= (nth 3 hl-column--state) column))
      (hl-column--unhighlight)
      (let ((overlays nil))
        (save-excursion
          (goto-char (window-start))
          (while (< (point) (window-end))
            (unless (and (not hl-column-highlight-point)
                         (<= current-line-start (point)
                             current-line-end))
              (save-excursion
                (move-to-column column)
                (if (= (point) (line-end-position))
                    (let ((ov (make-overlay (line-end-position)
                                            (line-end-position))))
                      (push ov overlays)
                      (unless hl-column-sticky-flag
                        (overlay-put ov 'window (selected-window)))
                      (overlay-put
                       ov 'after-string
                       (concat
                        (propertize
                         " " 'display
                         `(space :width
                                 (,(* (frame-char-width)
                                      (max (- column (current-column))
                                           0)))))
                        (propertize " " 'face hl-column-face))))
                  (when (< column (current-column))
                    (backward-char))
                  (let ((mark (point)))
                    (move-to-column (1+ column))
                    (let ((ov (make-overlay mark (point))))
                      (push ov overlays)
                      (unless hl-column-sticky-flag
                        (overlay-put ov 'window (selected-window)))
                      (overlay-put ov 'face hl-column-face))))))
            (forward-line 1)))
        (setq hl-column--state
              (list (copy-marker (window-start))
                    (copy-marker (window-end)) (point-marker) column
                    overlays))))))

(define-minor-mode hl-column-mode
  "Toggle highlighting of the current column."
  :lighter nil                        ; Isn't the highlightion enough?
  (make-local-variable 'hl-column--state)
  (if hl-column-mode
      (progn
        (add-hook 'post-command-hook #'hl-column--highlight nil t)
        (add-hook 'change-major-mode-hook #'hl-column--unhighlight nil
                  t))
    (remove-hook 'post-command-hook #'hl-column--highlight t)
    (remove-hook 'change-major-mode-hook #'hl-column--unhighlight t)
    (hl-column--unhighlight)))

;;;###autoload
(define-globalized-minor-mode global-hl-column-mode hl-column-mode
  (lambda () (unless (minibufferp) (hl-column-mode +1))))

(provide 'hl-column)
;;; hl-column.el ends here
