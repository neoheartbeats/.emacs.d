;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file enhances the editing experience in Emacs.

;;; Code:
;;

;;; Global functions for editing enhancement

(defun sthenno/delete-current-line ()
  "Delete the current line."
  (interactive)
  (delete-region (line-beginning-position) (line-beginning-position 2))
  (run-hooks 'sthenno/delete-current-line-hook))

(defun sthenno/delete-to-beginning-of-line ()
  "Delete from the current position to the beginning of the line."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(keymap-global-set "s-<backspace>" #'sthenno/delete-current-line)
(keymap-global-set "C-<backspace>" #'sthenno/delete-to-beginning-of-line)

(keymap-global-set "M-<down>" #'forward-paragraph)
(keymap-global-set "M-<up>"   #'backward-paragraph)

;;; Indentations

(defun indent-current-buffer ()
  "Indent current buffer.
If `major-mode' is `python-mode', abort."
  (interactive)
  (if (derived-mode-p 'python-mode)
      (message "Indentation does not support for Python.")
    (save-excursion
      (indent-region (point-min) (point-max) nil))))

(defun indent-current-buffer-comment ()
  "Indent comment for current buffer."
  (interactive)
  (let ((lo (point-min))
        (hi (point-max)))
    (save-excursion
      (setq hi (copy-marker hi))
      (goto-char lo)
      (while (< (point) hi)
        (if (comment-search-forward hi t)
            (comment-indent)
          (goto-char hi))))))

(defun untabify-current-buffer ()
  "Convert all tabs to multiple spaces for current buffer."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(defun sthenno/pretty-print-current-buffer ()
  "Pretty-print current buffer."
  (interactive)
  (save-excursion
    (indent-current-buffer)
    (indent-current-buffer-comment)
    (untabify-current-buffer)
    (delete-trailing-whitespace)))
(keymap-set emacs-lisp-mode-map "s-i" #'sthenno/pretty-print-current-buffer)

;; Inhibit passing these delimiters
(defun sthenno/inhibit-specific-delimiters ()
  "Remove the following from current `syntax-table'. This disables syntax highlighting
and auto-paring for such entries."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))
(add-hook 'org-mode-hook #'sthenno/inhibit-specific-delimiters)

;; Automatic pairing parenthesis
(electric-pair-mode 1)

;; Indentations
(use-package aggressive-indent
  :ensure t
  :diminish
  :config (global-aggressive-indent-mode 1))

(use-package indent-bars
  :ensure t
  :config
  (require 'indent-bars-ts)
  (setq indent-bars-no-descend-lists t  ; no extra bars in continued func arg lists
        indent-bars-treesit-support t
        indent-bars-treesit-ignore-blank-lines-types '("module"))

  (setq indent-bars-prefer-character t)

  (setq indent-bars-color '(highlight :face-bg t :blend 0.4)
        indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
        indent-bars-highlight-current-depth '(:blend 0.8)
        indent-bars-starting-column 0
        indent-bars-display-on-blank-lines t))

;;; Highlight these keywords in code comments
(use-package hl-todo
  :ensure t
  :config
  (defun sthenno/hl-todo-faces-setup ()
    (modus-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("TODO"  . ,prose-todo)
              ("FIXME" . ,err)
              ("XXXX*" . ,err)
              ("NOTE"  . ,fg-changed)
              ("HACK"  . ,fg-changed))))
    (global-hl-todo-mode 1))
  (add-hook 'after-init-hook #'sthenno/hl-todo-faces-setup))

;;; Deletions

;; Delete selection if you insert
(delete-selection-mode 1)

;;; Fill columns
(global-display-fill-column-indicator-mode 1)

;;; Display line numbers

(setq-default display-line-numbers-width 4)

(add-hook 'prog-mode-hook  #'display-line-numbers-mode)
(add-hook 'dired-mode-hook #'display-line-numbers-mode)

;;; Edit multiple occurrences in the same way simultaneously using "C-;"
(use-package iedit :ensure t)

;;; expand-region
(use-package expand-region
  :ensure t
  :bind ((:map global-map
               ("C-SPC" . er/expand-region))))

(provide 'init-editing-utils)
