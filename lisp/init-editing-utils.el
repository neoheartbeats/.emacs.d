;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

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

(keymap-global-set "C-<backspace>" #'sthenno/delete-current-line)
(keymap-global-set "s-<backspace>" #'sthenno/delete-to-beginning-of-line)

(keymap-global-set "M-<down>" #'forward-paragraph)
(keymap-global-set "M-<up>"   #'backward-paragraph)

;; Fill columns
(global-display-fill-column-indicator-mode 1)

;; Display line numbers
(setq-default display-line-numbers-width 4)
(add-hook 'prog-mode-hook  #'display-line-numbers-mode)
(add-hook 'dired-mode-hook #'display-line-numbers-mode)

;; Indentations
;;
(defun sthenno/buffer-format ()
  "Format the current buffer using indentation."
  (interactive)

  ;; Using different formatters based on the major-mode
  (cond ((or (derived-mode-p 'python-mode)
             (derived-mode-p 'python-ts-mode)))
        (t
         (save-excursion
           ;; Convert tabs to spaces first, so indentation doesn’t introduce them
           (untabify (point-min) (point-max))
           (indent-region (point-min) (point-max))

           ;; Indent only comment lines in a single pass
           (goto-char (point-min))
           (while (comment-search-forward (point-max) t)
             (comment-indent))

           ;; Remove trailing whitespaces
           (delete-trailing-whitespace)
           (save-buffer)))))

;; Bind it to a convenient key in prog-mode-map
(keymap-global-set "s-i" #'sthenno/buffer-format)

;; Run the formatter automatically on save
(defun sthenno/buffer-format-on-save ()
  "Format buffer on save."
  (add-hook 'after-save-hook #'sthenno/buffer-format nil t))
(add-hook 'prog-mode-hook #'sthenno/buffer-format-on-save)

;; Indentation highlights
(use-package indent-bars
  :ensure t
  :config
  (setq indent-bars-no-descend-lists t  ; no extra bars in continued func arg lists
        indent-bars-treesit-support t
        indent-bars-treesit-ignore-blank-lines-types '("module"))
  (setq indent-bars-prefer-character t)
  (setq indent-bars-color '(highlight :face-bg t :blend 0.4)
        indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
        indent-bars-highlight-current-depth '(:blend 0.8)
        indent-bars-starting-column 0
        indent-bars-display-on-blank-lines t)

  ;; Hooks
  (add-hook 'python-ts-mode-hook  #'(lambda ()
                                      (indent-bars-mode 1)))
  (add-hook 'bash-ts-mode-hook    #'(lambda ()
                                      (indent-bars-mode 1))))

;;; Inhibit passing these delimiters
(defun sthenno/inhibit-specific-delimiters ()
  "Remove some specific delimiters from current `syntax-table'.
This disables syntax highlighting and auto-paring for such entries."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))
(add-hook 'org-mode-hook #'sthenno/inhibit-specific-delimiters)

;; Automatic pairing parenthesis
(electric-pair-mode 1)

;; Delete selection if you insert
(delete-selection-mode 1)

;; Edit multiple occurrences in the same way simultaneously using "C-;"
(use-package iedit :ensure t)

;; Expand region, use it with shift-selection
(use-package expand-region
  :ensure t
  :config (setq expand-region-smart-cursor t)
  :bind ((:map global-map
               ("S-SPC" . er/expand-region))))

(provide 'init-editing-utils)

;;; init-editing-utils.el ends here
