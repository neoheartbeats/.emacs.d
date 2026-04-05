;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file enhances the editing experience in Emacs.

;;; Code:

(defvar sthenno/delete-current-line-hook nil
  "Hooks run after `sthenno/delete-current-line'.")

(defun sthenno/delete-current-line ()
  "Delete the current line."
  (interactive)
  (delete-region (line-beginning-position) (line-beginning-position 2))
  (run-hooks 'sthenno/delete-current-line-hook))

(defun sthenno/delete-to-beginning-of-line ()
  "Delete from point to the beginning of the current line."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(defun sthenno/enable-fill-column-indicator ()
  "Enable `display-fill-column-indicator-mode' in the current buffer."
  (display-fill-column-indicator-mode 1))

(defun sthenno/enable-line-numbers ()
  "Enable line numbers in the current buffer."
  (display-line-numbers-mode 1))

(defun sthenno/use-external-formatter-p ()
  "Return non-nil when the current major mode uses its own formatter."
  (or (derived-mode-p 'python-base-mode)
      (derived-mode-p 'tex-mode 'LaTeX-mode)))

(defun sthenno/buffer-format ()
  "Indent and clean up the current buffer when no external formatter is active."
  (interactive)
  (unless (sthenno/use-external-formatter-p)
    (save-excursion
      (untabify (point-min) (point-max))
      (indent-region (point-min) (point-max))
      (goto-char (point-min))
      (while (comment-search-forward (point-max) t)
        (comment-indent))
      (delete-trailing-whitespace))))

(defun sthenno/enable-buffer-format-on-save ()
  "Format the current buffer before saving."
  (add-hook 'before-save-hook #'sthenno/buffer-format nil t))

(defun sthenno/enable-indent-bars ()
  "Enable `indent-bars-mode' in the current buffer."
  (indent-bars-mode 1))

(defun sthenno/inhibit-specific-delimiters ()
  "Disable special syntax entries for angle brackets in Org buffers."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(keymap-global-set "C-<backspace>" #'sthenno/delete-current-line)
(keymap-global-set "s-<backspace>" #'sthenno/delete-to-beginning-of-line)
(keymap-global-set "M-<down>" #'forward-paragraph)
(keymap-global-set "M-<up>" #'backward-paragraph)
(keymap-global-set "s-i" #'sthenno/buffer-format)

(setq-default display-line-numbers-width 4)
(add-hook 'prog-mode-hook #'sthenno/enable-fill-column-indicator)
(add-hook 'prog-mode-hook #'sthenno/enable-line-numbers)
(add-hook 'prog-mode-hook #'sthenno/enable-buffer-format-on-save)
(add-hook 'dired-mode-hook #'sthenno/enable-line-numbers)
(add-hook 'org-mode-hook #'sthenno/inhibit-specific-delimiters)

(use-package indent-bars
  :ensure t
  :config
  (setopt indent-bars-no-descend-lists t
          indent-bars-treesit-support t
          indent-bars-treesit-ignore-blank-lines-types '("module")
          indent-bars-prefer-character t
          indent-bars-color '(highlight :face-bg t :blend 0.4)
          indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
          indent-bars-highlight-current-depth '(:blend 0.8)
          indent-bars-starting-column 0
          indent-bars-display-on-blank-lines t)
  (add-hook 'python-base-mode-hook #'sthenno/enable-indent-bars)
  (add-hook 'sh-mode-hook #'sthenno/enable-indent-bars)
  (add-hook 'bash-ts-mode-hook #'sthenno/enable-indent-bars))

(electric-pair-mode 1)
(delete-selection-mode 1)

(use-package iedit
  :ensure t)

(use-package expand-region
  :ensure t
  :config
  (setopt expand-region-smart-cursor t)
  :bind ((:map global-map
               ("S-SPC" . er/expand-region))))

(defun sthenno/convert-image-to-png (file)
  "Convert FILE to PNG format using the macOS `sips' tool."
  (let* ((input (expand-file-name file))
         (output (concat (file-name-sans-extension input) ".png")))
    (if (executable-find "sips")
        (progn
          (shell-command
           (format "sips -s format png %s --out %s"
                   (shell-quote-argument input)
                   (shell-quote-argument output)))
          (message "Converted “%s” to “%s”" input output)
          output)
      (message "Error: “sips” not found on your system! Are you even using macOS?"))))

(defun sthenno/convert-image-to-png-current-buffer ()
  "Convert the image visited by the current buffer to PNG."
  (interactive)
  (let* ((input (buffer-file-name))
         (output (sthenno/convert-image-to-png input)))
    (delete-file (expand-file-name input))
    (find-file (expand-file-name output))))

(provide 'init-editing-utils)
