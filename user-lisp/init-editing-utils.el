;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file enhances the editing experience in Emacs.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Basics
;;

(electric-pair-mode 1)
(delete-selection-mode 1)

(defun sthenno/delete-current-line ()
  (interactive)
  (delete-region (line-beginning-position)
                 (line-beginning-position 2)))

(defun sthenno/delete-to-beginning-of-line ()
  (interactive)
  (delete-region (line-beginning-position) (point)))

(keymap-global-set "C-<backspace>" #'sthenno/delete-current-line)
(keymap-global-set "s-<backspace>" #'sthenno/delete-to-beginning-of-line)
(keymap-global-set "M-<down>" #'forward-paragraph)
(keymap-global-set "M-<up>" #'backward-paragraph)

(defun sthenno/org-inhibit-specific-delimiters ()
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))
(add-hook 'org-mode-hook #'sthenno/org-inhibit-specific-delimiters)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; TODO: Outlines
;;

;; (use-package outline
;;   :ensure nil
;;   :config (setopt outline-minor-mode-use-buttons 'in-margins))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Indentation handlers
;;

(defun sthenno/lisp-indent-buffer ()
  (interactive)
  (save-excursion
    (let ((start (point-min))
          (end   (point-max))
          (inhibit-message t))
      (untabify start end)
      (lisp-indent-region start end))
    (delete-trailing-whitespace-if-possible)))
(keymap-set emacs-lisp-mode-map "s-i" #'sthenno/lisp-indent-buffer)

;; FIXME: An additional handler is expected for `lisp-mode'
(add-hook 'before-save-hook #'sthenno/lisp-indent-buffer nil t)

(use-package indent-bars
  :ensure t
  :hook ((python-base-mode sh-base-mode) . indent-bars-mode)
  :config (setopt indent-bars-no-descend-lists t
                  indent-bars-treesit-support t
                  indent-bars-treesit-ignore-blank-lines-types '("module")
                  indent-bars-prefer-character t
                  indent-bars-color '(highlight :face-bg t :blend 0.4)
                  indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
                  indent-bars-highlight-current-depth '(:blend 0.8)
                  indent-bars-starting-column 0
                  indent-bars-display-on-blank-lines t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Indentation handlers
;;

;; TODO: Maybe I need to find some alternatives for the following packages
(use-package iedit :ensure t)
(use-package expand-region
  :ensure t
  :config (setopt expand-region-smart-cursor t)
  :bind* ((:map global-map
                ("S-SPC" . er/expand-region))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; TODO: Do I really need this?
;;

;; (defun sthenno/convert-image-to-png (file)
;;   "Convert FILE to PNG format using the macOS `sips' tool."
;;   (let* ((input (expand-file-name file))
;;          (output (concat (file-name-sans-extension input) ".png")))
;;     (if (executable-find "sips")
;;         (progn
;;           (shell-command
;;            (format "sips -s format png %s --out %s"
;;                    (shell-quote-argument input)
;;                    (shell-quote-argument output)))
;;           (message "Converted “%s” to “%s”" input output)
;;           output)
;;       (message "Error: \"sips\" not found on your system!"))))

;; (defun sthenno/convert-image-to-png-current-buffer ()
;;   "Convert the image visited by the current buffer to PNG."
;;   (interactive)
;;   (let* ((input (buffer-file-name))
;;          (output (sthenno/convert-image-to-png input)))
;;     (delete-file (expand-file-name input))
;;     (find-file (expand-file-name output))))

(provide 'init-editing-utils)
