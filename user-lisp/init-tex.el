;;; init-tex.el --- AUCTeX and PDF tooling -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the TeX, LaTeX, and PDF workflow setup.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Formatters
;;

;;;###autoload
(defun sthenno/latexindent-buffer ()
  "Format the current TeX buffer with `latexindent' when available."
  (interactive)
  (when (and (executable-find "latexindent")
             (derived-mode-p 'latex-mode 'LaTeX-mode))
    (let ((orig (point)))
      (shell-command-on-region (point-min)
                               (point-max)
                               "latexindent -g /tmp/latexindent.log"
                               (current-buffer) t)
      (goto-char orig))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; AUCTeX
;;

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . TeX-PDF-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . (lambda ()
                         (add-hook 'before-save-hook #'sthenno/latexindent-buffer
                                   nil t))))
  :config (setopt TeX-auto-save t
                  TeX-parse-self t
                  TeX-save-query nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; RefTeX
;;

(use-package reftex
  :ensure nil
  :config (setopt reftex-plug-into-AUCTeX t
                  reftex-use-multiple-selection-buffers t))

(provide 'init-tex)
