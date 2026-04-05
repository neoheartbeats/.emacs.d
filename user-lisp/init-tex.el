;;; init-tex.el --- AUCTeX and PDF tooling -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the TeX, LaTeX, and PDF workflow setup.

;;; Code:

(defun sthenno/latexindent-buffer ()
  "Format the current TeX buffer with `latexindent' when available."
  (interactive)
  (when (and (executable-find "latexindent")
             (derived-mode-p 'latex-mode 'LaTeX-mode))
    (let ((origin (point)))
      (shell-command-on-region
       (point-min) (point-max)
       "latexindent -g /tmp/latexindent.log"
       (current-buffer) t)
      (goto-char origin))))

(use-package tex
  :ensure auctex
  :defer t
  :hook ((LaTeX-mode . TeX-PDF-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . (lambda ()
                         (setq-local compilation-scroll-output t
                                     TeX-command-extra-options
                                     "-interaction=nonstopmode -synctex=1 -file-line-error")
                         (add-hook 'before-save-hook #'sthenno/latexindent-buffer
                                   nil t))))
  :config
  (setopt TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-source-correlate-start-server t
          TeX-source-correlate-method 'synctex
          TeX-command-default "LateXMk"
          TeX-view-program-selection '((output-pdf "PDF Tools"))
          TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))))

(use-package reftex
  :defer t
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-use-multiple-selection-buffers t))

;;; [TODO]
(use-package pdf-tools
  :disabled t
  :ensure t
  :defer t
  :commands (pdf-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (use-package pdf-sync
    :after pdf-tools)
  (setopt pdf-view-display-size 'fit-width
          pdf-view-use-scaling t
          pdf-view-use-imagemagick nil))

(provide 'init-tex)
