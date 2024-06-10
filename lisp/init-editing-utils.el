;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;; Electric parenthesis
(add-hook 'after-init-hook #'electric-pair-mode)

;; Highlight parenthesis matched off-screen
(setq blink-matching-paren-highlight-offscreen t)

;; Misc settings
(setq undo-limit (* 160000 500)) ; Raise undo-limit to 80 Mb

;; Delete selection if you insert
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :diminish (auto-revert-mode)
  :hook (after-init . global-auto-revert-mode))

;; Framework for mode-specific buffer indexes
(use-package imenu
  :bind ("s-m" . imenu))

;; Using rainbow delimiters
(use-package rainbow-delimiters
  :straight t
  :diminish (rainbow-delimiters-mode)
  :config (add-hook 'prog-mode-hook #'(lambda ()
                                        (rainbow-delimiters-mode 1))))

;; Inhibit paring these delimiters
(add-hook 'after-init-hook #'(lambda ()
                               (modify-syntax-entry ?< ".")))

;; Fill columns
(setq display-fill-column-indicator-character ?\u254e)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Display line numbers
(setq-default display-line-numbers-width 5)
(global-display-line-numbers-mode 1)


;;; Basics for file formattings
(defun pp-current-el-buffer ()
  "Pretty-print the current buffer as Emacs Lisp code."
  (interactive)
  (let ((current-buffer-content (buffer-string)))
    (with-temp-buffer
      (insert current-buffer-content)
      (goto-char (point-min))
      (let ((pretty-printed (pp-to-string (read (current-buffer)))))
        (with-current-buffer (current-buffer)
          (erase-buffer)
          (insert pretty-printed))))))

(global-set-key (kbd "C-c C-p") 'pp-current-el-buffer)

;; EditorConfig for Emacs
;; (use-package editorconfig
;;   :straight t
;;   :diminish (editorconfig-mode)
;;   :config (editorconfig-mode 1))


(use-package indent-bars
  :straight (indent-bars
             :type git
             :host github
             :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-prefer-character t)
  :config
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.4)
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   indent-bars-highlight-current-depth '(:blend 0.8)
   indent-bars-starting-column 0
   indent-bars-zigzag nil
   indent-bars-display-on-blank-lines t)

  (defun indent-bars--guess-spacing ()
    "Get indentation spacing of current buffer.
Adapted from `highlight-indentation-mode'."
    (cond
     ((and (derived-mode-p 'python-mode) (boundp 'py-indent-offset))
      py-indent-offset)
     ((and (derived-mode-p 'python-mode) (boundp 'python-indent-offset))
      python-indent-offset)
     ((and (derived-mode-p 'ruby-mode) (boundp 'ruby-indent-level))
      ruby-indent-level)
     ((and (derived-mode-p 'scala-mode) (boundp 'scala-indent:step))
      scala-indent:step)
     ((and (derived-mode-p 'scala-mode) (boundp 'scala-mode-indent:step))
      scala-mode-indent:step)
     ((and (or (derived-mode-p 'scss-mode) (derived-mode-p 'css-mode))
	   (boundp 'css-indent-offset))
      css-indent-offset)
     ((and (derived-mode-p 'nxml-mode) (boundp 'nxml-child-indent))
      nxml-child-indent)
     ((and (derived-mode-p 'coffee-mode) (boundp 'coffee-tab-width))
      coffee-tab-width)
     ((and (derived-mode-p 'js-mode) (boundp 'js-indent-level))
      js-indent-level)
     ((and (derived-mode-p 'js2-mode) (boundp 'js2-basic-offset))
      js2-basic-offset)
     ((and (derived-mode-p 'sws-mode) (boundp 'sws-tab-width))
      sws-tab-width)
     ((and (derived-mode-p 'web-mode) (boundp 'web-mode-markup-indent-offset))
      web-mode-markup-indent-offset)
     ((and (derived-mode-p 'web-mode) (boundp 'web-mode-html-offset)) ; old var
      web-mode-html-offset)
     ((and (local-variable-p 'c-basic-offset) (numberp c-basic-offset))
      c-basic-offset)
     ((and (derived-mode-p 'yaml-mode) (boundp 'yaml-indent-offset))
      yaml-indent-offset)
     ((and (derived-mode-p 'elixir-mode) (boundp 'elixir-smie-indent-basic))
      elixir-smie-indent-basic)
     ((and (derived-mode-p 'lisp-data-mode) (boundp 'lisp-body-indent))
      lisp-body-indent)
     ((and (derived-mode-p 'cobol-mode) (boundp 'cobol-tab-width))
      cobol-tab-width)
     ((or (derived-mode-p 'go-ts-mode) (derived-mode-p 'go-mode))
      tab-width)
     ((derived-mode-p 'nix-mode)
      tab-width)
     ((and (derived-mode-p 'nix-ts-mode) (boundp 'nix-ts-mode-indent-offset))
      nix-ts-mode-indent-offset)
     ((and (derived-mode-p 'json-ts-mode) (boundp 'json-ts-mode-indent-offset))
      json-ts-mode-indent-offset)
     ((and (derived-mode-p 'json-mode) (boundp 'js-indent-level))
      js-indent-level)
     ((and (boundp 'standard-indent) standard-indent))
     ((and (derived-mode-p 'org-mode) (boundp 'org-list-indent-offset))
      org-list-indent-offset)
     (t 4)))
  :hook ((python-ts-mode org-mode) . indent-bars-mode))

(provide 'init-editing-utils)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
