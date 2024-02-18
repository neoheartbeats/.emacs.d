;;; init-comp.el --- Modern completion system -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;
;; Completion for minibuffers
;;
(use-package vertico
  :straight t
  :init (vertico-mode 1)
  :config
  (setq vertico-count 10)
  (setq vertico-cycle t)
  
  ;; Load extensions
  (require 'vertico-directory)

  ;; Correct file path when changed
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ;; Do not render italic fonts
  (set-face-attribute 'vertico-group-title nil :slant 'normal)
  :bind ((:map vertico-map
               ("<tab>" . vertico-insert)
               ("<return>" . vertico-directory-enter)
               ("<backspace>" . vertico-directory-delete-char))))

;; Rich annotations for minibuffer
(use-package marginalia
  :straight t
  :init (marginalia-mode 1))

;;
;; Consult is useful previewing current content in buffer
;;
(use-package consult
  :straight t
  :init
  (global-set-key (kbd "s-b") 'switch-to-buffer)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key
   [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key
   [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap project-switch-to-buffer] 'consult-project-buffer)
  (global-set-key [remap goto-line] 'consult-goto-line)
  (global-set-key [remap imenu] 'consult-imenu)
  :bind (("C-s" . consult-line)
         ("C-v" . consult-yank-from-kill-ring)
         ("M-s" . consult-ripgrep)
         ("s-o" . consult-outline)
         ("s-m" . consult-imenu)
         ("s-k" . consult-recent-file)))

;;
;; Completion in buffers
;;

(setq tab-always-indent 'complete)

;; Dabbrev settings
(use-package dabbrev
  :commands (dabbrev-expand dabbrev-completion)
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines nil)
  (setq dabbrev-upcase-means-case-search t))

;; Add extensions for the completion backend
(use-package cape
  :straight t
  :config (setq cape-dabbrev-min-length 3)
  :hook ((prog-mode . (lambda ()
                        (push 'cape-dabbrev completion-at-point-functions)
                        (push 'cape-dict completion-at-point-functions)
                        (push 'cape-file completion-at-point-functions)
                        (push 'cape-keyword completion-at-point-functions)
                        (push 'cape-abbrev completion-at-point-functions)))
         (emacs-lisp-mode . (lambda ()
                              (push 'cape-dabbrev completion-at-point-functions)
                              (push 'cape-dict completion-at-point-functions)
                              (push 'cape-file completion-at-point-functions)
                              (push 'cape-keyword completion-at-point-functions)
                              (push 'cape-elisp-symbol completion-at-point-functions)
                              (push 'cape-abbrev completion-at-point-functions)))
         (org-mode . (lambda ()
                       (push 'cape-dabbrev completion-at-point-functions)
                       (push 'cape-dict completion-at-point-functions)
                       (push 'cape-file completion-at-point-functions)
                       (push 'cape-elisp-block completion-at-point-functions)
                       (push 'cape-abbrev completion-at-point-functions)))))

;; Build the completion framework
(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion))))

  ;; Ignore cases
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t))

;;
;; The main completion frontend by Corfu
;;
(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :init (add-hook 'after-init-hook #'global-corfu-mode)
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.0)
  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t)
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match 'separator)
  (setq corfu-preselect 'first)
  (setq corfu-scroll-margin 5)
  (setq corfu-history-mode t)
  (setq corfu-popupinfo-mode t)
  :hook (eshell-mode . (lambda ()
                         (setq-local corfu-auto nil)))
  :bind (:map corfu-map
              ("<down>" . corfu-next)
              ("<up>" . corfu-previous)
              ("<space>" . corfu-quit)
              ("<escape>" . corfu-quit)))

(use-package corfu-prescient
  :straight t
  :config (corfu-prescient-mode 1))

(provide 'init-comp)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
