;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;
;;
;;; Code:
;;

;; Highlight parenthesis matched off-screen
(setq blink-matching-paren-highlight-offscreen t)

;; Automatic pair parenthesis
(add-hook 'after-init-hook #'(lambda ()
                               (electric-pair-mode 1)))

;; Inhibit paring these delimiters
(add-hook 'after-init-hook #'(lambda ()
                               (modify-syntax-entry ?< ".")))

(use-package paren
  :init (setq show-paren-delay 0.05
              show-paren-highlight-openparen t
              show-paren-when-point-inside-paren t))


(use-package eldoc
  :diminish (eldoc-mode)
  :init (setq eldoc-idle-delay 0))


;; Delete selection if you insert
(use-package delsel
  :init (add-hook 'after-init-hook #'(lambda ()
                                       (delete-selection-mode 1))))

;; Automatically reload files was modified by external program
(use-package autorevert
  :diminish (auto-revert-mode)
  :init (add-hook 'after-init-hook #'(lambda ()
                                       (global-auto-revert-mode 1))))

;; Using rainbow delimiters [TODO]
(use-package rainbow-delimiters
  :straight t
  :diminish (rainbow-delimiters-mode)
  :config (add-hook 'prog-mode-hook #'(lambda ()
                                        (rainbow-delimiters-mode 1))))

;; Fill columns
;;
;; Face `fill-column-indicator' is set in `init-gui-frames'
;;

(add-hook 'after-init-hook #'(lambda ()
                               (global-display-fill-column-indicator-mode 1)))

;; Display line numbers
(setq-default display-line-numbers-width 4)
(add-hook 'prog-mode-hook #'(lambda ()
                              (display-line-numbers-mode 1)))


(use-package pulsar
  :straight t
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.075
        pulsar-iterations 10
        pulsar-face 'pulsar-green
        pulsar-highlight-face 'pulsar-green)

  ;; Hooks
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-cyan)
  (add-hook 'indent-current-buffer-hook #'pulsar-pulse-line-cyan)
  (add-hook 'after-save-hook #'pulsar-pulse-line-green)
  (add-hook 'my/delete-current-line-hook #'pulsar-pulse-line-magenta)
  (add-hook 'my/cycle-to-next-buffer-hook #'pulsar-pulse-line-cyan)
  (add-hook 'my/cycle-to-previous-buffer-hook #'pulsar-pulse-line-cyan)

  (add-hook 'split-window-below-focus-hook #'pulsar-highlight-line)
  (add-hook 'split-window-right-focus-hook #'pulsar-highlight-line)

  (require 'init-comp)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)

  (pulsar-global-mode 1))


(use-package indent-bars
  :straight (indent-bars
             :type git
             :host github
             :repo "jdtsmith/indent-bars")
  :init
  (setq indent-bars-treesit-support t
        indent-bars-treesit-ignore-blank-lines-types '("module"))

  ;; Stipple-based pixle-toggling is not supported by NS built Emacs
  (setq indent-bars-prefer-character t)

  :config
  (setq indent-bars-color '(highlight :face-bg t :blend 0.4)
        indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
        indent-bars-highlight-current-depth '(:blend 0.8)
        indent-bars-starting-column 0
        indent-bars-display-on-blank-lines t)

  (add-hook 'python-ts-mode-hook #'(lambda ()
                                     (indent-bars-mode 1))))


;; Don't forget about your code comments!
(use-package hl-todo
  :straight t
  :init (add-hook 'after-init-hook #'(lambda ()
                                       (modus-themes-with-colors
                                         (setq hl-todo-keyword-faces
                                               `(("TODO"  . ,prose-todo)
                                                 ("FIXME" . ,err)
                                                 ("NOTE"  . ,fg-changed))))))
  :config (global-hl-todo-mode 1))

(provide 'init-editing-utils)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
