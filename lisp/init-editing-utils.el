;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;; Highlight parenthesis matched off-screen
(setq blink-matching-paren-highlight-offscreen t)

;; Smart pairing parenthesis
(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'(lambda ()
				(smartparens-strict-mode 1)))
  (add-hook 'org-mode-hook #'(lambda ()
			       (smartparens-mode 1))))


;; Misc settings
(setq undo-limit (* 160000 500)) ; Raise undo-limit to 80 Mb

;; Delete selection if you insert
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :diminish (auto-revert-mode)
  :hook (after-init . global-auto-revert-mode))

;; Using rainbow delimiters [TODO]
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
(setq-default display-line-numbers-width 4)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)


(use-package pulsar
  :straight t
  :config
  (setq pulsar-pulse t
	pulsar-delay 0.1
	pulsar-iterations 15
	pulsar-face 'pulsar-green
	pulsar-highlight-face 'pulsar-magenta)

  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-cyan)
  (add-hook 'after-save-hook #'pulsar-pulse-line-green)
  
  (add-hook 'my/delete-current-line-hook #'pulsar-pulse-line-magenta)
  (add-hook 'my/cycle-to-next-buffer-hook #'pulsar-recenter-top)
  (add-hook 'my/cycle-to-previous-buffer-hook #'pulsar-recenter-top)
  
  (require 'init-comp)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  
  (pulsar-global-mode 1))


(use-package focus
  :straight t
  :config (add-hook 'org-mode-hook #'(lambda ()
				       (focus-mode 1))))


;;; Basics for file formattings [TODO]
;; (defun pp-current-el-buffer ()
;;   "Pretty-print the current buffer as Emacs Lisp code."
;;   (interactive)
;;   (let ((current-buffer-content (buffer-string)))
;;     (with-temp-buffer
;;       (insert current-buffer-content)
;;       (goto-char (point-min))
;;       (let ((pretty-printed (pp-to-string (read (current-buffer)))))
;;         (with-current-buffer (current-buffer)
;;           (erase-buffer)
;;           (insert pretty-printed))))))

;; (global-set-key (kbd "C-c C-p") 'pp-current-el-buffer)


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
  (setq indent-bars-color '(highlight :face-bg t :blend 0.4)
	indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
	indent-bars-highlight-current-depth '(:blend 0.8)
	indent-bars-starting-column 0
	indent-bars-zigzag nil
	indent-bars-display-on-blank-lines t)
  :hook ((python-ts-mode) . indent-bars-mode))

(provide 'init-editing-utils)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
