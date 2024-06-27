;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:
;;

;; Highlight parenthesis matched off-screen
(setq blink-matching-paren-highlight-offscreen t)

;; Smart pairing parenthesis
(use-package smartparens
  :straight t
  :diminish (smartparens-mode)
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'(lambda ()
				                (smartparens-mode 1)))
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
;; Face `fill-column-indicator' is set in `init-gui-frames'
(add-hook 'prog-mode-hook #'(lambda ()
			                  (display-fill-column-indicator-mode 1)))
(add-hook 'org-mode-hook #'(lambda ()
			                 (display-fill-column-indicator-mode 1)))

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
	    pulsar-highlight-face 'pulsar-magenta)

  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-cyan)
  (add-hook 'indent-current-buffer-hook #'pulsar-pulse-line-cyan)
  (add-hook 'after-save-hook #'pulsar-pulse-line-green)
  
  (add-hook 'my/delete-current-line-hook #'pulsar-pulse-line-magenta)
  (add-hook 'my/cycle-to-next-buffer-hook #'pulsar-pulse-line-cyan)
  (add-hook 'my/cycle-to-previous-buffer-hook #'pulsar-pulse-line-cyan)
  
  (require 'init-comp)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  
  (pulsar-global-mode 1))


(use-package focus
  :straight t
  :config
  
  ;; Rendering for comments may not be correct if using the default style
  ;; `defun'. Note this may cause conflict with `highlight-paren-mode'
  ;; and `indent-bars-mode' in most cases [TODO]
  (add-to-list 'focus-mode-to-thing '(python-ts-mode . paragraph))
  :bind (:map global-map
	          ("M-<up>" . focus-prev-thing)
	          ("M-<down>" . focus-next-thing))
  :hook ((python-ts-mode) . focus-mode))


;; [TODO] Integration with `focus-mode'
(use-package indent-bars
  :straight (indent-bars
             :type git
             :host github
             :repo "jdtsmith/indent-bars")
  :config
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
  :hook ((python-ts-mode) . indent-bars-mode))


;; Don't forget about your code comments!
(use-package hl-todo
  :straight t
  :init
  (setq hl-todo-keyword-faces '(("TODO"  . "#6ae4b9")
                                ("FIXME" . "#ff5f59")
                                ("NOTE"  . "#efef80")))
  :config (global-hl-todo-mode 1))

(provide 'init-editing-utils)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
