;; enhance.el --- Credits: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Enhance minibuffer & editors.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion
;;
;; Company setup
(use-package company
  :init (global-company-mode 1)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
	(company-dabbrev-downcase nil)
	(company-dabbrev-ignore-case nil)
  :bind
	(
		(:map company-active-map
			("<escape>" . company-abort)
			("C-p" . company-select-previous)
			("C-n" . company-select-next))
		(:map company-search-map
			("<escape>" . company-search-abort)
			("C-p" . company-select-previous)
			("C-n" . company-select-next))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use listed completion style with `vertico'
(use-package vertico
	:straight (
							:files (:defaults "extensions/*")
							:includes (
													vertico-buffer
													vertico-directory
													vertico-flat
													vertico-indexed
													vertico-mouse
													vertico-quick
													vertico-repeat
													vertico-reverse))
	:init (vertico-mode 1)
	:config
	(use-package vertico-directory
		:bind
		(
			(:map vertico-map
				("RET" . vertico-directory-enter)
				("DEL" . vertico-directory-delete-char)
				("M-DEL" . vertico-directory-delete-word)))
		:hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use orderless completion style
(use-package orderless
  :custom (completion-styles '(orderless)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion for parenthesis
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  (org-mode . smartparens-mode)
  :config
  (show-paren-mode 1)
	(setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Templating system
;;
;; YASnippet for snippets
(use-package yasnippet
  :init (yas-global-mode 1)
  :custom
  (yas-visit-from-menu t)
  (yas-triggers-in-field t)
	(yas-indent-line 'fixed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enhance minibuffer
;;
;; Consult setup
(use-package consult
  :bind
  (
		("C-s" . consult-line)
		("M-s" . consult-ripgrep)
		("s-b" . consult-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window management
;;
;; Ignore buffers start with "*" & "magit:" while moving to previous or next buffer
(set-frame-parameter (selected-frame) 'buffer-predicate
  (lambda (buf)
    (not (string-match-p "^\\(magit:\\|*\\)" (buffer-name buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Excellent git client
(use-package magit
  :custom
  (magit-diff-refine-hunk t)
  (magit-section-visibility-indicator nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Provides only the command "restart-emacs"
(use-package restart-emacs
	:bind ("s-r" . restart-emacs))

(provide 'enhance)
