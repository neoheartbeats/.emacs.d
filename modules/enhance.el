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
;; (use-package company
;;   :init (global-company-mode 1)
;;   :custom
;;   (company-minimum-prefix-length 2)
;;   (company-idle-delay 0)
;;   (company-echo-delay 0)
;;   (company-selection-wrap-around t)
;;   (company-tooltip-align-annotations t)
;; 	(company-dabbrev-downcase nil)
;; 	(company-dabbrev-ignore-case nil)
;; 	:config
;; 	(defun ispell-completion () ;; Spelling completion
;; 		(require 'ispell)
;; 		(make-local-variable 'company-backends)
;; 		(setq company-backends '((
;; 															 company-dabbrev
;; 															 company-yasnippet
;; 															 company-ispell)))
;; 		(setq company-ispell-dictionary ispell-dictionary)
;; 		(company-capf
;; 			'((:separate
;; 					company-dabbrev
;; 					company-yasnippet
;; 					company-ispell))))
;;   :bind
;; 	(
;; 		(:map company-active-map
;; 			("<escape>" . company-abort)
;; 			("C-p" . company-select-previous)
;; 			("C-n" . company-select-next))
;; 		(:map company-search-map
;; 			("<escape>" . company-search-abort)
;; 			("C-p" . company-select-previous)
;; 			("C-n" . company-select-next)))
;; 	:hook
;; 	(org-mode . ispell-completion))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Auto completion with `corfu'
(use-package corfu
	:straight (
							:files (:defaults "extensions/*")
							:includes (corfu-history))
	:custom
	(corfu-cycle t)
	(corfu-auto t)
	(corfu-auto-delay 0)
	(corfu-auto-prefix 2)
	:hook
	(
		(prog-mode . corfu-mode)
		(org-mode . corfu-mode))
  :init
  (global-corfu-mode)
	:config
	(use-package corfu-history
		:init (corfu-history-mode))

	;; Icon support
	(use-package kind-icon
		:custom
		(kind-icon-default-face 'corfu-default)
		:config
		(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
	(use-package emacs
		:init
		(setq completion-cycle-threshold 3)
		(setq tab-always-indent 'complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion At Point Extensions made for `corfu'
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
	(add-to-list 'completion-at-point-functions #'cape-ispell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use the `orderless' completion style
(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '(
																				 (file (styles . (partial-completion))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use listed completion style with `vertico'
(use-package vertico
	:straight (
							:files (:defaults "extensions/*")
							:includes (vertico-directory))
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

;; Switching windows
(use-package ace-window
	:init
	(global-set-key (kbd "s-<return>") 'ace-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Excellent git client
(use-package magit
  :custom
  (magit-diff-refine-hunk t)
  (magit-section-visibility-indicator nil))

(provide 'enhance)
