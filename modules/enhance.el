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
;; Auto completion with `corfu'
(use-package corfu
	:straight (:files (:defaults "extensions/*")
                    :includes (corfu-history))
	:custom
	(corfu-cycle t)
	(corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
	(corfu-auto-delay 0)
	(corfu-auto-prefix 2)
	:hook
	((prog-mode . corfu-mode)
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
  :bind
  (:map corfu-map
        ("<tab>" . corfu-next)
        ("<escape>" . corfu-quit)))

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
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use listed completion style with `vertico'
(use-package vertico
	:straight (:files (:defaults "extensions/*")
                    :includes (vertico-directory
                               vertico-mouse))
	:init
  (vertico-mode)

  ;; Persist history over Emacs restarts.
  (use-package savehist
    :init
    (savehist-mode))
  (use-package emacs
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
    ;; Vertico commands are hidden in normal buffers.
    (setq read-extended-command-predicate
          #'command-completion-default-include-p)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t))
	:config
	(use-package vertico-directory
		:bind
		((:map vertico-map
				   ("RET" . vertico-directory-enter)
				   ("DEL" . vertico-directory-delete-char)
				   ("M-DEL" . vertico-directory-delete-word)))
		:hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
  ;; Support for scrolling and candidate selection
  (use-package vertico-mouse
    :init (vertico-mouse-mode)))

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
  (("C-s" . consult-line)
	 ("M-s" . consult-ripgrep)
	 ("s-b" . consult-buffer)))

;; Annotation for minibuffer with `marginalia'
;; (use-package marginalia
;;   :init
;;   (marginalia-mode 1))
;;
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

(provide 'enhance)
