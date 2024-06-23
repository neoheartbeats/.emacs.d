;;; init-comp.el --- Modern completion system -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

;;; Build the completion framework
;;
;; Before we start
(use-package emacs
  :init
  
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Only list the commands of the current modes
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; Emacs 30: `cape-dict' is used instead
  (setq text-mode-ispell-word-completion nil)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Use the `orderless' completion style
(use-package orderless
  :straight t)

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion)))
      orderless-component-separator #'orderless-escapable-split-on-space)

;; Ignore cases
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)


;; Completion for minibuffers
(use-package vertico
  :straight t
  :init
  (add-hook 'after-init-hook #'(lambda ()
   				 (vertico-mode 1)))
  :config
  (setq vertico-count 10)
  (setq vertico-cycle t)

  ;; Load extensions
  (require 'vertico-multiform) ; This is required by `embark' configurations
  (require 'vertico-directory)

  ;; Correct file path when changed
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ;; Do not render italic fonts
  (set-face-attribute 'vertico-group-title nil :slant 'normal)

  ;; Cut long candidates in `vertico' completion
  (use-package vertico-truncate
    :straight (vertico-truncate
	       :type git
	       :host github
	       :repo "jdtsmith/vertico-truncate")
    :config (vertico-truncate-mode 1))
  
  :bind ((:map vertico-map
               ("<tab>" . vertico-insert)
               ("<return>" . vertico-directory-enter)
               ("<backspace>" . vertico-directory-delete-char))))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Support opening new minibuffers from inside existing minibuffers.
(setq enable-recursive-minibuffers t)

;; Disable showing the *Completions* buffer that conflicts with vertico
;; if using `ffap-menu'
(advice-add #'ffap-menu-ask :around
            (lambda (&rest args)
              (cl-letf (((symbol-function #'minibuffer-completion-help)
                         #'ignore))
                (apply args))))


;; Rich annotations for minibuffer
(use-package marginalia
  :straight t
  :init (marginalia-mode 1))


;; Consult is useful previewing current content in buffer
(use-package consult
  :straight t
  :init
  (global-set-key (kbd "s-b") 'switch-to-buffer)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key
   [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key
   [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  
  ;; Framework for mode-specific buffer indexes
  (global-set-key [remap imenu] 'consult-imenu)
  :config

  ;; Back to last visited by C-s C-s if using `consult-line'
  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))
  (consult-customize consult-line :keymap my-consult-line-map)

  ;; https://github.com/minad/consult/wiki#add-category-specific-minibuffer-keybindings
  (defun define-minibuffer-key (key &rest defs)
    "Define KEY conditionally in the minibuffer.
DEFS is a plist associating completion categories to commands."
    (define-key minibuffer-local-map key
		(list 'menu-item nil defs :filter
		      (lambda (d)
			(plist-get d (completion-metadata-get
				      (completion-metadata (minibuffer-contents)
							   minibuffer-completion-table
							   minibuffer-completion-predicate)
				      'category))))))

  (define-minibuffer-key "\C-s"
			 'consult-location #'previous-history-element
			 'file #'consult-find-for-minibuffer)

  ;; Only display normal buffers using `consult-buffer'
  (dolist (src consult-buffer-sources)
    (unless (eq src 'consult--source-buffer)
      (set src (plist-put (symbol-value src) :hidden t))))

  :bind (:map global-map
	      ("C-s" . consult-line)
	      ("M-s" . consult-ripgrep)
              ("C-v" . consult-yank-from-kill-ring)
              ("s-m" . consult-imenu)))

;;; Embark: Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
  :straight t
  :init
  
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  (global-set-key [remap describe-bindings] 'embark-bindings)

  (setq embark-indicators
        '(embark-minimal-indicator  ; Default is `embark-mixed-indicator'
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  ;; A `which-key' styled integration for `embark' keys
  (require 'vertico-multiform)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (vertico-multiform-mode 1)

  ;; Quitting the minibuffer after an action
  (setq embark-quit-after-action '((kill-buffer . t) (t . nil)))
  
  ;; Hide the mode line of the Embark completion buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind ("s-/" . embark-act))

(use-package embark-consult
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;; Dabbrev settings
(use-package dabbrev
  :config

  ;; Better letter casesx
  (setq dabbrev-case-distinction nil
	dabbrev-case-replace nil
	dabbrev-case-fold-search t
	dabbrev-upcase-means-case-search t)

  ;; See https://github.com/minad/corfu
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; Add extensions for the completion backend
(use-package cape
  :straight t
  :config
  (setq cape-dabbrev-min-length 4)

  (defun my-cape-setup (&rest capes)
    "Add CAPES to `completion-at-point-functions'."
    (dolist (cape capes)
      (add-to-list 'completion-at-point-functions cape)))

  (defun my-cape-prog-mode-setup ()
    (my-cape-setup 'cape-dabbrev
		   'cape-file
		   'cape-keyword))

  (defun my-cape-emacs-lisp-mode-setup ()
    (my-cape-setup 'cape-dabbrev
		   'cape-file
		   'cape-keyword
		   'cape-elisp-symbol))

  (defun my-cape-org-mode-setup ()
    (my-cape-setup 'cape-dabbrev
		   'cape-file
		   'cape-dict))

  :hook ((prog-mode . my-cape-prog-mode-setup)
         (emacs-lisp-mode . my-cape-emacs-lisp-mode-setup)
         (org-mode . my-cape-org-mode-setup)))


;; The main completion frontend by Corfu
(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :init (add-hook 'after-init-hook #'(lambda ()
				       (global-corfu-mode 1)))
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.02) ; Making this to 0 is too expensive
  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t)
  (setq corfu-quit-at-boundary 'separator)
  (setq corfu-quit-no-match t)
  (setq corfu-preview-current nil)
  (setq corfu-preselect 'directory) ; Auto select the first except directories

  ;; Maintain a list of recently selected candidates
  ;; This requires `savehist-mode' is enabled
  (require 'corfu-history)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  
  :bind (:map corfu-map
              ("<down>" . corfu-next)
	      ("<tab>" . corfu-next)
              ("<up>" . corfu-previous)
	      ("s-<tab>" . corfu-previous)
              ("<escape>" . corfu-quit)))

(provide 'init-comp)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
