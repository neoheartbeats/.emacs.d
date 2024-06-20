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

;; Fuzzy searching: simple but effective sorting and filtering by `prescient'
;; (use-package prescient
;;   :straight t
;;   :config
;;   (setq prescient-filter-method '(fuzzy prefix initialism))
;;   (prescient-persist-mode 1))

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
  :config
  (setq vertico-count 10)
  (setq vertico-cycle t)

  ;; Load extensions
  (require 'vertico-directory)

  ;; Correct file path when changed
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ;; Do not render italic fonts
  (set-face-attribute 'vertico-group-title nil :slant 'normal)

  (vertico-mode 1))

;; See also `prescient'
;; (use-package vertico-prescient
;;   :straight t
;;   :config
;;   (setq vertico-prescient-enable-sorting t)
;;   (vertico-prescient-mode 1))
;; :bind ((:map vertico-map
;;              ("<tab>" . vertico-insert)
;;              ("<return>" . vertico-directory-enter)
;;              ("<backspace>" . vertico-directory-delete-char))))

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

;; Truncation for long candidates in `vertico' completion
;; (use-package vertico-truncate
;;   :straight (vertico-truncate
;; 	     :type git
;; 	     :host github
;; 	     :repo "jdtsmith/vertico-truncate")
;;   :config (vertico-truncate-mode 1))


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
  (global-set-key [remap project-switch-to-buffer] 'consult-project-buffer)

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

  ;; Previewing files in `find-file'
  (setq read-file-name-function #'consult-find-file-with-preview)

  (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
    (interactive)
    (let ((default-directory (or dir default-directory))
          (minibuffer-completing-file-name t))
      (consult--read #'read-file-name-internal :state (consult--file-preview)
                     :prompt prompt
                     :initial initial
                     :require-match mustmatch
                     :predicate pred)))

  ;; Skipping directories when using `consult-find'
  (setq consult-find-args
	"find . -not ( -wholename */.* -prune -o -name node_modules -prune )")
  
  :bind (:map global-map
	      ("C-s" . consult-line)
	      ("M-s" . consult-ripgrep)
              ("C-v" . consult-yank-from-kill-ring)
              ("M-i" . consult-imenu)))


;; Dabbrev settings
(use-package dabbrev
  :config

  ;; Better letter cases
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
    (my-cape-setup 'cape-dabbrev 'cape-file 'cape-keyword 'cape-abbrev))

  (defun my-cape-emacs-lisp-mode-setup ()
    (my-cape-setup 'cape-dabbrev 'cape-file 'cape-keyword 'cape-elisp-symbol 'cape-abbrev))

  (defun my-cape-org-mode-setup ()
    (my-cape-setup 'cape-dabbrev 'cape-file 'cape-elisp-block 'cape-dict 'cape-keyword))

  :hook ((prog-mode . my-cape-prog-mode-setup)
         (emacs-lisp-mode . my-cape-emacs-lisp-mode-setup)
         (org-mode . my-cape-org-mode-setup)))


;; The main completion frontend by Corfu
(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :init (add-hook 'after-init-hook #'global-corfu-mode)
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

  ;; See also `prescient'
  ;; (use-package corfu-prescient
  ;;   :straight t
  ;;   :config
  ;;   (setq corfu-prescient-enable-sorting t)
  ;;   (corfu-prescient-mode 1))
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
