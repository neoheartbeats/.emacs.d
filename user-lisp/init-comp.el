;;; init-comp.el --- Completion and minibuffer framework -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file includes:
;;
;; - completion styles enhancement using `orderless'
;; - minibuffer enhancement using `vertico' and `consult'
;; - popup completions by `corfu' as frontend and `cape' as backend

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(setopt completion-cycle-threshold nil
        text-mode-ispell-word-completion nil
        tab-always-indent 'complete
        tab-first-completion nil
        echo-keystrokes 0.05
        resize-mini-windows t
        help-window-select t
        enable-recursive-minibuffers t
        read-minibuffer-restore-windows nil
        read-extended-command-predicate #'command-completion-default-include-p
        minibuffer-default-prompt-format " [%s]"
        minibuffer-visible-completions nil
        minibuffer-prompt-properties '(read-only t cursor-intangible t
                                                 face minibuffer-prompt)
        crm-prompt (format "%s %%p" (propertize "[%d]" 'face 'shadow))
        completions-sort 'historical
        completion-auto-help nil
        completion-show-help nil
        completion-show-inline-help nil)
(file-name-shadow-mode 1)

(use-package orderless
  :ensure t
  :init (setopt completion-styles '(flex basic)
                completion-category-defaults nil
                completion-category-overrides
                '((file (styles
                         (partial-completion
                          ((completion-pcm-leading-wildcard t)))
                         basic))))
  :config
  (defun sthenno/completion-style-corfu ()
    "Use literal-first completion styles for Corfu popups."
    (setq-local completion-styles
                '((orderless
                   ((orderless-style-dispatchers nil)
                    (orderless-matching-styles (orderless-literal))))
                  basic)
                completion-category-overrides nil
                completion-category-defaults nil))

  (setopt orderless-matching-styles
          '(orderless-literal orderless-prefixes)))

(setopt completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)

(use-package vertico
  :ensure t
  :init
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (advice-add #'ffap-menu-ask :around
              (lambda (orig-fun &rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help) #'ignore))
                  (apply orig-fun args))))
  (setq-default completion-in-region-function
                (lambda (&rest args) (apply
                                      (if vertico-mode
                                          #'consult-completion-in-region
                                        #'completion--in-region)
                                      args)))
  :hook (after-init . vertico-mode)
  :config
  (setopt vertico-count 12
          vertico-resize t
          vertico-scroll-margin 4
          vertico-cycle nil
          vertico-count-format (cons "[ %-6s ] " "%s of %s"))
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  :bind ((:map vertico-map
               ("<tab>" . vertico-insert)
               ("<return>" . vertico-directory-enter)
               ("<backspace>" . vertico-directory-delete-char))))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package consult
  :ensure t
  :init (setopt register-preview-delay 0.05
                register-preview-function #'consult-register-format
                xref-show-xrefs-function #'consult-xref
                xref-show-definitions-function #'consult-xref)
  :config
  (keymap-substitute project-prefix-map
                     #'project-find-regexp #'consult-ripgrep)
  (cl-nsubstitute-if '(consult-ripgrep "Find regexp")
                     (pcase-lambda (`(,cmd _))
                       (eq cmd #'project-find-regexp))
                     project-switch-commands)
  :bind ((:map global-map
               ("s-b" . consult-buffer)
               ("C-s" . consult-line)
               ("s-;" . consult-goto-line)
               ("C-v" . consult-yank-pop)
               ("s-m" . consult-imenu-multi)
               ("s-n" . consult-recent-file)
               ("M-i" . consult-info)
               ("M-s" . consult-ripgrep))
         (:map consult-narrow-map
               ("?" . consult-narrow-help))))

(use-package dabbrev
  :config
  (setopt dabbrev-case-distinction 'case-replace
          dabbrev-case-replace 'case-replace
          dabbrev-case-fold-search nil
          dabbrev-upcase-means-case-search t)

  (defun sthenno/dabbrev-elisp ()
    "Tune `dabbrev' for `emacs-lisp-mode'."
    (setq-local dabbrev-case-fold-search nil
                dabbrev-case-replace nil))

  (add-hook 'emacs-lisp-mode-hook #'sthenno/dabbrev-elisp)
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package cape
  :ensure t
  :init
  (setopt cape-dict-case-fold t
          cape-dict-case-replace t
          cape-dict-limit 25)

  (defun sthenno/capf-eglot ()
    "Compose CAPFs for `eglot-managed-mode'."
    (setq-local completion-at-point-functions
                `(,(cape-capf-super
                    (cape-capf-buster #'eglot-completion-at-point)
                    #'cape-dabbrev)
                  cape-file)
                cape-dabbrev-min-length 4))

  (defun sthenno/capf-elisp ()
    "Compose CAPFs for `emacs-lisp-mode'."
    (setq-local completion-at-point-functions
                `(,(cape-capf-super
                    (cape-capf-prefix-length #'cape-dict 4)
                    (cape-capf-predicate
                     #'elisp-completion-at-point
                     (lambda (cand)
                       (or (not (keywordp cand))
                           (eq (char-after (car completion-in-region--data))
                               ?:))))
                    #'cape-dabbrev)
                  cape-file)
                cape-dabbrev-min-length 2))

  (defun sthenno/capf-text ()
    "Compose CAPFs for text-oriented buffers."
    (setq-local completion-at-point-functions
                `(,(cape-capf-super
                    (cape-capf-prefix-length #'cape-dict 4)
                    #'cape-dabbrev)
                  cape-file
                  cape-dabbrev)
                cape-dabbrev-min-length 5))

  (add-hook 'eglot-managed-mode-hook #'sthenno/capf-eglot)
  (add-hook 'emacs-lisp-mode-hook #'sthenno/capf-elisp)
  (add-hook 'text-mode-hook #'sthenno/capf-text))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :config
  (setopt corfu-auto t
          corfu-auto-delay 0.025
          corfu-auto-prefix 2
          corfu-count 5
          corfu-scroll-margin 3
          corfu-min-width 20
          corfu-max-width 40
          corfu-separator ?\s
          corfu-preview-current 'insert
          corfu-cycle t
          corfu-preselect 'directory)

  (defun sthenno/corfu-eshell-setup ()
    "Use a more conservative Corfu setup in Eshell."
    (setq-local corfu-auto nil)
    (corfu-mode 1)
    (keymap-set corfu-map "RET" #'corfu-send))

  (defun sthenno/corfu-combined-sort (candidates)
    "Sort CANDIDATES with display metadata first and Corfu second."
    (let ((candidates
           (if-let* ((display-sort-func
                      (corfu--metadata-get 'display-sort-function)))
               (funcall display-sort-func candidates)
             candidates)))
      (if corfu-sort-function
          (funcall corfu-sort-function candidates)
        candidates)))

  (add-hook 'eshell-mode-hook #'sthenno/corfu-eshell-setup)
  (add-hook 'corfu-mode-hook #'sthenno/completion-style-corfu)
  (add-hook 'prog-mode-hook #'corfu-popupinfo-mode)
  (setopt corfu-sort-override-function #'sthenno/corfu-combined-sort
          corfu-popupinfo-delay '(0.025 . 0.05)
          corfu-popupinfo-hide nil
          corfu-popupinfo-max-width 80
          corfu-popupinfo-min-width 20)
  (keymap-set corfu-map "RET" #'corfu-insert)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  :bind (:map corfu-map
              ("<down>" . corfu-next)
              ("TAB" . corfu-complete)
              ([tab] . corfu-complete)
              ("<up>" . corfu-previous)
              ("<escape>" . corfu-quit)))

(provide 'init-comp)
