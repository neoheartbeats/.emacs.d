;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Main Emacs configuration.  Personal library code lives in user-lisp/.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'package)
(require 'seq)
(require 'use-package)

(declare-function denote-file-has-denoted-filename-p "denote" (file))
(declare-function denote-journal-new-or-existing-entry "denote-journal"
                  (&optional date))
(declare-function denote-rename-buffer-mode "denote" (&optional arg))
(declare-function gptel-make-openai "gptel" (name &rest args))
(declare-function yas-global-mode "yasnippet" (&optional arg))


;;; Packages

(setopt package-native-compile t
        package-install-upgrade-built-in t
        package-archives '(("gnu-devel" . "https://elpa.gnu.org/devel/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")))

(defconst sthenno/site-lisp-directory
  (locate-user-emacs-file "site-lisp/")
  "Directory for manually managed package checkouts.")

(defun sthenno/site-lisp (directory)
  "Return DIRECTORY inside `sthenno/site-lisp-directory'."
  (expand-file-name directory sthenno/site-lisp-directory))


;;; Core startup and UI state

(setopt save-silently t
        remote-file-name-inhibit-locks t
        backup-inhibited t
        redisplay-skip-fontification-on-input t
        fill-column 88
        mode-line-format ""
        header-line-format ""
        custom-file (locate-user-emacs-file "custom.el")
        user-full-name user-login-name
        user-mail-address "sthenno@sthenno.com"
        inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        inhibit-default-init t
        initial-scratch-message "Hi!")

(scroll-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode -1)

(define-advice display-startup-echo-area-message
    (:override () sthenno-startup-message)
  "Display a custom startup message in the echo area."
  (minibuffer-message
   " %s"
   (propertize "Funding for this program was made possible by viewers like you."
               'face 'default)))


;;; System behavior

(setopt mac-option-modifier 'meta
        mac-command-modifier 'super
        switch-to-prev-buffer-skip 0
        save-place-autosave-interval 300
        recentf-max-saved-items 25
        recentf-autosave-interval 300
        recentf-show-messages nil
        recentf-suppress-open-file-help t
        electric-indent-actions '(yank before-save)
        elisp-fontify-semantically t
        ring-bell-function #'ignore
        use-short-answers t
        use-dialog-box nil
        indent-tabs-mode nil
        insert-directory-program (or (executable-find "gls")
                                     insert-directory-program)
        backward-delete-char-untabify-method 'hungry
        kill-do-not-save-duplicates t
        kill-ring-max 256
        copy-region-blink-delay 0
        copy-region-blink-predicate #'ignore
        sentence-end-double-space nil
        save-interprogram-paste-before-kill t)
(setq-default abbrev-mode t)

(save-place-mode 1)
(savehist-mode 1)
(pixel-scroll-precision-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(delete-selection-mode 1)

(add-hook 'prog-mode-hook #'turn-on-auto-revert-mode)

(keymap-global-set "s-q" #'kill-emacs)
(keymap-global-set "s-w" #'kill-current-buffer)
(keymap-global-set "s-e" #'delete-window)
(keymap-global-set "s-d" #'find-file)
(keymap-global-set "s-<right>" #'switch-to-next-buffer)
(keymap-global-set "s-<left>" #'switch-to-prev-buffer)
(keymap-global-set "<escape>" #'keyboard-escape-quit)
(keymap-global-set "M-<down>" #'forward-paragraph)
(keymap-global-set "M-<up>" #'backward-paragraph)

(defun sthenno/delete-current-line ()
  "Delete the current line."
  (interactive)
  (delete-region (line-beginning-position) (line-beginning-position 2)))

(defun sthenno/delete-to-beginning-of-line ()
  "Delete text from point to the beginning of the current line."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(defun sthenno/lisp-indent-buffer ()
  "Indent the current Lisp buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((inhibit-message t))
        (lisp-indent-region (point-min) (point-max))))))

(keymap-global-set "C-<backspace>" #'sthenno/delete-current-line)
(keymap-global-set "s-<backspace>" #'sthenno/delete-to-beginning-of-line)
(keymap-set emacs-lisp-mode-map "C-c C-c" #'emacs-lisp-native-compile-and-load)
(keymap-set emacs-lisp-mode-map "s-i" #'sthenno/lisp-indent-buffer)

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setopt dired-no-confirm t
          dired-use-ls-dired t
          dired-hide-details-hide-information-lines t
          dired-hide-details-hide-absolute-location t
          dired-check-symlinks nil
          dired-recursive-deletes 'always
          dired-movement-style 'cycle))


;;; Appearance

(use-package modus-themes
  :ensure nil
  :load-path "site-lisp/modus-themes"
  :init
  (add-to-list 'custom-theme-load-path (sthenno/site-lisp "modus-themes"))
  (mapc #'disable-theme custom-enabled-themes)
  :config
  (setopt modus-themes-common-palette-overrides
          `((fg-line-number-active fg-dim)
            (bg-line-number-active bg-hl-line)
            (fg-line-number-inactive "#535353")
            (bg-line-number-inactive unspecified)
            (underline-link border)
            (underline-link-visited border)
            (underline-link-symbolic border)
            (fg-link unspecified)
            (fg-link-visited unspecified)
            (prose-todo info)
            (prose-done "#535353")
            ,@modus-themes-preset-overrides-faint))
  (load-theme 'modus-vivendi :no-confirm))

(set-face-attribute 'default nil :family "Tempestypes" :height 140 :weight 'light)
(set-face-attribute 'region nil :extend nil)
(set-face-attribute 'fill-column-indicator nil :height 0.1)
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'italic nil :slant 'normal)
(set-face-attribute 'show-paren-match nil
                    :foreground "green"
                    :box '(:line-width (-1 . -1) :style released-button))
(set-face-attribute 'mode-line nil
                    :background 'unspecified :foreground "#535353" :box nil
                    :underline t :height 0.1)
(set-face-attribute 'mode-line-active nil
                    :background 'unspecified :foreground "#535353" :box nil
                    :underline t :height 0.1)
(set-face-attribute 'mode-line-inactive nil
                    :background 'unspecified :foreground "#535353" :box nil
                    :underline t :height 0.1)

(let ((font "PingFang SC"))
  (set-fontset-font t 'kana (font-spec :family font))
  (set-fontset-font t 'han (font-spec :family font))
  (set-fontset-font t 'cjk-misc (font-spec :family font)))
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
(set-fontset-font t 'ucs (font-spec :family "SF Pro") nil 'prepend)

(setopt global-hl-line-sticky-flag 'window
        x-stretch-cursor t
        cursor-type '(bar . 1)
        display-line-numbers-widen t
        display-line-numbers-width 6
        show-paren-delay 0.05
        show-paren-when-point-inside-paren t
        show-paren-context-when-offscreen t
        show-paren-not-in-comments-or-strings 'on-mismatch
        default-input-method nil)

(global-hl-line-mode 1)
(global-display-fill-column-indicator-mode 1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)


;;; Org and notes

(use-package org
  :ensure nil
  :init
  (setopt org-directory "/Users/sthenno/uncodified/"
          org-default-notes-file (expand-file-name "notes.org" org-directory)
          org-persist-directory (locate-user-emacs-file "org-persist/")
          org-startup-with-link-previews t
          org-startup-truncated t
          org-use-property-inheritance t
          org-babel-uppercase-example-markers t
          org-hide-emphasis-markers t
          org-hide-macro-markers t
          org-hide-drawer-startup t
          org-special-ctrl-a t
          org-image-align 'left
          org-image-max-width 'fill-column
          org-yank-image-save-method (expand-file-name "images/" org-directory)
          org-attach-method 'cp
          org-return-follows-link t
          org-support-shift-select t
          org-confirm-babel-evaluate nil
          org-src-preserve-indentation t
          org-edit-src-content-indentation 0
          org-edit-src-persistent-message nil
          org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-src-ask-before-returning-to-edit-buffer nil
          org-export-allow-bind-keywords t
          org-babel-update-intermediate t
          org-babel-load-languages '((dot . t)
                                     (emacs-lisp . t)
                                     (python . t)
                                     (shell . t))))

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (setopt org-modern-list '((?- . "•"))
          org-modern-block-fringe 0
          org-modern-todo nil
          org-modern-block-name nil
          org-modern-checkbox '((?X . "􀃰")
                                (?- . "􀃞")
                                (?\s . "􀂒"))
          org-modern-timestamp '(" %Y-%m-%d " . " %H:%M "))
  (set-face-attribute 'org-modern-block-name nil :inherit 'shadow))

(use-package denote
  :ensure nil
  :load-path "site-lisp/denote"
  :config
  (setopt denote-directory org-directory
          denote-file-type 'org
          denote-known-keywords '("stages" "silos" "images" "papers")
          denote-save-buffers t
          denote-kill-buffers t
          denote-open-link-function #'find-file
          denote-org-front-matter "#+TITLE: %1$s\n\n"
          denote-buffer-name-prefix "[uncodified] "
          denote-sort-dired-default-reverse-sort t)
  (denote-rename-buffer-mode 1))

(use-package denote-org
  :ensure nil
  :load-path "site-lisp/denote-org"
  :after denote
  :config
  (setopt denote-org-store-link-to-heading 'context))

(use-package denote-journal
  :ensure nil
  :load-path "site-lisp/denote-journal"
  :after denote
  :config
  (setopt denote-journal-title-format "%e %B %Y"
          denote-journal-directory (expand-file-name "stages/" denote-directory)
          denote-journal-keyword "stages"
          initial-buffer-choice #'denote-journal-new-or-existing-entry))

;;;###autoload
(defun sthenno/denote-org-path-sorted-notes (directory)
  "Return a list of note files in DIRECTORY, sorted by name."
  (sort (seq-filter #'denote-file-has-denoted-filename-p
                    (directory-files directory t "\\.org$"))
        #'string<))

;;;###autoload
(defun sthenno/denote-journal-find-stages-file-date (offset)
  "Open the Denote journal file OFFSET positions away from the current one."
  (let* ((buffer-file (buffer-file-name))
         (sorted-files (sthenno/denote-org-path-sorted-notes
                        denote-journal-directory))
         (current-file-index (cl-position buffer-file sorted-files
                                          :test #'string=)))
    (if (null current-file-index)
        (message "Current file is not a note file.")
      (let ((target-index (+ current-file-index offset)))
        (if (or (< target-index 0)
                (>= target-index (length sorted-files)))
            (message "No Denote note file.")
          (find-file (nth target-index sorted-files)))))))

;;;###autoload
(defun sthenno/denote-journal-entry-previous ()
  "Open the previous journal entry."
  (interactive)
  (sthenno/denote-journal-find-stages-file-date -1))

;;;###autoload
(defun sthenno/denote-journal-entry-next ()
  "Open the next journal entry."
  (interactive)
  (sthenno/denote-journal-find-stages-file-date 1))

(with-eval-after-load 'org
  (keymap-set org-mode-map "s-<up>" #'sthenno/denote-journal-entry-previous)
  (keymap-set org-mode-map "s-<down>" #'sthenno/denote-journal-entry-next))
(keymap-global-set "C-c d" #'denote-journal-new-or-existing-entry)


;;; Projects

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setopt magit-diff-refine-hunk t))

(use-package xref
  :ensure nil
  :bind (:map global-map
              ("M-/" . xref-find-references))
  :init
  (setopt xref-search-program 'ripgrep))


;;; Templates

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode)
  :config
  (setopt yas-triggers-in-field t)
  (yas-global-mode 1))


;;; Completion and minibuffer

(setopt completion-cycle-threshold nil
        text-mode-ispell-word-completion nil
        tab-always-indent 'complete
        echo-keystrokes 0.125
        resize-mini-windows 'grow-only
        help-window-select t
        read-minibuffer-restore-windows nil
        read-extended-command-predicate #'command-completion-default-include-p
        minibuffer-default-prompt-format " [%s]"
        minibuffer-visible-completions nil
        minibuffer-prompt-properties '(read-only t cursor-intangible t
                                                 face minibuffer-prompt)
        crm-prompt (format "%s %%p" (propertize "[%d]" 'face 'shadow))
        completions-sort 'historical
        completion-eager-display nil
        completion-eager-update nil
        completion-styles '(flex basic)
        completion-ignore-case nil
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)

(file-name-shadow-mode 1)

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("<return>" . vertico-directory-enter)
              ("<backspace>" . vertico-directory-delete-char))
  :config
  (setopt vertico-count 12
          vertico-resize t
          vertico-scroll-margin 4
          vertico-cycle nil
          vertico-count-format (cons "[ %-6s ] " "%s of %s"))
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package consult
  :ensure t
  :demand t
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
               ("?" . consult-narrow-help)))
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (setopt register-preview-delay 0.125
          register-preview-function #'consult-register-format
          xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
  :config
  (keymap-substitute project-prefix-map
                     #'project-find-regexp #'consult-ripgrep))

(use-package dabbrev
  :ensure nil
  :hook (emacs-lisp-mode . sthenno/dabbrev-elisp)
  :config
  (setopt dabbrev-case-distinction 'case-replace
          dabbrev-case-replace 'case-replace
          dabbrev-case-fold-search nil
          dabbrev-upcase-means-case-search t)

  (defun sthenno/dabbrev-elisp ()
    "Tune `dabbrev' for `emacs-lisp-mode'."
    (setopt-local dabbrev-case-fold-search nil
                  dabbrev-case-replace nil))

  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (dolist (mode '(authinfo-mode doc-view-mode pdf-view-mode tags-table-mode))
    (add-to-list 'dabbrev-ignored-buffer-modes mode)))

(use-package cape
  :ensure t
  :demand t
  :hook ((emacs-lisp-mode . sthenno/capf-elisp)
         (text-mode . sthenno/capf-text))
  :config
  (setopt cape-dict-case-fold t
          cape-dict-case-replace t
          cape-dict-limit 25)

  (defun sthenno/capf-elisp ()
    "Compose CAPFs for `emacs-lisp-mode'."
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'elisp-completion-at-point
                                       #'cape-dabbrev)
                      #'cape-file))
    (setopt-local cape-dabbrev-min-length 2))

  (defun sthenno/capf-text ()
    "Compose CAPFs for text-oriented buffers."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       (cape-capf-prefix-length #'cape-dict 4)
                       #'cape-dabbrev)
                      #'cape-file))
    (setopt-local cape-dabbrev-min-length 5)))

(use-package corfu
  :ensure t
  :demand t
  :hook ((after-init . global-corfu-mode)
         (eshell-mode . sthenno/corfu-eshell-setup)
         (prog-mode . corfu-popupinfo-mode))
  :bind (:map corfu-map
              ("<down>" . corfu-next)
              ("TAB" . corfu-complete)
              ([tab] . corfu-complete)
              ("<up>" . corfu-previous)
              ("<escape>" . corfu-quit))
  :config
  (setopt corfu-auto t
          corfu-auto-delay 0.025
          corfu-auto-prefix 2
          corfu-count 10
          corfu-scroll-margin 4
          corfu-min-width 20
          corfu-max-width 40
          corfu-separator ?\s
          corfu-preview-current 'insert
          corfu-cycle t
          corfu-popupinfo-delay '(0.025 . 0.05)
          corfu-popupinfo-hide nil
          corfu-popupinfo-max-width 80
          corfu-popupinfo-min-width 20)

  (defun sthenno/corfu-eshell-setup ()
    "Use a more conservative Corfu setup in Eshell."
    (setopt-local corfu-auto nil)
    (corfu-mode 1)
    (keymap-set corfu-map "RET" #'corfu-send))

  (keymap-set corfu-map "RET" #'corfu-insert)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))


;;; Languages

(setopt treesit-font-lock-level 3
        treesit-enabled-modes t
        treesit-auto-install-grammar 'ask)

(use-package eglot
  :ensure nil
  :hook ((LaTeX-mode tex-mode python-base-mode) . eglot-ensure))

(use-package python
  :ensure nil
  :bind (:map python-base-mode-map
              ("s-<up>" . python-nav-beginning-of-block)
              ("s-<down>" . python-nav-end-of-block)
              ("C-x m" . python-nav-if-name-main))
  :init
  (setopt python-indent-offset 4
          python-indent-guess-indent-offset nil
          python-indent-guess-indent-offset-verbose nil))


;;; TeX

;;;###autoload
(defun sthenno/latexindent-buffer ()
  "Format the current TeX buffer with `latexindent' when available."
  (interactive)
  (when (and (executable-find "latexindent")
             (derived-mode-p 'latex-mode 'LaTeX-mode))
    (let ((orig (point)))
      (shell-command-on-region (point-min)
                               (point-max)
                               "latexindent -g /tmp/latexindent.log"
                               (current-buffer) t)
      (goto-char orig))))

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . TeX-PDF-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . sthenno/latexindent-on-save))
  :config
  (setopt TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil))

(defun sthenno/latexindent-on-save ()
  "Format the current LaTeX buffer before saving."
  (add-hook 'before-save-hook #'sthenno/latexindent-buffer nil t))

(use-package reftex
  :ensure nil
  :config
  (setopt reftex-plug-into-AUCTeX t
          reftex-use-multiple-selection-buffers t))


;;; AI and Hermit

(use-package gptel
  :ensure t
  :bind ((:map global-map
               ("s-p" . gptel))
         (:map gptel-mode-map
               ("s-<return>" . gptel-send)))
  :config
  (setopt gptel-default-mode #'org-mode
          gptel-org-branching-context t
          gptel-model 'sthenno
          gptel-backend (gptel-make-openai "local"
                          :protocol "http"
                          :host "192.168.100.207:8000"
                          :endpoint "/v1/chat/completions"
                          :stream t
                          :key "sk-tmp"
                          :models '(sthenno))))

(require 'sthenno-hermit)


;;; Customizations

(when (file-exists-p custom-file)
  (load custom-file :noerror :nomessage))

(provide 'init)
;;; init.el ends here
