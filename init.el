;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Main Emacs configuration. Personal library code lives in user-lisp/.

;;; Code:

(setq custom-file (make-temp-file ".emacs-custom"))


;;; Core startup and UI state
(setopt save-silently t
        remote-file-name-inhibit-locks t
        backup-inhibited t
        redisplay-skip-fontification-on-input t
        fill-column 100
        mode-line-format ""
        header-line-format ""
        user-full-name user-login-name
        user-mail-address "sthenno@sthenno.com")
(setopt inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        inhibit-default-init t
        initial-scratch-message "")
(setopt menu-bar-mode nil
        scroll-bar-mode nil
        tool-bar-mode nil
        line-number-mode nil
        column-number-mode nil)

(define-advice display-startup-echo-area-message
    (:override () sthenno-startup-message)
  "Display a custom startup message in the echo area."
  (message "Funding for this program was made possible by viewers like you."))


;;; System essentials
(setopt mac-option-modifier 'meta
        mac-command-modifier 'super)
(setopt switch-to-prev-buffer-skip 0
        save-place-autosave-interval 300
        elisp-fontify-semantically t
        ring-bell-function #'ignore
        use-short-answers t
        use-dialog-box nil
        indent-tabs-mode nil
        insert-directory-program (or (executable-find "gls")
                                     insert-directory-program)
        backward-delete-char-untabify-method 'hungry
        kill-do-not-save-duplicates t
        copy-region-blink-predicate #'ignore
        sentence-end-double-space nil
        save-interprogram-paste-before-kill t
        global-auto-revert-mode t)

(require 'savehist)
(setopt save-place-mode t
        savehist-mode t
        electric-pair-mode t
        electric-indent-mode t
        delete-selection-mode t)

(keymap-global-set "s-q" #'kill-emacs)
(keymap-global-set "s-w" #'kill-current-buffer)
(keymap-global-set "s-e" #'delete-window)
(keymap-global-set "s-d" #'find-file)
(keymap-global-set "s-<right>" #'switch-to-next-buffer)
(keymap-global-set "s-<left>" #'switch-to-prev-buffer)
(keymap-global-set "<escape>" #'keyboard-escape-quit)
(keymap-global-set "M-<down>" #'forward-paragraph)
(keymap-global-set "M-<up>" #'backward-paragraph)

(defun sthenno/delete-to-beginning-of-line ()
  "Delete text from point to the beginning of the current line."
  (interactive)
  (delete-region (line-beginning-position) (point)))
(keymap-global-set "s-<backspace>" #'sthenno/delete-to-beginning-of-line)

(defun sthenno/lisp-indent-buffer ()
  "Indent the current Lisp buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((inhibit-message t))
        (lisp-indent-region (point-min) (point-max))))))
(keymap-set emacs-lisp-mode-map "s-i" #'sthenno/lisp-indent-buffer)
(keymap-set emacs-lisp-mode-map "C-c C-c" #'emacs-lisp-byte-compile-and-load)
(keymap-set emacs-lisp-mode-map "s-k" #'kill-sexp)
(keymap-set emacs-lisp-mode-map "M-<backspace>" #'backward-kill-sexp)

;;; Dired
(with-eval-after-load 'dired
  (setopt dired-no-confirm t
          dired-use-ls-dired t
          dired-hide-details-hide-information-lines t
          dired-hide-details-hide-absolute-location t
          dired-check-symlinks nil
          dired-recursive-deletes 'always
          dired-movement-style 'cycle))

;;; Scrolling
(setopt scroll-margin 0)
(require 'ultra-scroll)
(setopt ultra-scroll-mode t)


;;; Appearance
(require-theme 'modus-themes)
;; (setopt modus-themes-common-palette-overrides '((fg-line-number-active fg-dim)
;;                                                 (bg-line-number-active bg-hl-line)
;;                                                 (fg-line-number-inactive "#535353")
;;                                                 (bg-line-number-inactive unspecified)
;;                                                 (fg-paren-match unspecified)
;;                                                 (bg-paren-match unspecified)))
(load-theme 'modus-vivendi :no-confirm)

(set-face-attribute 'default nil :family "Tempestypes" :height 140)
(set-face-attribute 'region nil :extend t :foreground 'unspecified)
(set-face-attribute 'fill-column-indicator nil :height 0.1)
(set-face-attribute 'show-paren-match nil
                    :background 'unspecified :foreground "green" :box '(:line-width (-1 . -1)))
;; (set-face-attribute 'line-number nil :font (font-spec :family "Noto Serif CJK SC"))
;; (set-face-attribute 'line-number-current-line nil :font (font-spec :family "Noto Serif CJK SC"))

(dolist (face '(mode-line mode-line-active mode-line-inactive))
  (set-face-attribute face nil
                      :background 'unspecified :foreground "#535353" :box nil
                      :underline t :height 0.1))

(let ((font "Noto Serif CJK SC"))
  (dolist (charset '(kana han cjk-misc))
    (set-fontset-font t charset (font-spec :family font))))
;; (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
;; (set-fontset-font t 'ucs (font-spec :family "SF Pro") nil 'prepend)

(setopt global-hl-line-sticky-flag nil
        global-hl-line-mode t
        global-display-fill-column-indicator-mode t
        global-display-line-numbers-mode t
        x-stretch-cursor t
        cursor-type '(bar . 1)
        display-line-numbers-widen t
        display-line-numbers-width 4
        display-fill-column-indicator-warning t
        show-paren-delay 0.0125
        show-paren-context-when-offscreen t
        show-paren-mode t
        show-paren-not-in-comments-or-strings 'on-mismatch
        default-input-method nil
        blink-cursor-mode nil)


;;; Org and notes
(require 'org)

(setopt org-directory "/Users/sthenno/Developer/op1/"
        org-persist-directory (locate-user-emacs-file "org-persist/")
        org-ellipsis ""
        org-startup-with-link-previews t
        org-link-elisp-confirm-function nil
        org-startup-truncated t
        org-use-property-inheritance t
        org-babel-uppercase-example-markers t
        org-hide-emphasis-markers t
        org-hide-macro-markers t
        org-hide-drawer-startup t
        org-special-ctrl-a t
        org-image-align 'left
        org-image-max-width 0.60
        org-attach-method 'cp
        org-return-follows-link t
        org-support-shift-select t
        org-confirm-babel-evaluate nil
        org-src-preserve-indentation t
        org-src-content-indentation 0
        org-edit-src-persistent-message nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-ask-before-returning-to-edit-buffer nil
        org-babel-update-intermediate t
        org-babel-load-languages '((emacs-lisp . t)
                                   (python . t)
                                   (shell . t)))

(require 'denote)
(setopt denote-directory org-directory
        denote-file-type 'org
        denote-known-keywords '("stages" "silos" "images" "papers")
        denote-save-buffers t
        denote-kill-buffers t
        denote-open-link-function #'find-file
        denote-org-front-matter "#+TITLE: %1$s\n\n"
        denote-buffer-name-prefix "[uncodified] "
        denote-sort-dired-default-reverse-sort t
        denote-rename-buffer-mode t)

(require 'denote-org)
(setopt denote-org-store-link-to-heading 'context)

(require 'denote-journal)
(setopt denote-journal-title-format "%e %B %Y"
        denote-journal-directory (file-name-concat denote-directory "stages")
        denote-journal-keyword "stages")

(defun sthenno/denote-org-path-sorted-notes (directory)
  "Return a list of note files in DIRECTORY, sorted by name."
  (sort (seq-filter #'denote-file-has-denoted-filename-p
                    (directory-files directory t "\\.org$"))
        #'string<))

(defun sthenno/denote-journal-find-stages-file-date (offset)
  "Open the Denote journal file OFFSET positions away from the current one."
  (let* ((buffer-file (buffer-file-name))
         (sorted-files (sthenno/denote-org-path-sorted-notes
                        denote-journal-directory))
         (current-file-index (seq-position sorted-files buffer-file #'string=)))
    (if (null current-file-index)
        (message "Current file is not a note file.")
      (let ((target-index (+ current-file-index offset)))
        (if (or (< target-index 0)
                (>= target-index (length sorted-files)))
            (message "No Denote note file.")
          (find-file (nth target-index sorted-files)))))))

(defun sthenno/denote-journal-entry-previous ()
  "Open the previous journal entry."
  (interactive)
  (sthenno/denote-journal-find-stages-file-date -1))

(defun sthenno/denote-journal-entry-next ()
  "Open the next journal entry."
  (interactive)
  (sthenno/denote-journal-find-stages-file-date 1))

(with-eval-after-load 'org
  (keymap-set org-mode-map "s-<up>" #'sthenno/denote-journal-entry-previous)
  (keymap-set org-mode-map "s-<down>" #'sthenno/denote-journal-entry-next))
(keymap-global-set "s-j" #'denote-journal-new-or-existing-entry)


;;; Projects
(autoload 'magit-status "magit" nil t)
(keymap-global-set "C-x g" #'magit-status)
(with-eval-after-load 'magit
  (setopt magit-diff-refine-hunk t))
(keymap-global-set "M-/" #'xref-find-references)
(with-eval-after-load 'xref
  (setopt xref-search-program 'ripgrep))


;;; Templates
(require 'yasnippet)
(setopt yas-triggers-in-field t)
(setopt yas-global-mode t)


;;; Completion and minibuffer
(setopt text-mode-ispell-word-completion 'completion-at-point
        tab-always-indent 'complete
        echo-keystrokes 0.125
        resize-mini-windows t
        help-window-select nil
        read-extended-command-predicate #'command-completion-default-include-p
        minibuffer-default-prompt-format " [%s]"
        minibuffer-visible-completions nil
        completion-cycle-threshold nil
        completions-sort 'historical
        completion-eager-display 'auto
        completion-eager-update 'auto
        completion-styles '(flex)
        completion-ignore-case nil
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        file-name-shadow-mode t)

(require 'vertico)
(require 'vertico-directory)
(setopt vertico-resize t
        vertico-count-format (cons "[ %-6s ] " "%s of %s"))
(keymap-set vertico-map "<tab>" #'vertico-insert)
(keymap-set vertico-map "<return>" #'vertico-directory-enter)
(keymap-set vertico-map "<backspace>" #'vertico-directory-delete-char)
(setopt vertico-mode t)

(require 'marginalia)
(setopt marginalia-mode t)

(require 'consult)
(require 'consult-imenu)
(keymap-global-set "s-b" #'consult-buffer)
(keymap-global-set "C-s" #'consult-line)
(keymap-global-set "s-m" #'consult-imenu)
(keymap-global-set "s-n" #'consult-recent-file)

(require 'dabbrev)
(defun sthenno/dabbrev-elisp ()
  "Tune `dabbrev' for `emacs-lisp-mode'."
  (setopt-local dabbrev-case-fold-search nil
                dabbrev-case-replace nil))
(setopt dabbrev-case-distinction 'case-replace
        dabbrev-case-replace 'case-replace
        dabbrev-case-fold-search nil
        dabbrev-upcase-means-case-search t)
(add-hook 'emacs-lisp-mode-hook #'sthenno/dabbrev-elisp)
(add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
(dolist (mode '(authinfo-mode doc-view-mode pdf-view-mode tags-table-mode))
  (add-to-list 'dabbrev-ignored-buffer-modes mode))

(setopt ispell-program-name "aspell"
        ispell-save-corrections-as-abbrevs t)

(require 'corfu)
(require 'corfu-history)
(require 'corfu-popupinfo)
(defun sthenno/corfu-eshell-setup ()
  "Use a more conservative Corfu setup in Eshell."
  (setopt-local corfu-auto nil)
  (setopt corfu-mode t)
  (keymap-set corfu-map "RET" #'corfu-send))
(setopt corfu-auto t
        corfu-auto-delay 0.05
        corfu-auto-prefix 2
        corfu-preview-current 'insert
        corfu-popupinfo-delay '(0.025 . 0.05))
(keymap-set corfu-map "<down>" #'corfu-next)
(keymap-set corfu-map "TAB" #'corfu-complete)
(keymap-set corfu-map "<up>" #'corfu-previous)
(keymap-set corfu-map "<escape>" #'corfu-quit)
(keymap-set corfu-map "RET" #'corfu-insert)
(add-hook 'eshell-mode-hook #'sthenno/corfu-eshell-setup)
(add-hook 'prog-mode-hook #'corfu-popupinfo-mode)
(setopt corfu-history-mode t)
(setopt global-corfu-mode t)

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'corfu-history))
(with-eval-after-load 'completion-preview
  (setopt completion-preview-sort-function corfu-sort-function))


;;; Languages
(setopt treesit-enabled-modes t
        treesit-auto-install-grammar 'ask)
(with-eval-after-load 'python
  (setopt python-indent-offset 4
          python-indent-guess-indent-offset nil
          python-indent-guess-indent-offset-verbose nil))


;;; AI
(require 'gptel)
(require 'gptel-openai)

(keymap-global-set "s-p" #'gptel)
(keymap-set gptel-mode-map "s-<return>" #'gptel-send)
(keymap-set org-mode-map "s-<return>" #'gptel-send)
(setopt gptel-default-mode 'org-mode
        gptel-org-branching-context t
        gptel-track-media t
        gptel-max-tokens 32768
        gptel-temperature 1.0
        gptel-backend (gptel-make-openai "sthenno"
                        :protocol "http"
                        :host "netzach.local:8000"
                        :endpoint "/v1/chat/completions"
                        :stream t
                        :key "sk-tmp"
                        :models '(sthenno))
        gptel-model 'sthenno)

(provide 'init)
