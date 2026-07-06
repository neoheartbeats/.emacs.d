;;; init.el --- Load the full configuration -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Main single-file Emacs configuration.

;;; Code:

(setq custom-file (make-temp-file ".emacs-custom"))

;;; Package manager
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))

(let* ((repo (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process
                                 `("git" nil ,buffer t "clone"
                                   ,@(when-let* ((depth (plist-get order :depth)))
                                       (list (format "--depth=%d" depth)
                                             "--no-single-branch"))
                                   ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "."
                                        "--batch" "--eval"
                                        "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn
              (message "%s" (buffer-string))
              (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer
                        (buffer-string))))
      ((error)
       (warn "%s" err)
       (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil))
      (load "./elpaca-autoloads"))))

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Keep package declarations explicit because `elpaca' is a macro.
(elpaca consult)
(elpaca corfu)
(elpaca denote)
(elpaca denote-journal)
(elpaca denote-org)
(elpaca gptel)
(elpaca magit)
(elpaca marginalia)
(elpaca vertico)

;; The config below uses direct `require' forms, so activate queued packages now.
(elpaca-wait)

(autoload 'denote-journal-new-or-existing-entry "denote-journal" nil t)

;;; Startup
(setopt inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        inhibit-default-init t
        initial-scratch-message ""
        initial-buffer-choice #'denote-journal-new-or-existing-entry)

(setopt user-full-name user-login-name
        user-mail-address "sthenno@sthenno.com")

(setopt menu-bar-mode nil)
(setopt scroll-bar-mode nil)
(setopt tool-bar-mode nil)

(define-advice display-startup-echo-area-message
    (:override () sthenno-startup-message)
  "Display a custom startup message in the echo area."
  (message "Funding for this program was made possible by viewers like you."))


;;; Platform
(setopt mac-option-modifier 'meta
        mac-command-modifier 'super)


;;; Files
(setopt save-silently t
        remote-file-name-inhibit-locks t
        backup-inhibited t
        global-auto-revert-mode t)


;;; Windows
(setopt redisplay-skip-fontification-on-input t
        mode-line-compact t
        header-line-format ""
        switch-to-prev-buffer-skip 0)


;;; Editing
(setopt fill-column 88
        elisp-fontify-semantically t
        indent-tabs-mode nil
        backward-delete-char-untabify-method 'hungry
        kill-do-not-save-duplicates t
        sentence-end-double-space nil
        save-interprogram-paste-before-kill t)

(setopt display-line-numbers-width 4)
(setopt display-line-numbers-grow-only t)
(add-hook 'prog-mode-hook #'(lambda ()
                              (display-line-numbers-mode 1)))


;;; Interaction
(setopt ring-bell-function #'ignore
        use-short-answers t
        use-dialog-box nil
        copy-region-blink-predicate #'ignore)


;;; Persistence
(require 'savehist)
(add-to-list 'savehist-additional-variables 'corfu-history)

(setopt save-place-autosave-interval nil
        save-place-mode t
        savehist-mode t
        electric-pair-mode t
        delete-selection-mode t)


;;; Key bindings
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

(defun sthenno/lisp-load-buffer ()
  "Load the current Lisp buffer"
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name)))

(keymap-set emacs-lisp-mode-map "s-i" #'sthenno/lisp-indent-buffer)
(keymap-set emacs-lisp-mode-map "C-c C-c" #'sthenno/lisp-load-buffer)
(keymap-set emacs-lisp-mode-map "s-k" #'kill-sexp)
(keymap-set emacs-lisp-mode-map "M-<backspace>" #'backward-kill-sexp)

;;; Dired
(with-eval-after-load 'dired
  (setopt insert-directory-program (or (executable-find "gls")
                                       insert-directory-program)
          dired-no-confirm t
          dired-use-ls-dired t
          dired-hide-details-hide-information-lines t
          dired-hide-details-hide-absolute-location t
          dired-check-symlinks nil
          dired-recursive-deletes 'always
          dired-movement-style 'cycle))

;;; Scrolling

(setopt pixel-scroll-precision-mode t)

;;; Appearance
(require-theme 'modus-themes)
(setopt modus-themes-common-palette-overrides '(
                                                ;; (fg-paren-match unspecified)
                                                ;; (bg-paren-match unspecified)
                                                (border bg-main)
                                                (fringe bg-main)))
(load-theme 'modus-vivendi :no-confirm)

(set-face-attribute 'default nil :family "M PLUS 1 Code" :height 140)
(set-face-attribute 'region nil :extend t :foreground 'unspecified)

;;; Fonts
(let ((font "PingFang SC"))
  (dolist (charset '(kana han cjk-misc))
    (set-fontset-font t charset (font-spec :family font))))
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
(set-fontset-font t 'ucs (font-spec :family "SF Pro") nil 'prepend)

;;; Cursor and matching
(setopt cursor-type '(bar . 1)
        x-stretch-cursor t
        blink-cursor-mode nil)

(setopt default-input-method nil
        show-paren-delay 0.125
        show-paren-context-when-offscreen t
        show-paren-mode t
        show-paren-not-in-comments-or-strings 'on-mismatch)

;;; Org and notes
(require 'org)

;;; Org files
(setopt org-directory "/Users/sthenno/Developer/op1/"
        org-persist-directory (locate-user-emacs-file "org-persist/"))

;;; Org display
(setopt org-startup-truncated nil
        org-startup-with-link-previews t
        org-ellipsis " (*)"
        org-hide-emphasis-markers t
        org-hide-macro-markers t
        org-hide-drawer-startup t
        org-image-align 'left
        org-image-actual-width t
        org-image-max-width 0.6)

(define-advice org--create-inline-image
    (:filter-return (image) sthenno-max-height)
  "Keep Org inline image preview height within the preview width."
  (when-let* ((props (cdr-safe image))
              (size (or (plist-get props :max-width)
                        (plist-get props :width))))
    (setcdr image (plist-put props :max-height 320)))
  image)

(setopt org-attach-method 'cp)

;;; Org Babel
(setopt org-babel-uppercase-example-markers t
        org-confirm-babel-evaluate nil
        org-babel-update-intermediate t
        org-babel-load-languages '((emacs-lisp . t)
                                   (python . t)
                                   (shell . t)))

;;; Org source editing
(setopt org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-content-indentation 0
        org-edit-src-persistent-message nil
        org-src-tab-acts-natively t
        org-src-ask-before-returning-to-edit-buffer nil)

;;; Denote
(require 'denote)
(setopt denote-directory org-directory
        denote-file-type 'org
        denote-known-keywords '("stages" "silos" "images" "papers")
        denote-save-buffers t
        denote-kill-buffers t
        denote-open-link-function #'find-file
        denote-sort-dired-default-reverse-sort t
        denote-rename-buffer-mode t)

(setopt denote-buffer-name-prefix "[D] "
        denote-org-front-matter "#+title: %1$s\n\n")

;;; Denote Org
(require 'denote-org)
(setopt denote-org-store-link-to-heading 'context)

;;; Denote journal
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

(keymap-set org-mode-map "s-<up>" #'sthenno/denote-journal-entry-previous)
(keymap-set org-mode-map "s-<down>" #'sthenno/denote-journal-entry-next)
(keymap-global-set "s-j" #'denote-journal-new-or-existing-entry)


;;; Projects
(autoload 'magit-status "magit" nil t)
(keymap-global-set "C-x g" #'magit-status)
(with-eval-after-load 'magit
  (setopt magit-diff-refine-hunk t))
(keymap-global-set "M-/" #'xref-find-references)
(with-eval-after-load 'xref
  (setopt xref-search-program 'ripgrep))


;;; Completion and minibuffer

;; Completion defaults
(setopt text-mode-ispell-word-completion 'completion-at-point
        tab-always-indent 'complete)

;; Minibuffer
(setopt echo-keystrokes 0.125
        resize-mini-windows t
        help-window-select nil
        read-extended-command-predicate #'command-completion-default-include-p
        minibuffer-default-prompt-format " [%s]"
        minibuffer-visible-completions nil)

;; Completion styles
(setopt completion-styles '(flex)
        completion-cycle-threshold nil
        completions-sort 'historical
        completion-eager-display 'auto
        completion-eager-update 'auto
        completion-ignore-case nil)
(setopt read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        file-name-shadow-mode t)

;; Vertico

;; (require 'vertico)
;; (require 'vertico-directory)
;; (setopt vertico-resize t
;;         vertico-count-format (cons "[ %-6s ] " "%s of %s"))

;; (keymap-set vertico-map "<tab>" #'vertico-insert)
;; (keymap-set vertico-map "<return>" #'vertico-directory-enter)
;; (keymap-set vertico-map "<backspace>" #'vertico-directory-delete-char)
;; (setopt vertico-mode t)



;; Marginalia

;; (require 'marginalia)
;; (setopt marginalia-mode t)

;; Consult
(autoload 'consult-buffer "consult" nil t)
(autoload 'consult-line "consult" nil t)
(keymap-global-set "s-b" #'consult-buffer)
(keymap-global-set "C-s" #'consult-line)
;; (keymap-global-set "s-m" #'consult-imenu-multi)

;; Dabbrev
(require 'dabbrev)
(defun sthenno/dabbrev-elisp ()
  "Tune `dabbrev' for `emacs-lisp-mode'."
  (setopt-local dabbrev-case-fold-search nil
                dabbrev-case-replace nil))

(setopt dabbrev-case-distinction 'case-replace
        dabbrev-case-replace 'case-replace
        dabbrev-case-fold-search nil
        dabbrev-upcase-means-case-search t)

(define-advice dabbrev-capf
    (:around (capf &rest args) sthenno-ignore-user-error)
  "Return nil when Dabbrev reports no completion."
  (condition-case nil
      (apply capf args)
    (user-error nil)))

(add-hook 'emacs-lisp-mode-hook #'sthenno/dabbrev-elisp)
(add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
(dolist (mode '(authinfo-mode doc-view-mode pdf-view-mode tags-table-mode))
  (add-to-list 'dabbrev-ignored-buffer-modes mode))

;; Corfu
(require 'corfu)

(defun sthenno/corfu-confirm ()
  "Accept the current Corfu candidate, sending it to Eshell when appropriate."
  (interactive)
  (if (derived-mode-p 'eshell-mode)
      (corfu-send)
    (corfu-insert)))

(defun sthenno/corfu-eshell-setup ()
  "Use a more conservative Corfu setup in Eshell."
  (setopt-local corfu-auto nil)
  (corfu-mode 1))

(setopt corfu-auto t
        corfu-auto-delay 0.125
        corfu-auto-prefix 2
        corfu-preview-current 'insert
        corfu-popupinfo-delay '(0.125 . 0.25))

(defun my-corfu-move-to-minibuffer ()
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           completion-cycle-threshold completion-cycling)
       (consult-completion-in-region beg end table pred)))))
(add-to-list 'corfu-continue-commands #'my-corfu-move-to-minibuffer)

;; TAB in `corfu-map' is bound at the end of the AI section, together with
;; the inline completion TAB dispatch.
(keymap-set corfu-map "<down>" #'corfu-next)
(keymap-set corfu-map "<up>" #'corfu-previous)
(keymap-set corfu-map "<escape>" #'corfu-quit)
(keymap-set corfu-map "RET" #'sthenno/corfu-confirm)

(add-hook 'eshell-mode-hook #'sthenno/corfu-eshell-setup)
(add-hook 'prog-mode-hook #'corfu-popupinfo-mode)
(corfu-history-mode 1)
(global-corfu-mode 1)

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

;; (require 'gptel)

;; (keymap-global-set "s-p" #'gptel)
;; (keymap-set gptel-mode-map "s-<return>" #'gptel-send)
;; (keymap-set org-mode-map "s-<return>" #'gptel-send)

;; (setopt gptel-default-mode 'org-mode
;;         gptel-org-branching-context t
;;         gptel-track-media t)
;; (setopt gptel-model 'sthenno
;;         gptel-max-tokens 32768
;;         gptel-temperature 1.0
;;         gptel-backend (gptel-make-openai "sthenno"
;;                         :protocol "http"
;;                         :host "netzach:8000"
;;                         :endpoint "/v1/chat/completions"
;;                         :stream t
;;                         :key "sk-tmp"
;;                         :models '((sthenno :capabilities (media tool-use json url)
;;                                            :mime-types ("image/jpeg" "image/png" "image/webp")
;;                                            :context-window 256))))

;;; init.el ends here
