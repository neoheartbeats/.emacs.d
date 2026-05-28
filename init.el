;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Main single-file Emacs configuration.

;;; Code:

(setq custom-file (make-temp-file ".emacs-custom"))

(declare-function consult-imenu-multi "consult-imenu" (&optional query))
(declare-function corfu-history-mode "corfu-history" (&optional arg))
(declare-function corfu-popupinfo-mode "corfu-popupinfo" (&optional arg))
(declare-function denote-journal-new-or-existing-entry "denote-journal" (&optional date))
(declare-function gptel-make-openai "gptel-openai" (name &rest args))
(declare-function vertico-directory-delete-char "vertico-directory" (n))
(declare-function vertico-directory-enter "vertico-directory" (&optional arg))


;;; Startup
(setopt inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        inhibit-default-init t
        initial-scratch-message ""
        initial-buffer-choice #'denote-journal-new-or-existing-entry)

(setopt user-full-name user-login-name
        user-mail-address "sthenno@sthenno.com")

(setopt menu-bar-mode nil
        scroll-bar-mode nil
        tool-bar-mode nil
        line-number-mode nil
        column-number-mode nil)

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
(setopt fill-column 100
        elisp-fontify-semantically t
        indent-tabs-mode nil
        backward-delete-char-untabify-method 'hungry
        kill-do-not-save-duplicates t
        sentence-end-double-space nil
        save-interprogram-paste-before-kill t)


;;; Interaction
(setopt ring-bell-function #'ignore
        use-short-answers t
        use-dialog-box nil
        copy-region-blink-predicate #'ignore)


;;; Persistence
(require 'savehist)
(add-to-list 'savehist-additional-variables 'corfu-history)

(setopt save-place-autosave-interval 300
        save-place-mode t
        savehist-mode t
        electric-pair-mode t
        delete-selection-mode t)


;;; Key bindings
(dolist (binding '(("s-q" . kill-emacs)
                   ("s-w" . kill-current-buffer)
                   ("s-e" . delete-window)
                   ("s-d" . find-file)
                   ("s-<right>" . switch-to-next-buffer)
                   ("s-<left>" . switch-to-prev-buffer)
                   ("<escape>" . keyboard-escape-quit)
                   ("M-<down>" . forward-paragraph)
                   ("M-<up>" . backward-paragraph)))
  (keymap-global-set (car binding) (cdr binding)))

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
        (lisp-indent-region (point-min) (point-max))
        (delete-trailing-whitespace)))))
(dolist (binding '(("s-i" . sthenno/lisp-indent-buffer)
                   ("C-c C-c" . emacs-lisp-byte-compile-and-load)
                   ("s-k" . kill-sexp)
                   ("M-<backspace>" . backward-kill-sexp)))
  (keymap-set emacs-lisp-mode-map (car binding) (cdr binding)))

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
(setopt scroll-margin 0)
(require 'ultra-scroll)
(setopt ultra-scroll-mode t)


;;; Appearance
(require-theme 'modus-themes)
(setopt modus-themes-common-palette-overrides '((fg-paren-match unspecified)
                                                (bg-paren-match unspecified)
                                                (border bg-main)
                                                (fringe bg-main)))
(load-theme 'modus-vivendi :no-confirm)

(set-face-attribute 'default nil :family "Tempestypes" :height 140)
(set-face-attribute 'region nil :extend t :foreground 'unspecified)
(set-face-attribute 'fill-column-indicator nil :height 0.1)
(set-face-attribute 'show-paren-match nil
                    :background 'unspecified :foreground "green" :box '(:line-width (-1 . -1)))

;;; Fonts
(let ((font "PingFang SC"))
  (dolist (charset '(kana han cjk-misc))
    (set-fontset-font t charset (font-spec :family font))))
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
(set-fontset-font t 'ucs (font-spec :family "SF Pro") nil 'prepend)

;;; Highlighting
;; (setopt global-hl-line-sticky-flag nil
;;         global-hl-line-mode t)

;;; Columns and line numbers

;; (setopt global-display-fill-column-indicator-mode t
;;         display-line-numbers-widen t
;;         display-line-numbers-width 4
;;         global-display-line-numbers-mode t)

;;; Cursor and matching
(setopt cursor-type '(bar . 1)
        x-stretch-cursor t
        blink-cursor-mode nil)

(setopt default-input-method nil
        show-paren-delay 0.0125
        show-paren-context-when-offscreen t
        show-paren-mode t
        show-paren-not-in-comments-or-strings 'on-mismatch)


;;; Org and notes
(require 'seq)
(require 'image)
(require 'org)

;;; Org files
(setopt org-directory "/Users/sthenno/Developer/op1/"
        org-persist-directory (locate-user-emacs-file "org-persist/"))

;;; Org display
(setopt org-startup-truncated t
        org-startup-with-link-previews t
        org-ellipsis " (*)"
        org-hide-emphasis-markers t
        org-hide-macro-markers t
        org-hide-drawer-startup t
        org-image-align 'left
        org-image-max-width 0.60)

;;; Org interaction
(setopt org-link-elisp-confirm-function nil
        org-special-ctrl-a t
        org-return-follows-link t
        org-support-shift-select t
        org-use-property-inheritance t)

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

;;; Org image completion
(defvar sthenno/org-image-completion-directory "/Users/sthenno/Pictures/"
  "Root directory for Org image link completion.")

(defvar sthenno/org-image-completion-trigger "/"
  "Trigger string for Org image link completion.")

(defun sthenno/org-image-completion-file-p (file)
  "Return non-nil if FILE is an image file Emacs can display."
  (and (file-regular-p file)
       (file-readable-p file)
       (image-supported-file-p file)))

(defun sthenno/org-image-completion-candidates ()
  "Return root-level image candidates with full paths attached."
  (let ((directory (file-name-as-directory sthenno/org-image-completion-directory)))
    (when (file-directory-p directory)
      (mapcar (lambda (file)
                (propertize (file-name-nondirectory file)
                            'sthenno/org-image-completion-path file))
              (sort (seq-filter #'sthenno/org-image-completion-file-p
                                (directory-files directory t nil t))
                    #'string<)))))

(defun sthenno/org-image-completion-candidate-path (candidate)
  "Return the full image path for CANDIDATE."
  (or (get-text-property 0 'sthenno/org-image-completion-path candidate)
      (file-name-concat
       (file-name-as-directory sthenno/org-image-completion-directory)
       (substring-no-properties candidate))))

(defun sthenno/org-image-completion-doc-buffer (candidate)
  "Return a preview buffer for image CANDIDATE."
  (let* ((path (sthenno/org-image-completion-candidate-path candidate))
         (image (and (sthenno/org-image-completion-file-p path)
                     (ignore-errors
                       (create-image path nil nil :max-width 320 :max-height 240)))))
    (when image
      (with-current-buffer (get-buffer-create " *sthenno-org-image-preview*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-image image (file-name-nondirectory path))
          (insert "\n" (file-name-nondirectory path)))
        (current-buffer)))))

(defun sthenno/org-image-completion-trigger-bounds ()
  "Return bounds for the nearest Org image completion trigger on this line."
  (let ((trigger sthenno/org-image-completion-trigger))
    (save-excursion
      (when (and (not (equal trigger ""))
                 (search-backward trigger (line-beginning-position) t))
        (cons (point) (match-end 0))))))

(defun sthenno/org-image-completion-insert-link (trigger-start candidate status)
  "Replace TRIGGER-START through point with an Org link to CANDIDATE.
STATUS is the completion exit status."
  (when (eq status 'finished)
    (let ((path (sthenno/org-image-completion-candidate-path candidate)))
      (when (sthenno/org-image-completion-file-p path)
        (delete-region trigger-start (point))
        (insert (org-link-make-string path))
        (org-link-preview)))))

(defun sthenno/org-image-completion-at-point ()
  "Complete Org image links after `sthenno/org-image-completion-trigger'."
  (when (derived-mode-p 'org-mode)
    (when-let* ((bounds (sthenno/org-image-completion-trigger-bounds)))
      (let ((trigger-start (copy-marker (car bounds)))
            (completion-start (cdr bounds)))
        (list completion-start
              (point)
              (sthenno/org-image-completion-candidates)
              :company-doc-buffer #'sthenno/org-image-completion-doc-buffer
              :company-prefix-length t
              :exit-function (lambda (candidate status)
                               (sthenno/org-image-completion-insert-link trigger-start
                                                                         candidate
                                                                         status)))))))

(defun sthenno/org-image-completion-setup ()
  "Enable Org image completion in the current buffer."
  (add-hook 'completion-at-point-functions
            #'sthenno/org-image-completion-at-point nil t)
  (corfu-popupinfo-mode 1))

(add-hook 'org-mode-hook #'sthenno/org-image-completion-setup)

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

(dolist (binding '(("s-<up>" . sthenno/denote-journal-entry-previous)
                   ("s-<down>" . sthenno/denote-journal-entry-next)))
  (keymap-set org-mode-map (car binding) (cdr binding)))
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
        completion-ignore-case nil
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        file-name-shadow-mode t)

;; Vertico
(require 'vertico)
(setopt vertico-resize t
        vertico-count-format (cons "[ %-6s ] " "%s of %s"))

(dolist (binding '(("<tab>" . vertico-insert)
                   ("<return>" . vertico-directory-enter)
                   ("<backspace>" . vertico-directory-delete-char)))
  (keymap-set vertico-map (car binding) (cdr binding)))
(setopt vertico-mode t)

;; Marginalia
(require 'marginalia)
(setopt marginalia-mode t)

;; Consult
(require 'consult)
(dolist (binding '(("s-b" . consult-buffer)
                   ("C-s" . consult-line)
                   ("s-m" . consult-imenu-multi)))
  (keymap-global-set (car binding) (cdr binding)))

;; Dabbrev
(require 'dabbrev)
(defun sthenno/dabbrev-elisp ()
  "Tune `dabbrev' for `emacs-lisp-mode'."
  (setopt-local dabbrev-case-fold-search nil
                dabbrev-case-replace nil))

(defun sthenno/dabbrev-capf-ignore-user-error (capf &rest args)
  "Return nil when CAPF reports no dabbrev completion."
  (condition-case nil
      (apply capf args)
    (user-error nil)))

(setopt dabbrev-case-distinction 'case-replace
        dabbrev-case-replace 'case-replace
        dabbrev-case-fold-search nil
        dabbrev-upcase-means-case-search t)
(advice-add 'dabbrev-capf :around #'sthenno/dabbrev-capf-ignore-user-error)
(add-hook 'emacs-lisp-mode-hook #'sthenno/dabbrev-elisp)
(add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
(dolist (mode '(authinfo-mode doc-view-mode pdf-view-mode tags-table-mode))
  (add-to-list 'dabbrev-ignored-buffer-modes mode))

;; Spelling
(setopt ispell-program-name "aspell"
        ispell-save-corrections-as-abbrevs t)

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
        corfu-auto-delay 0.0125
        corfu-auto-prefix 2
        corfu-preview-current 'insert
        corfu-popupinfo-delay '(0.0125 . 0.025))
(dolist (binding '(("<down>" . corfu-next)
                   ("TAB" . corfu-complete)
                   ("<up>" . corfu-previous)
                   ("<escape>" . corfu-quit)
                   ("RET" . sthenno/corfu-confirm)))
  (keymap-set corfu-map (car binding) (cdr binding)))
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
(require 'gptel)

(keymap-global-set "s-p" #'gptel)
(dolist (map (list gptel-mode-map org-mode-map))
  (keymap-set map "s-<return>" #'gptel-send))

(setopt gptel-default-mode 'org-mode
        gptel-org-branching-context t
        gptel-track-media t)
(setopt gptel-model 'sthenno
        gptel-max-tokens 32768
        gptel-temperature 1.0
        gptel-backend (gptel-make-openai "sthenno"
                        :protocol "http"
                        :host "netzach:8000"
                        :endpoint "/v1/chat/completions"
                        :stream t
                        :key "sk-tmp"
                        :models '(sthenno)))

(provide 'init)
