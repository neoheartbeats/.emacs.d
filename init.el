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
                                                (fringe bg-main)
                                                (cursor fg-alt)))
(load-theme 'modus-vivendi :no-confirm)

(set-face-attribute 'default nil :family "Tempestypes" :height 140)
(set-face-attribute 'region nil :extend t :foreground 'unspecified)
;; (set-face-attribute 'fill-column-indicator nil :height 0.1)
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

;; (setopt global-display-fill-column-indicator-mode t)
;; (setopt display-line-numbers-widen t
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
        (org-link-preview-region)))))

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
        completion-ignore-case nil)
(setopt read-file-name-completion-ignore-case t
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
                        :models '((sthenno :capabilities (media tool-use json url)
                                           :mime-types ("image/jpeg" "image/png" "image/webp")
                                           :context-window 256))))

(require 'subr-x)

(defface sthenno/gptel-inline-completion-face
  '((t :foreground "#535353"))
  "Face for gptel inline completion ghost text."
  :group 'faces)

(defvar-local sthenno/gptel-inline-completion-mode nil
  "Non-nil when gptel inline completion mode is enabled.")

(defvar sthenno/gptel-inline-completion-delay 0.15
  "Idle delay before requesting a gptel inline completion.")

(defvar sthenno/gptel-inline-completion-max-tokens 128
  "Maximum tokens to request for gptel inline completions.")

(defvar sthenno/gptel-inline-completion-temperature 0.2
  "Temperature to use for gptel inline completions.")

(defvar sthenno/gptel-inline-completion-before-lines 80
  "Number of lines before point to include in inline completion prompts.")

(defvar sthenno/gptel-inline-completion-after-lines 20
  "Number of lines after point to include in inline completion prompts.")

(defvar sthenno/gptel-inline-completion-context-chars 32768
  "Approximate character budget for gptel inline completion context.")

(defvar sthenno/gptel-inline-completion-system-prompt
  (concat "You are an inline code and prose completion engine. "
          "Return only the exact text to insert at <cursor>. "
          "Prefer the smallest useful continuation.")
  "System prompt used for gptel inline completions.")

(defvar-local sthenno/gptel-inline--overlay nil
  "Overlay displaying the current gptel inline completion.")

(defvar-local sthenno/gptel-inline--text ""
  "Current raw gptel inline completion text.")

(defvar-local sthenno/gptel-inline--timer nil
  "Idle timer for the current buffer's gptel inline completion.")

(defvar-local sthenno/gptel-inline--request-id 0
  "Generation id for the current buffer's gptel inline completion request.")

(defvar-local sthenno/gptel-inline--in-flight nil
  "Non-nil while a gptel inline completion request is running.")

(defun sthenno/gptel-inline--cancel ()
  "Cancel the current buffer's pending inline completion timer."
  (when (timerp sthenno/gptel-inline--timer)
    (cancel-timer sthenno/gptel-inline--timer))
  (setq sthenno/gptel-inline--timer nil))

(defun sthenno/gptel-inline--hide ()
  "Delete the current inline completion overlay."
  (when (overlayp sthenno/gptel-inline--overlay)
    (delete-overlay sthenno/gptel-inline--overlay))
  (setq sthenno/gptel-inline--overlay nil))

(defun sthenno/gptel-inline--clean (text)
  "Return displayable inline completion TEXT."
  (let ((clean (substring-no-properties (or text ""))))
    (setq clean
          (replace-regexp-in-string
           "\\`[[:space:]\n\r]*```[[:alnum:]_-]*[[:space:]\n\r]*\n" "" clean))
    (replace-regexp-in-string "\n?[[:space:]\n\r]*```[[:space:]\n\r]*\\'" "" clean)))

(defun sthenno/gptel-inline--position (&optional position)
  "Return the inline completion text at POSITION, if any."
  (let ((position (or position (point))))
    (when (and (overlayp sthenno/gptel-inline--overlay)
               (overlay-buffer sthenno/gptel-inline--overlay)
               (= (overlay-start sthenno/gptel-inline--overlay) position))
      (let ((text (sthenno/gptel-inline--clean
                   sthenno/gptel-inline--text)))
        (unless (string-empty-p text)
          text)))))

(defun sthenno/gptel-inline--show (text position &optional window)
  "Show TEXT as inline completion ghost text at POSITION.
When WINDOW is live and displays the current buffer, scope the overlay to it."
  (let ((display (sthenno/gptel-inline--clean text)))
    (if (string-empty-p display)
        (sthenno/gptel-inline--hide)
      (let ((ghost (propertize display
                               'face 'sthenno/gptel-inline-completion-face)))
        (add-text-properties 0 1 '(cursor 1) ghost)
        (unless (overlayp sthenno/gptel-inline--overlay)
          (setq sthenno/gptel-inline--overlay
                (make-overlay position position nil nil nil))
          (overlay-put sthenno/gptel-inline--overlay 'priority 1000))
        (move-overlay sthenno/gptel-inline--overlay
                      position position (current-buffer))
        (overlay-put sthenno/gptel-inline--overlay
                     'window
                     (and (window-live-p window)
                          (eq (window-buffer window) (current-buffer))
                          window))
        (overlay-put sthenno/gptel-inline--overlay
                     'after-string ghost)))))

(defun sthenno/gptel-inline--bounds (cursor)
  "Return inline completion context bounds around CURSOR."
  (let* ((line-before-start (save-excursion
                              (goto-char cursor)
                              (forward-line (- sthenno/gptel-inline-completion-before-lines))
                              (line-beginning-position)))
         (line-after-end (save-excursion
                           (goto-char cursor)
                           (forward-line sthenno/gptel-inline-completion-after-lines)
                           (line-end-position)))
         (budget sthenno/gptel-inline-completion-context-chars)
         (before-budget (floor (* budget 0.75)))
         (after-budget (- budget before-budget)))
    (cons (max line-before-start (- cursor before-budget))
          (min line-after-end (+ cursor after-budget)))))

(defun sthenno/gptel-inline--media (beg end)
  "Return Org media context entries between BEG and END."
  (when (derived-mode-p 'org-mode)
    (require 'gptel-org)
    (let ((gptel-track-media t))
      (delq nil
            (mapcar (lambda (part)
                      (when-let* ((path (plist-get part :media))
                                  (mime (plist-get part :mime)))
                        (list path :mime mime)))
                    (gptel--parse-media-links major-mode beg end))))))

(defun sthenno/gptel-inline-completion-clear ()
  "Clear the current gptel inline completion."
  (interactive)
  (setq sthenno/gptel-inline--request-id
        (1+ sthenno/gptel-inline--request-id)
        sthenno/gptel-inline--text "")
  (sthenno/gptel-inline--hide))

(defun sthenno/gptel-inline--corfu-p ()
  "Return non-nil when a completion UI is active in the current buffer."
  (bound-and-true-p completion-in-region-mode))

(defun sthenno/gptel-inline--blocked-p ()
  "Return non-nil when inline completion should not run in this buffer."
  (or (minibufferp)
      buffer-read-only
      (use-region-p)
      (bound-and-true-p gptel-mode)))

(defun sthenno/gptel-inline--eol-p ()
  "Return non-nil when point is in a good place for inline completion."
  (save-excursion
    (skip-chars-forward " \t")
    (eolp)))

(defun sthenno/gptel-inline--ready-p ()
  "Return non-nil when the current buffer should request inline completion."
  (and sthenno/gptel-inline-completion-mode
       (not sthenno/gptel-inline--in-flight)
       (not (sthenno/gptel-inline--position))
       (not (sthenno/gptel-inline--corfu-p))
       (not (sthenno/gptel-inline--blocked-p))
       (sthenno/gptel-inline--eol-p)
       (not (bobp))))

(defun sthenno/gptel-inline--prompt (cursor)
  "Return (PROMPT . MEDIA-CONTEXT) for an inline completion at CURSOR."
  (let* ((bounds (sthenno/gptel-inline--bounds cursor))
         (before-start (car bounds))
         (after-end (cdr bounds))
         (file (or buffer-file-name (buffer-name)))
         (before (buffer-substring-no-properties before-start cursor))
         (after (buffer-substring-no-properties cursor after-end)))
    (cons (format "File: %s\nMajor mode: %S\n\nBefore <cursor>:\n%s\n<cursor>\n\nAfter <cursor>:\n%s\n\nComplete at <cursor>."
                  file major-mode before after)
          (sthenno/gptel-inline--media before-start after-end))))

(defun sthenno/gptel-inline--current-p (request-id request-marker request-tick)
  "Return non-nil if REQUEST-ID, REQUEST-MARKER and REQUEST-TICK still match."
  (and sthenno/gptel-inline-completion-mode
       (= request-id sthenno/gptel-inline--request-id)
       (when-let* ((position (marker-position request-marker)))
         (= position (point)))
       (= request-tick (buffer-chars-modified-tick))
       (not (sthenno/gptel-inline--blocked-p))
       (not (sthenno/gptel-inline--corfu-p))))

(defun sthenno/gptel-inline--handle
    (response info buffer request-id request-marker request-tick request-window manual)
  "Handle a gptel inline completion RESPONSE with request metadata.
INFO is the gptel callback plist.  BUFFER, REQUEST-ID, REQUEST-MARKER,
REQUEST-TICK, REQUEST-WINDOW and MANUAL describe the original request."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (cond
         ((stringp response)
          (when (sthenno/gptel-inline--current-p
                 request-id request-marker request-tick)
            (setq sthenno/gptel-inline--text
                  (concat sthenno/gptel-inline--text response))
            (sthenno/gptel-inline--show
             sthenno/gptel-inline--text
             (marker-position request-marker)
             request-window)))
         ((eq response t)
          (let ((current (sthenno/gptel-inline--current-p
                          request-id request-marker request-tick)))
            (set-marker request-marker nil)
            (setq sthenno/gptel-inline--in-flight nil)
            (cond
             ((and current
                   (string-empty-p (sthenno/gptel-inline--clean
                                    sthenno/gptel-inline--text)))
              (sthenno/gptel-inline-completion-clear))
             ((and (not current)
                   (/= request-tick (buffer-chars-modified-tick)))
              (sthenno/gptel-inline--schedule)))))
         ((or (null response) (eq response 'abort))
          (set-marker request-marker nil)
          (setq sthenno/gptel-inline--in-flight nil)
          (when (= request-id sthenno/gptel-inline--request-id)
            (sthenno/gptel-inline-completion-clear))
          (when (and manual (null response))
            (message "gptel inline completion failed: %s"
                     (or (plist-get info :status) "no response")))))))))

(defun sthenno/gptel-inline-completion-request (&optional manual)
  "Request an inline completion from gptel.
When MANUAL is non-nil, report why no request was started."
  (interactive (list t))
  (save-excursion
    (sthenno/gptel-inline-completion-clear)
    (cond
     (sthenno/gptel-inline--in-flight
      (when manual
        (message "gptel inline completion request already running.")))
     ((not (sthenno/gptel-inline--ready-p))
      (when manual
        (message "gptel inline completion is not available here.")))
     (t
      (let* ((buffer (current-buffer))
             (request-id (1+ sthenno/gptel-inline--request-id))
             (request-marker (copy-marker (point) nil))
             (request-tick (buffer-chars-modified-tick))
             (request-window (and (eq (window-buffer (selected-window)) buffer)
                                  (selected-window)))
             (prompt-data (sthenno/gptel-inline--prompt
                           (marker-position request-marker)))
             (prompt (car prompt-data))
             (media-context (cdr prompt-data)))
        (setq sthenno/gptel-inline--request-id request-id
              sthenno/gptel-inline--in-flight t
              sthenno/gptel-inline--text "")
        (condition-case err
            (let ((gptel-use-context nil)
                  (gptel-context media-context)
                  (gptel-use-tools nil)
                  (gptel-max-tokens sthenno/gptel-inline-completion-max-tokens)
                  (gptel-temperature sthenno/gptel-inline-completion-temperature)
                  (gptel-track-media (and media-context t)))
              (when media-context
                (require 'gptel-context)
                (setq gptel-use-context 'user))
              (gptel-request prompt
                :buffer buffer
                :position request-marker
                :stream t
                :system sthenno/gptel-inline-completion-system-prompt
                :callback (lambda (response info)
                            (sthenno/gptel-inline--handle
                             response info buffer request-id request-marker
                             request-tick request-window manual))))
          (error
           (set-marker request-marker nil)
           (setq sthenno/gptel-inline--in-flight nil)
           (when manual
             (message "gptel inline completion failed: %s"
                      (error-message-string err))))))))))

(defun sthenno/gptel-inline-completion-accept ()
  "Accept the visible gptel inline completion.
Return non-nil when a completion was accepted."
  (interactive)
  (when-let* ((text (sthenno/gptel-inline--position)))
    (sthenno/gptel-inline-completion-clear)
    (insert text)
    t))

(defun sthenno/gptel-inline-completion-tab ()
  "Accept inline completion, complete with Corfu, or indent."
  (interactive)
  (cond
   ((sthenno/gptel-inline-completion-accept))
   ((sthenno/gptel-inline--corfu-p)
    (corfu-complete))
   (t
    (indent-for-tab-command))))

(defvar sthenno/gptel-inline-completion-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "TAB" #'sthenno/gptel-inline-completion-tab)
    (keymap-set map "<tab>" #'sthenno/gptel-inline-completion-tab)
    map)
  "Keymap for `sthenno/gptel-inline-completion-mode'.")

(defun sthenno/gptel-inline--idle (buffer)
  "Request inline completion in BUFFER from an idle timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq sthenno/gptel-inline--timer nil)
      (when (sthenno/gptel-inline--ready-p)
        (sthenno/gptel-inline-completion-request)))))

(defun sthenno/gptel-inline--schedule ()
  "Schedule a gptel inline completion request for the current buffer."
  (sthenno/gptel-inline--cancel)
  (when (sthenno/gptel-inline--ready-p)
    (setq sthenno/gptel-inline--timer
          (run-with-idle-timer
           sthenno/gptel-inline-completion-delay nil #'sthenno/gptel-inline--idle (current-buffer)))))

(defun sthenno/gptel-inline--post-command ()
  "Update inline completion state after each command."
  (when (and (overlayp sthenno/gptel-inline--overlay)
             (overlay-buffer sthenno/gptel-inline--overlay)
             (or (not (= (overlay-start sthenno/gptel-inline--overlay)
                         (point)))
                 (sthenno/gptel-inline--blocked-p)
                 (sthenno/gptel-inline--corfu-p)))
    (sthenno/gptel-inline-completion-clear)))

(defun sthenno/gptel-inline--after-change (&rest _)
  "Schedule inline completion after buffer text changes."
  (when sthenno/gptel-inline-completion-mode
    (sthenno/gptel-inline-completion-clear)
    (sthenno/gptel-inline--schedule)))

(define-minor-mode sthenno/gptel-inline-completion-mode
  "Show gptel-powered inline completions in the current buffer."
  :lighter nil
  :keymap sthenno/gptel-inline-completion-mode-map
  (if sthenno/gptel-inline-completion-mode
      (progn
        (add-hook 'post-command-hook #'sthenno/gptel-inline--post-command nil t)
        (add-hook 'after-change-functions #'sthenno/gptel-inline--after-change nil t))
    (remove-hook 'post-command-hook #'sthenno/gptel-inline--post-command t)
    (remove-hook 'after-change-functions #'sthenno/gptel-inline--after-change t)
    (sthenno/gptel-inline--cancel)
    (sthenno/gptel-inline-completion-clear)
    (setq sthenno/gptel-inline--in-flight nil)))

(defun sthenno/gptel-inline-completion-setup ()
  "Enable gptel inline completion in the current buffer."
  (sthenno/gptel-inline-completion-mode 1))

(dolist (binding '(("TAB" . sthenno/gptel-inline-completion-tab)
                   ("<tab>" . sthenno/gptel-inline-completion-tab)))
  (keymap-global-set (car binding) (cdr binding))
  (keymap-set corfu-map (car binding) (cdr binding)))

(add-hook 'prog-mode-hook #'sthenno/gptel-inline-completion-setup)
(add-hook 'text-mode-hook #'sthenno/gptel-inline-completion-setup)

(provide 'init)
