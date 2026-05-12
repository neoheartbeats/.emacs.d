;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Main Emacs configuration.  Personal library code lives in user-lisp/.

;;; Code:

(require 'package)
(require 'seq)

(autoload 'dired-hide-details-mode "dired" nil t)


;;; Packages
(setopt package-native-compile t
        package-install-upgrade-built-in t
        package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))
        package-selected-packages '(auctex consult corfu
                                           denote denote-journal denote-org
                                           gptel magit marginalia vertico yasnippet))

(unless (bound-and-true-p package--activated)
  (package-activate-all))

(defun sthenno/package-missing-packages ()
  "Return selected packages that are not installed."
  (seq-remove #'package-installed-p package-selected-packages))

(defun sthenno/package-install-selected ()
  "Install missing packages from `package-selected-packages'."
  (let ((missing (sthenno/package-missing-packages)))
    (when missing
      (unless package--initialized
        (package-initialize t))
      (unless (seq-every-p (lambda (package)
                             (assq package package-archive-contents))
                           missing)
        (package-refresh-contents))
      (package-install-selected-packages t))))
(sthenno/package-install-selected)

(defun sthenno/after-init (function)
  "Run FUNCTION after startup, or immediately if startup is done."
  (if after-init-time
      (funcall function)
    (add-hook 'after-init-hook function)))


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
        initial-scratch-message "Hi!"
        menu-bar-mode nil
        scroll-bar-mode nil
        tool-bar-mode nil
        line-number-mode nil)
(define-advice display-startup-echo-area-message
    (:override () sthenno-startup-message)
  "Display a custom startup message in the echo area."
  (message "Funding for this program was made possible by viewers like you."))


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
        save-interprogram-paste-before-kill t
        abbrev-mode t)

(require 'savehist)
(setopt save-place-mode t
        savehist-mode t
        pixel-scroll-precision-mode t
        electric-pair-mode t
        electric-indent-mode t
        delete-selection-mode t)

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

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(with-eval-after-load 'dired
  (setopt dired-no-confirm t
          dired-use-ls-dired t
          dired-hide-details-hide-information-lines t
          dired-hide-details-hide-absolute-location t
          dired-check-symlinks nil
          dired-recursive-deletes 'always
          dired-movement-style 'cycle))


;;; Appearance

(eval-and-compile
  (require-theme 'modus-themes))
(setopt modus-themes-common-palette-overrides `((fg-line-number-active fg-dim)
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
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-vivendi :no-confirm)

(set-face-attribute 'default nil :family "Tempestypes" :height 140 :weight 'light)
(set-face-attribute 'region nil :extend nil)
(set-face-attribute 'fill-column-indicator nil :height 0.1)
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'italic nil :slant 'normal)
(set-face-attribute 'show-paren-match nil
                    :foreground "green" :box '(:line-width (-1 . -1)))
(dolist (face '(mode-line mode-line-active mode-line-inactive))
  (set-face-attribute face nil
                      :background 'unspecified :foreground "#535353" :box nil
                      :underline t :height 0.1))

(let ((font "PingFang SC"))
  (dolist (charset '(kana han cjk-misc))
    (set-fontset-font t charset (font-spec :family font))))
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
(set-fontset-font t 'ucs (font-spec :family "SF Pro") nil 'prepend)

(setopt global-hl-line-sticky-flag 'window
        x-stretch-cursor t
        cursor-type '(bar . 1)
        display-line-numbers-widen t
        display-line-numbers-width 6
        display-fill-column-indicator-warning t
        show-paren-delay 0.05
        show-paren-when-point-inside-paren t
        show-paren-context-when-offscreen t
        show-paren-not-in-comments-or-strings 'on-mismatch
        default-input-method nil)

(setopt global-hl-line-mode t
        global-display-fill-column-indicator-mode t
        show-paren-mode t
        blink-cursor-mode nil)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)


;;; Org and notes

(require 'org)
(let ((directory "/Users/sthenno/uncodified/"))
  (setopt org-directory directory
          org-default-notes-file (file-name-concat directory "notes.org")
          org-persist-directory (locate-user-emacs-file "org-persist/")
          org-startup-with-link-previews t
          org-link-preview-batch-size 8
          org-link-preview-delay 0.15
          org-link-elisp-confirm-function nil
          org-startup-truncated t
          org-use-property-inheritance t
          org-babel-uppercase-example-markers t
          org-hide-emphasis-markers t
          org-hide-macro-markers t
          org-hide-drawer-startup t
          org-special-ctrl-a t
          org-image-align 'left
          org-image-max-width 'fill-column
          org-yank-image-save-method (file-name-concat directory "images")
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
          org-export-allow-bind-keywords t
          org-babel-update-intermediate t
          org-babel-load-languages '((dot . t)
                                     (emacs-lisp . t)
                                     (python . t)
                                     (shell . t))))


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
        denote-journal-keyword "stages"
        initial-buffer-choice #'denote-journal-new-or-existing-entry)

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
(keymap-global-set "C-c d" #'denote-journal-new-or-existing-entry)


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
(sthenno/after-init #'yas-global-mode)


;;; Completion and minibuffer

(setopt completion-cycle-threshold nil
        text-mode-ispell-word-completion 'completion-at-point
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
        completion-eager-display 'auto
        completion-eager-update 'auto
        completion-styles '(basic flex)
        completion-ignore-case nil
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        file-name-shadow-mode t)

(require 'vertico)
(require 'vertico-directory)
(setopt vertico-resize t
        vertico-scroll-margin 4
        vertico-cycle nil
        vertico-count-format (cons "[ %-6s ] " "%s of %s"))
(keymap-set vertico-map "<tab>" #'vertico-insert)
(keymap-set vertico-map "<return>" #'vertico-directory-enter)
(keymap-set vertico-map "<backspace>" #'vertico-directory-delete-char)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(sthenno/after-init #'vertico-mode)

(require 'marginalia)
(sthenno/after-init #'marginalia-mode)

(require 'consult)
(keymap-global-set "s-b" #'consult-buffer)
(keymap-global-set "C-s" #'consult-line)
(keymap-global-set "s-;" #'consult-goto-line)
(keymap-global-set "C-v" #'consult-yank-pop)
(keymap-global-set "s-m" #'consult-imenu-multi)
(keymap-global-set "s-n" #'consult-recent-file)
(keymap-global-set "M-i" #'consult-info)
(keymap-global-set "M-s" #'consult-ripgrep)
(keymap-set consult-narrow-map "?" #'consult-narrow-help)
(setq completion-in-region-function #'consult-completion-in-region)
(setopt register-preview-delay 0.125
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
(keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)

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
        ispell-save-corrections-as-abbrevs t
        flyspell-delay-use-timer t
        flyspell-mode t
        dictionary-server nil)
(keymap-set org-mode-map "M-." #'dictionary-lookup-definition)

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
(sthenno/after-init #'global-corfu-mode)
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'corfu-history))
(with-eval-after-load 'completion-preview
  (setopt completion-preview-sort-function corfu-sort-function))


;;; Languages
(setopt treesit-enabled-modes t
        treesit-auto-install-grammar 'ask)
(dolist (hook '(LaTeX-mode-hook tex-mode-hook python-base-mode-hook))
  (add-hook hook #'eglot-ensure))
(with-eval-after-load 'python
  (setopt python-indent-offset 4
          python-indent-guess-indent-offset nil
          python-indent-guess-indent-offset-verbose nil))


;;; AI and Hermit
(eval-and-compile
  (add-to-list 'load-path (locate-user-emacs-file "user-lisp")))
(require 'gptel)
(keymap-global-set "s-p" #'gptel)
(keymap-set gptel-mode-map "s-<return>" #'gptel-send)
(keymap-set org-mode-map "s-<return>" #'gptel-send)
(setopt gptel-default-mode #'org-mode
        gptel-org-branching-context t
        gptel-model 'sthenno
        gptel-max-tokens 32768
        gptel-temperature 0.70
        gptel-backend (gptel-make-openai "sthenno"
                        :protocol "http"
                        :host "192.168.100.207:8000"
                        :endpoint "/v1/chat/completions"
                        :stream t
                        :key "sk-tmp"
                        :models '(sthenno)
                        :request-params '( :top_p 0.80
                                           :presence_penalty 1.5
                                           :top_k 20
                                           :chat_template_kwargs
                                           (:enable_thinking :json-false))))

(require 'sthenno-hermit)
(require 'sthenno-hermit-voice)
(setopt sthenno/hermit-voice-system-prompt
        "Speak like 氷芽川四糸乃: soft-spoken, distant yet gentle, emotionally restrained, using short quiet sentences with subtle hesitation and an ethereal, icy calm atmosphere rather than exaggerated anime mannerisms. Response in Simplified Chinese."
        sthenno/hermit-stt-mode 'stream
        sthenno/hermit-stream-auto-submit-after-seconds 1.35
        sthenno/hermit-stream-command
        (format "whisper-stream -m %s -l zh --step 1000 --length 5000 --keep 200 --max-tokens 64 -vth 0.60"
                (shell-quote-argument
                 (expand-file-name "~/.local/share/whisper.cpp/models/ggml-base.bin")))
        sthenno/hermit-transcribe-command
        (format "whisper-cli -m %s -f %%f -l auto -nt -np --suppress-nst --prompt %s 2>/dev/null"
                (shell-quote-argument
                 (expand-file-name "~/.local/share/whisper.cpp/models/ggml-base.bin"))
                (shell-quote-argument
                 "以下是中文为主的内容。")))
(keymap-global-set "<f12>" #'sthenno/hermit-listen)


;;; Customizations
(when (file-exists-p custom-file)       ;
  (load custom-file :noerror :nomessage))

(provide 'init)
