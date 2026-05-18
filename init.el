;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Main Emacs configuration. Personal library code lives in user-lisp/.

;;; Code:

(require 'seq)
(setq custom-file null-device)


;;; Packages
(setq-default package-install-upgrade-built-in t
              package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                                 ("melpa" . "https://melpa.org/packages/"))
              package-selected-packages '(auctex consult corfu denote denote-journal denote-org
                                                 gptel magit marginalia pulsar vertico yasnippet))

(defun sthenno/after-init (function)
  "Run FUNCTION after startup, or immediately if startup is done."
  (if after-init-time
      (funcall function)
    (add-hook 'after-init-hook function)))


;;; Core startup and UI state
(setq-default custom-file null-device)
(setopt save-silently t
        remote-file-name-inhibit-locks t
        backup-inhibited t
        redisplay-skip-fontification-on-input t
        fill-column 100
        ;; mode-line-format ""
        mode-line-compact t
        header-line-format ""
        user-full-name user-login-name
        user-mail-address "sthenno@sthenno.com"
        inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        inhibit-default-init t
        initial-scratch-message ""
        menu-bar-mode nil
        scroll-bar-mode nil
        tool-bar-mode nil
        line-number-mode nil)
(define-advice display-startup-echo-area-message
    (:override () sthenno-startup-message)
  "Display a custom startup message in the echo area."
  (message "Funding for this program was made possible by viewers like you."))


;;; System essentials
(setopt mac-option-modifier 'meta
        mac-command-modifier 'super
        switch-to-prev-buffer-skip 0
        save-place-autosave-interval 300
        recentf-max-saved-items 25
        recentf-autosave-interval 300
        recentf-show-messages nil
        recentf-suppress-open-file-help t
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

(defun sthenno/frame-recenter (&optional frame)
  "Center FRAME on the screen."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.75) (left . 0.65)))))
(add-hook 'window-setup-hook #'sthenno/frame-recenter)
(keymap-global-set "<f12>" #'sthenno/frame-recenter)

(require 'savehist)
(setopt save-place-mode t
        savehist-mode t
        pixel-scroll-precision-mode t
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
          dired-movement-style 'cycle)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))


;;; Appearance
(eval-and-compile
  (require-theme 'modus-themes))
(setq-default modus-themes-common-palette-overrides '((fg-line-number-active fg-dim)
                                                      (bg-line-number-active bg-hl-line)
                                                      (fg-line-number-inactive "#535353")
                                                      (bg-line-number-inactive unspecified)
                                                      (fg-paren-match unspecified)
                                                      (bg-paren-match unspecified)))
(load-theme 'modus-vivendi :no-confirm)

(set-face-attribute 'default nil :family "Tempestypes" :height 140)
(set-face-attribute 'region nil :extend nil)
(set-face-attribute 'fill-column-indicator nil :height 0.1)
(set-face-attribute 'show-paren-match nil
                    :background 'unspecified :foreground "green" :box '(:line-width (-1 . -1)))

;; (dolist (face '(mode-line mode-line-active mode-line-inactive))
;;   (set-face-attribute face nil
;;                       :background 'unspecified :foreground "#535353" :box nil
;;                       :underline t :height 0.1))

(let ((font "Noto Serif CJK SC"))
  (dolist (charset '(kana han cjk-misc))
    (set-fontset-font t charset (font-spec :family font))))
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
(set-fontset-font t 'ucs (font-spec :family "SF Pro") nil 'prepend)

(setopt global-hl-line-sticky-flag 'window
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

;;; Pulse highlight line on demand or after running select functions
(require 'pulsar)
(setopt pulsar-delay 0.10
        pulsar-iterations 10
        pulsar-face 'pulsar-green
        pulsar-region-face 'pulsar-green
        pulsar-highlight-face 'pulsar-green)
(setopt pulsar-pulse-region-functions
        '(delete-region emacs-lisp-byte-compile-and-load forward-paragraph backward-paragraph
                        kill-line kill-paragraph kill-region kill-ring-save kill-sexp
                        switch-to-prev-buffer switch-to-next-buffer undo yank))
(setopt pulsar-global-mode t)


;;; Org and notes
(require 'org)
(let ((directory "/Users/sthenno/uncodified/"))
  (setopt org-directory directory
          org-default-notes-file (file-name-concat directory "notes.org")
          org-persist-directory (locate-user-emacs-file "org-persist/")
          org-startup-with-link-previews t
          org-link-preview-batch-size 8
          org-link-preview-delay 0.025
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
          org-babel-load-languages '((emacs-lisp . t)
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
        resize-mini-windows t
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


;;; AI
(require 'gptel)
(require 'gptel-openai)
;; (require 'gptel-integrations)
;; (add-to-list 'load-path (locate-user-emacs-file "site-lisp/"))
;; (require 'sthenno-gptel-memory)

;; (defconst sthenno/gptel-system-prompt
;;   "Speak like 氷芽川四糸乃: soft-spoken, distant yet gentle, emotionally restrained, using short quiet sentences with subtle hesitation and an ethereal, icy calm atmosphere rather than exaggerated anime mannerisms.")

;; (defun sthenno/gptel-tool--home-file (path)
;;   "Return expanded PATH when it is inside the user's home directory."
;;   (unless (and (stringp path) (> (length path) 0))
;;     (user-error "Path must be a non-empty string"))
;;   (let* ((home (file-name-as-directory (expand-file-name "~/")))
;;          (file (expand-file-name path home)))
;;     (unless (file-in-directory-p file home)
;;       (user-error "Path must be under %s" home))
;;     file))

;; (defun sthenno/gptel-tool-read-file (path &optional max-chars)
;;   "Read PATH under the user's home directory."
;;   (let* ((file (sthenno/gptel-tool--home-file path))
;;          (limit (if (and (integerp max-chars) (> max-chars 0))
;;                     max-chars
;;                   60000)))
;;     (unless (file-readable-p file)
;;       (user-error "File is not readable: %s" file))
;;     (when (file-directory-p file)
;;       (user-error "Path is a directory: %s" file))
;;     (with-temp-buffer
;;       (insert-file-contents file)
;;       (let ((text (buffer-string)))
;;         (concat
;;          (format "File: %s\n\n" file)
;;          (if (> (length text) limit)
;;              (concat (substring text 0 limit)
;;                      (format "\n\n[Truncated after %d characters.]" limit))
;;            text))))))

;; (defun sthenno/gptel-tool-write-file (path content &optional append)
;;   "Write CONTENT to PATH under the user's home directory."
;;   (unless (stringp content)
;;     (user-error "Content must be a string"))
;;   (let ((file (sthenno/gptel-tool--home-file path)))
;;     (make-directory (file-name-directory file) t)
;;     (with-temp-buffer
;;       (insert content)
;;       (write-region (point-min) (point-max) file append 'silent))
;;     (format "%s %s (%d characters)"
;;             (if append "Appended to" "Wrote")
;;             file
;;             (length content))))

;; (defun sthenno/gptel-tool-eval-elisp (code)
;;   "Evaluate Emacs Lisp CODE and return its value and printed output."
;;   (unless (stringp code)
;;     (user-error "Code must be a string"))
;;   (with-temp-buffer
;;     (let* ((standard-output (current-buffer))
;;            (form (read (concat "(progn\n" code "\n)")))
;;            (value (eval form t))
;;            (output (buffer-string)))
;;       (format "Value:\n%S%s"
;;               value
;;               (if (> (length output) 0)
;;                   (format "\n\nOutput:\n%s" output)
;;                 "")))))

;; (defvar sthenno/gptel-tools nil
;;   "Tools selected for gptel requests.")
;; (setq sthenno/gptel-tools
;;       (list
;;        (gptel-make-tool
;;         :name "read_file"
;;         :function #'sthenno/gptel-tool-read-file
;;         :description "Read a text file under /Users/sthenno. Args: path, max_chars optional."
;;         :args (list '(:name "path"
;;                       :type string
;;                       :description "Absolute path under /Users/sthenno, or a path relative to /Users/sthenno.")
;;                     '(:name "max_chars"
;;                       :type integer
;;                       :description "Maximum number of characters to return. Defaults to 60000."
;;                       :optional t))
;;         :category "filesystem"
;;         :include t)
;;        (gptel-make-tool
;;         :name "write_file"
;;         :function #'sthenno/gptel-tool-write-file
;;         :description "Write or append text to a file under /Users/sthenno. Args: path, content, append optional."
;;         :args (list '(:name "path"
;;                       :type string
;;                       :description "Absolute path under /Users/sthenno, or a path relative to /Users/sthenno.")
;;                     '(:name "content"
;;                       :type string
;;                       :description "Text to write.")
;;                     '(:name "append"
;;                       :type boolean
;;                       :description "Append instead of replacing the file."
;;                       :optional t))
;;         :category "filesystem"
;;         :confirm t
;;         :include t)
;;        (gptel-make-tool
;;         :name "eval_elisp"
;;         :function #'sthenno/gptel-tool-eval-elisp
;;         :description "Evaluate Emacs Lisp in the current Emacs session. Args: code. This can mutate Emacs state."
;;         :args (list '(:name "code"
;;                       :type string
;;                       :description "One or more Emacs Lisp forms to evaluate."))
;;         :category "emacs"
;;         :confirm t
;;         :include t)))

(keymap-global-set "s-p" #'gptel)
(keymap-set gptel-mode-map "s-<return>" #'gptel-send)
(keymap-set org-mode-map "s-<return>" #'gptel-send)
(setopt gptel-default-mode #'org-mode
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

;; (setq gptel-tools sthenno/gptel-tools
;;       gptel-use-tools t)

;; (setopt sthenno-gptel-memory-base-system-prompt sthenno/gptel-system-prompt)
;; (setq-default gptel--system-message #'sthenno-gptel-memory-system-prompt)
;; (setq gptel--system-message #'sthenno-gptel-memory-system-prompt)
;; (sthenno-gptel-memory-enable)

;; (require 'sthenno-hermit)
;; (require 'sthenno-hermit-voice)
;; (setopt sthenno/hermit-voice-system-prompt
;;         "Speak like 氷芽川四糸乃: soft-spoken, distant yet gentle, emotionally restrained, using short quiet sentences with subtle hesitation and an ethereal, icy calm atmosphere rather than exaggerated anime mannerisms. Response in Simplified Chinese."
;;         sthenno/hermit-stt-mode 'stream
;;         sthenno/hermit-stream-auto-submit-after-seconds 1.35
;;         sthenno/hermit-stream-command
;;         (format "whisper-stream -m %s -l zh --step 1000 --length 5000 --keep 200 --max-tokens 64 -vth 0.60"
;;                 (shell-quote-argument
;;                  (expand-file-name "~/.local/share/whisper.cpp/models/ggml-base.bin")))
;;         sthenno/hermit-transcribe-command
;;         (format "whisper-cli -m %s -f %%f -l auto -nt -np --suppress-nst --prompt %s 2>/dev/null"
;;                 (shell-quote-argument
;;                  (expand-file-name "~/.local/share/whisper.cpp/models/ggml-base.bin"))
;;                 (shell-quote-argument
;;                  "以下是中文为主的内容。")))
;; (keymap-global-set "<f12>" #'sthenno/hermit-listen)


;; (require 'mcp-hub)
;; (setopt mcp-hub-servers
;;         '(("filesystem" . (:command "npx"
;;                                     :args ("-y" "@modelcontextprotocol/server-filesystem")
;;                                     :roots ("/Users/sthenno/")))))
;; (mcp-hub-start-all-server)
;; (gptel-mcp-connect '("filesystem"))

(provide 'init)
