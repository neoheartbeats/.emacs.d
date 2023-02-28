;; init-system.el --- Configs specific to macOS -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; This file provides `macOS' system specific settings.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; macOS specified key mapping
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(bind-keys
 ([(super a)] . mark-whole-buffer)
 ([(super c)] . kill-ring-save)
 ([(super l)] . goto-line)
 ([(super q)] . save-buffers-kill-emacs)
 ([(super s)] . save-buffer)
 ([(super v)] . yank)
 ([(super w)] . delete-frame)
 ([(super e)] . delete-window)
 ([(super z)] . undo)
 ([(super f)] . find-file))

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Increase how much is read from processes (default is 4kb)
(setq read-process-output-max #x10000) ; 64kb

;; Don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)
(setq command-line-ns-option-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Garbage Collector Magic Hack
(use-package
 gcmh
 :diminish (gcmh-mode)
 :hook (emacs-startup . gcmh-mode)
 :custom
 ((gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold most-positive-fixnum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set UTF-8 as the default coding system
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")
(set-selection-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better compatibilities
(use-package compat :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Locate position history
(use-package saveplace :ensure nil :hook (after-init . save-place-mode))
(use-package
 savehist
 :ensure nil
 :hook (after-init . savehist-mode)
 :init
 (setq
  enable-recursive-minibuffers t ; Allow commands in minibuffers
  history-length 1000
  savehist-additional-variables
  '(mark-ring
    global-mark-ring search-ring regexp-search-ring extended-command-history)
  savehist-autosave-interval 300))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The rules of minimalism
(use-package
 simple
 :ensure nil
 :hook
 ((after-init . size-indication-mode)
  (text-mode . visual-line-mode)
  ((prog-mode conf-mode) . enable-trailing-whitespace))
 :init
 (setq
  column-number-mode t
  line-number-mode t
  line-move-visual nil
  track-eol t ; Keep cursor at end of lines. Require `line-move-visual' is nil.
  set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again

 ;; Trailing TAB, (HARD) SPACE
 (setq-default show-trailing-whitespace nil) ; don't show trailing whitespace by default
 (defun enable-trailing-whitespace ()
   "Show trailing spaces and delete on saving."
   (setq show-trailing-whitespace t)
   (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Misc
(setq use-short-answers t)
(setq dired-use-ls-dired nil)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(setq-default major-mode 'org-mode)
(setq-default fill-column 80)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil) ; permanently indent with spaces, no TABs

(setq inhibit-compacting-font-caches t) ; don’t compact font caches during GC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global keybindings
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p
         (format "Really delete '%s'?"
                 (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mouse and scroll settings
;;
;; Smoother and nicer scrolling
(setq scroll-step 1)
(setq scroll-conservatively 15)
(setq scroll-margin 15)
(setq scroll-preserve-screen-position 'always)
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))

(add-hook 'after-init-hook #'(lambda () (pixel-scroll-precision-mode 1)))

;; Disable auto copying
(setq mouse-drag-copy-region nil)
(setq search-default-mode 'char-fold-to-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Buffer specified
;;
;; Ignore these buffers while switching
(defcustom pes-buffer-skip-regexp
  (rx
   bos
   (or (or "*scratch*"
           "*Messages*"
           "*Help*"
           "Warning"
           "*Native-compile-Log*"
           "*Compile-Log*"
           "*Async-native-compile-log*"
           "*straight-process*"
           "*Org Preview LaTeX Output*")
       (seq "magit-diff" (zero-or-more anything))
       (seq "magit-process" (zero-or-more anything))
       (seq "magit-revision" (zero-or-more anything))
       (seq "magit-stash" (zero-or-more anything)))
   eos)
  "Regexp matching buffers ignored while switching buffers."
  :type 'regexp)

(defun pes-buffer-skip-p (window buffer bury-or-kill)
  "Return `t' if BUFFER name matches `pes-buffer-skip-regexp'."
  (string-match-p pes-buffer-skip-regexp (buffer-name buffer)))

(setq switch-to-prev-buffer-skip #'pes-buffer-skip-p)

;; Go to other windows easily with one keystroke `cmd-'
(global-set-key (kbd "s-1") (kbd "C-x 1")) ; cmd-1 kill other windows (keep 1)
(global-set-key (kbd "s-2") (kbd "C-x 2")) ; cmd-2 split horizontally
(global-set-key (kbd "s-3") (kbd "C-x 3")) ; cmd-3 split vertically

(global-set-key (kbd "<s-right>") 'switch-to-next-buffer)
(global-set-key (kbd "<s-left>") 'switch-to-prev-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Speedup `regexp' searching
(setq-default xref-search-program 'ripgrep)
(setq-default shell-file-name "/bin/zsh")


;; System specified path
(defvar pes-home-path "/Users/ilyaw39/")

(defvar pes-dev-path "/Users/ilyaw39/Developer/")
(defvar pes-org-path "/Users/ilyaw39/Developer/LutwidgeTown/")

;; Homebrew specified path
(defvar pes-hb-bin-path "/opt/homebrew/bin/")
(defvar pes-hb-room-path "/opt/homebrew/Caskroom/")


(provide 'init-system)
;;; init-macos.el ends here
