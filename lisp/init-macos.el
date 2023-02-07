;;; init-macos.el --- Configure keys specific to macOS -*- lexical-binding: t -*-
;;; Commentary:

;; This file provides `macOS' system specific settings.

;;; Code:


;; macOS specified key mapping
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(setq default-input-method "chinese-py")

;; Enable those
(dolist (c '(narrow-to-region narrow-to-page upcase-region downcase-region))
  (put c 'disabled nil))


;; Smoother and nicer scrolling
(setq scroll-margin 15)
(setq scroll-step 1)
(setq scroll-conservatively 105)
(setq scroll-preserve-screen-position 'always)
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))

(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left"))
    (global-set-key
     (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))

;; Enable `pixel-scroll-precision-mode'
(pixel-scroll-precision-mode 1)

;; Disable auto copying
(setq mouse-drag-copy-region nil)

;; Disabe default key for scaling text
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))


;; Everything in UTF-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(setq search-default-mode 'char-fold-to-regexp)


;; macOS styled keybindings
;; Editing specified
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-x") 'kill-region)

(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "S-s-z") 'undo-redo)

;; Buffer specified
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-e") 'delete-window)
(global-set-key
 (kbd "s-w")
 (lambda ()
   (interactive)
   (kill-buffer (current-buffer))))
(global-set-key (kbd "<s-right>") 'switch-to-next-buffer)
(global-set-key (kbd "<s-left>") 'switch-to-prev-buffer)

;; Ignore these buffers while switching
(defcustom my-buffer-skip-regexp
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
  "Return `t' if BUFFER name matches `my-buffer-skip-regexp'."
  (string-match-p my-buffer-skip-regexp (buffer-name buffer)))

(setq switch-to-prev-buffer-skip 'pes-buffer-skip-p)

;; File management specified
(global-set-key (kbd "s-f") 'find-file)

;; Go to other windows easily with one keystroke `cmd-'
(global-set-key (kbd "s-1") (kbd "C-x 1")) ; cmd-1 kill other windows (keep 1)
(global-set-key (kbd "s-2") (kbd "C-x 2")) ; cmd-2 split horizontally
(global-set-key (kbd "s-3") (kbd "C-x 3")) ; cmd-3 split vertically


;; Disable these keys
(global-unset-key [swipe-left])
(global-unset-key [swipe-right])
(global-unset-key (kbd "C-z"))


(setq dired-use-ls-dired nil)


;; Speedup `regexp' searching
(setq-default xref-search-program 'ripgrep)
(setq-default shell-file-name "/bin/zsh")


;; System specified path
(defvar my-home-path "/Users/ilyaw39/")
(defvar my-dev-path "/Users/ilyaw39/Developer/")
(defvar my-org-path "/Users/ilyaw39/PLEASE/")

;; Homebrew specified path
(defvar my-hb-bin-path "/opt/homebrew/bin/")
(defvar my-hb-room-path "/opt/homebrew/Caskroom/")
(defvar my-python-exec-path "/opt/homebrew/bin/python3")


(provide 'init-macos)
;;; init-macos.el ends here
