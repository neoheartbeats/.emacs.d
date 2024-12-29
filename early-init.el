;;; early-init.el --- pre-initialisation config -*- no-byte-compile: t; -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Code loaded before the package system and GUI is initialized.

;; This file includes:
;;   - Init garbage-collection config
;;   - Native-Compilation specified config
;;   - Early file-loading behaviors
;;   - Init GUI frames config

;;; Code:

;;; Turn off GC during startup
(setq gc-cons-threshold (* 1024 1024 1024)
      gc-cons-percentage 1.0)

(setq-default emacs-startup-hook nil
              default-frame-alist nil)

;;; Adjust display according to `file-name-handler-alist'

(unless (or (not (called-interactively-p))
            (daemonp)
            noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)
    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun sthenno/reset-file-handler-alist ()
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'sthenno/reset-file-handler-alist 101)))

;; Site files tend to use `load-file', which emits "Loading X..." messages in the echo
;; area, which in turn triggers a redisplay. Redisplays can have a substantial effect on
;; startup times and in this case happens so early that Emacs may flash white while
;; starting up.
(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))

;; Undo our `load-file' advice above, to limit the scope of any edge cases it may
;; introduce down the road.
(define-advice startup--load-user-init-file (:before (&rest _) nomessage-remove)
  (advice-remove #'load-file #'load-file@silence))

;;; Customize Native-Compilation and caching
(defvar user-cache-directory "~/.cache/emacs/"
  "Location that files created by Emacs are placed.")

;; By default any warnings encountered during async native compilation pops up. As this
;; tends to happen rather frequently with a lot of packages, it can get annoying.
(setq native-comp-async-report-warnings-errors 'silent)

;;; Perform drawing the frame when initialization

;; NOTE: `menu-bar-lines' is forced to redrawn under macOS GUI, therefore it is helpless
;; by inhibiting it in the early stage. However, since I don't use the `menu-bar' under
;; macOS, `menu-bar-mode' is disabled later.

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(push '(width  . 100) default-frame-alist)
(push '(height . 45) default-frame-alist)
(push '(alpha  . (90 . 90)) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)

;;; _
(provide 'early-init)
