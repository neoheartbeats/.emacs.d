;;; early-init.el --- Pre-initialisation config -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Code loaded before the package system and GUI is initialized.
;;
;; This file includes:
;; - Init garbage-collection config
;; - Native-Compilation specified config
;; - Early file-loading behaviors
;; - Init GUI frames config

;;; Code:
;;

;;; XXX:
;; Correct PATH to fix homebrew-emacs-plus native-compile specific issues. This is
;; probably caused by the mismatching between version of `comp-libgccjit-version' and
;; GCC that macOS provides.
;;
;; Related issues:
;; - https://github.com/d12frosted/homebrew-emacs-plus/pull/687
;; - https://github.com/d12frosted/homebrew-emacs-plus/pull/492
;; - https://github.com/d12frosted/homebrew-emacs-plus/pull/542

(setenv "LIBRARY_PATH"
        (concat "/opt/homebrew/opt/gcc/lib/gcc/14:"
                "/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/14:"
                "/opt/homebrew/opt/libgccjit/lib/gcc/14"))

;; (setq load-path
;;       (delete "/opt/homebrew/Cellar/emacs-plus@30/30.0.60/share/emacs/30.0.60/lisp/org"
;;               load-path))


;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)


;;; Customize Native-Compilation
;;
;; To maximize the speed of native compilation
;;
(setq native-comp-speed 3)

;; By default any warnings encountered during async native compilation pops up. As this
;; tends to happen rather frequently with a lot of packages, it can get annoying.
(setq native-comp-async-report-warnings-errors nil)

;; To ensure `.eln' files are created correctly
(setq native-comp-async-query-on-exit t)
(setq native-compile-prune-cache t)

;; Do not enable packages during this early stage
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)


;;;
;;
;; HACK: Adjust display according to `file-name-handler-alist'. `eval-buffer' cannot run
;; here due to this snippet.
;;
;; SEE: https://github.com/karthink/.emacs.d/blob/master/early-init.el
;;

;; (unless (or (daemonp) noninteractive)
;;   (let ((old-file-name-handler-alist file-name-handler-alist))
;;     ;; `file-name-handler-alist' is consulted on each `require', `load' and
;;     ;; various path/io functions. You get a minor speed up by unsetting this.
;;     ;; Some warning, however: this could cause problems on builds of Emacs where
;;     ;; its site lisp files aren't byte-compiled and we're forced to load the
;;     ;; *.el.gz files (e.g. on Alpine).
;;     (setq-default file-name-handler-alist nil)
;;     ;; ...but restore `file-name-handler-alist' later, because it is needed for
;;     ;; handling encrypted or compressed files, among other things.
;;     (defun sthenno/reset-file-handler-alist ()
;;       (setq file-name-handler-alist
;;             ;; Merge instead of overwrite because there may have bene changes to
;;             ;; `file-name-handler-alist' since startup we want to preserve.
;;             (delete-dups (append file-name-handler-alist
;;                                  old-file-name-handler-alist))))
;;     (add-hook 'emacs-startup-hook #'sthenno/reset-file-handler-alist 101))

;;   (setq-default inhibit-redisplay t
;;                 inhibit-message t)
;;   (add-hook 'window-setup-hook
;;             (lambda ()
;;               (setq-default inhibit-redisplay nil
;;                             inhibit-message nil)
;;               (redisplay)))

;;   ;; Site files tend to use `load-file', which emits "Loading X..." messages in
;;   ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
;;   ;; substantial effect on startup times and in this case happens so early that
;;   ;; Emacs may flash white while starting up.
;;   (define-advice load-file (:override (file) silence)
;;     (load file nil 'nomessage))

;;   ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
;;   ;; may introduce down the road.
;;   (define-advice startup--load-user-init-file (:before (&rest _) nomessage-remove)
;;     (advice-remove #'load-file #'load-file@silence)))


;; Perform darwing the frame when initialization
;;
;; NOTE: `menu-bar-lines' is forced to redrawn under macOS GUI, therefore it is helpless
;; by inhibiting it in the early stage. However, since I dont't use the `menu-bar' under
;; macOS, `menu-bar-mode' is disabled later.

;;
;; (push '(menu-bar-lines . 0)     default-frame-alist)
;;
;; (add-hook 'after-init-hook #'(lambda ()
;;                                (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
;;                                (menu-bar-mode -1)))

(push '(tool-bar-lines . 0)     default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(vertical-scroll-bars)   default-frame-alist)

(setq tool-bar-mode nil
      scroll-bar-mode nil)

(push '(width  . 120) initial-frame-alist)
(push '(height . 50)  initial-frame-alist)

;; NOTE: Since feature `alpha-background' conflicts with Emacs overlay rendering for
;; images such as svg files, `alpha' can be a replacement for a rough approach.
;;
(push '(alpha . (90 . 90))           default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark)        default-frame-alist)
