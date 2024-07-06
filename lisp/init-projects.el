;;; init-projects.el --- Project management -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; [TODO]
;;
;;; Code:
;;

(use-package project
  :init
  (setq project-prompter #'project-prompt-project-name)

  ;; From https://github.com/karthink/.emacs.d/blob/master/lisp/setup-project.el
  (setq project-switch-commands
        '((?f "Find file" project-find-file)
          (?d "Dired" project-dired)
          (?b "Buffer" project-switch-to-buffer)
          (?q "Query replace" project-query-replace-regexp)
          (?m "magit" project-magit-status)
          (?k "Kill buffers" project-kill-buffers)
          (?e "Eshell" project-eshell)))

  (defun project-magit-status ()
    "Run magit-status in the current project's root."
    (interactive)
    (magit-status-setup-buffer (project-root (project-current t))))

  ;;
  ;; [FIXME]
  ;;
  
  (defun my/project-remove-project ()
    "Remove project from `project--list' using completion."
    (interactive)
    (project--ensure-read-project-list)
    (let* ((projects project--list)
           (dir (completing-read "REMOVE project from list: " projects nil t)))
      (setq project--list (delete (assoc dir projects) projects))))

  :bind (:map global-map
              ("C-x p <backspace>" . my/project-remove-project)))


;; Git client using Magit
(use-package magit
  :straight t
  :config
  (setq magit-section-visibility-indicator '(" 􀰌"))
  (setq magit-diff-refine-hunk t)
  :bind ("C-x g" . magit-status))


;;; Xref
;;
;; For further backends support, see also `etags', `init-eglot'
;;

(use-package xref
  :init (setq xref-search-program 'ripgrep)
  :bind (:map global-map
              ("M-/" . xref-find-references)))

;; TAGS
;; (use-package etags
;;   :init

;;   ;; Automatically generate and update tags tables
;;   (add-hook 'emacs-lisp-mode-hook #'(lambda ()
;;                                       (etags-regen-mode 1))))

(provide 'init-projects)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
