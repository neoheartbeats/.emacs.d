;;; init-org.el --- Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This config is currently for a patched version of Org that is under development.
;; See https://code.tecosaur.net/tec/org-mode for more details.

;;; Code:
;;

;;; Setup default directory
(setopt org-directory "/Users/sthenno/uncodified/")
(setopt org-persist-directory (locate-user-emacs-file "org-persist/"))

;;; Org Mode buffer init behaviors
(setopt org-startup-with-link-previews t)

;;; Modern Org mode theme
(use-package org-modern
  :ensure t
  :config
  (setopt org-modern-label-border 0.25)
  (setopt org-modern-list '((?- . "•")))
  (setopt org-modern-checkbox '((?X  . "􀃰")
                                (?-  . "􀃞")
                                (?\s . "􀂒")))
  (setopt org-modern-timestamp '(" %Y-%m-%d " . " %H:%M "))
  (setopt org-modern-block-name
          '(("src"   . ("􀃥" "􀃥"))
            ("quote" . ("􁈏" "􁈐"))
            (t . t)))
  (setopt org-modern-keyword '(("title"   . "􀫘")
                               ("results" . "􀎛")
                               (t . t)))
  ;; Hooks
  (add-hook 'org-mode-hook #'(lambda ()
                               (org-modern-mode 1))))

;; External settings for `org-modern'
(setopt org-ellipsis " …")
(setopt org-use-property-inheritance t)
(setopt org-auto-align-tags nil)
(setopt org-tags-column 0)
(setopt org-hide-emphasis-markers t
        org-hide-macro-markers t)

;; Better experiences jumping through headlines
(setopt org-special-ctrl-a/e t)

;; Fold drawers by default
(setopt org-cycle-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-fold-hide-drawer-all)

;;; Org fragments and overlays

;; Org images
(setopt org-image-align 'left
        org-image-actual-width '(420)
        org-image-max-width 'fill-column)

(setopt org-yank-image-save-method (expand-file-name "images/" org-directory))
(setopt org-yank-dnd-default-attach-method 'cp)

;;; Org Hyperlinks
(setopt org-return-follows-link t)

;;; Open file links in current window
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;;; Using shift-<arrow-keys> to select text
(setopt org-support-shift-select t)

;;; The Zettlekasten note-taking system by Denote
(use-package denote
  :ensure t
  :config
  (setopt denote-directory org-directory)
  (setopt denote-file-type 'org)
  (setopt denote-known-keywords '("stages" "silos" "images" "papers"))
  (setopt denote-save-buffers t
          denote-kill-buffers t)
  (setopt denote-open-link-function #'find-file)

  ;; Do not include date, tags and ids in note files
  (setopt denote-org-front-matter "#+title: %1$s\n\n")

  (denote-rename-buffer-mode 1)
  (setopt denote-buffer-name-prefix "[uncodified] ")
  (setopt denote-rename-buffer-format "%D")

  ;; Do not issue any extra prompts. Always sort by the `title' file name component and
  ;; never do a reverse sort.
  (setopt denote-sort-dired-extra-prompts nil
          denote-sort-dired-default-sort-component 'title
          denote-sort-dired-default-reverse-sort t)

  ;; Hooks
  (add-hook 'dired-mode-hook #'denote-dired-mode))

(use-package denote-org
  :ensure t
  :config
  (setopt denote-org-store-link-to-heading 'context)
  (defun sthenno/denote-org-path-sorted-notes (directory)
    "Return a list of note files in DIRECTORY, sorted by name."
    (sort (seq-filter 'denote-file-is-note-p
                      (directory-files directory t "\\.org$"))
          'string<)))

(use-package denote-journal
  :ensure t
  :config
  (setopt denote-journal-title-format "%e %B %Y"
          denote-journal-directory (expand-file-name "stages/" denote-directory)
          denote-journal-keyword '("stages")) ; Stages are journals

  ;; Helper functions
  (defun sthenno/denote-journal-find-stages-file-date (length)
    "Open the denote note file in the given LENGTH."
    (let* ((buff (buffer-file-name))
           (sorted-files (sthenno/denote-org-path-sorted-notes denote-journal-directory))
           (current-file-index (cl-position buff sorted-files :test 'string=)))
      (if (null current-file-index)
          (message "Current file is not a note file.")
        (let ((idx (+ current-file-index length)))
          (if (or (< idx 0)
                  (>= idx (length sorted-files)))
              (message "No denote note file.")
            (find-file (nth idx sorted-files)))))))

  (defun sthenno/denote-journal-entry-previous ()
    "Open the previous note file in the current directory."
    (interactive)
    (sthenno/denote-journal-find-stages-file-date -1))

  (defun sthenno/denote-journal-entry-next ()
    "Open the next note file in the current directory."
    (interactive)
    (sthenno/denote-journal-find-stages-file-date 1))

  (keymap-set org-mode-map "s-<up>" #'sthenno/denote-journal-entry-previous)
  (keymap-set org-mode-map "s-<down>" #'sthenno/denote-journal-entry-next)

  (keymap-global-set "C-c d" #'denote-journal-new-or-existing-entry))

;;; Load languages for Org Babel

;; Do not ask for confirmation before executing
(setopt org-link-elisp-confirm-function nil
        org-link-shell-confirm-function nil)
(setopt org-link-search-must-match-exact-headline nil)

;; Org code blocks
(setopt org-confirm-babel-evaluate nil)

(setopt org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-edit-src-persistent-message nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'plain
        org-src-ask-before-returning-to-edit-buffer nil)

;; Enable these languages for Org-Babel
(setopt org-babel-load-languages '((emacs-lisp . t)
                                   (python . t)
                                   (latex . t)
                                   (shell . t)))

;;; Export
(setopt org-export-allow-bind-keywords t)

(provide 'init-org)
