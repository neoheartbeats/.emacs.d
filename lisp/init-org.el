;;; init-org.el --- Org Mode -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

(straight-use-package 'org)

;; Setup default directory
(setq org-directory "~/Sthenno/")

;; Org Mode buffer init behaviors
(setq
  org-startup-with-inline-images t
  org-startup-with-latex-preview t)

;; Install AUCTeX.
(use-package tex
  :straight auctex)


;; Modern Org Mode theme
(use-package org-modern
  :straight t
  :config
  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X . "􀃰") (?- . "􀃞") (?\s . "􀂒")))
  (setq org-modern-table-vertical 2)
  (setq org-modern-tag nil)
  (setq org-modern-block-name nil)
  (setq org-modern-keyword nil)
  (setq org-modern-block-fringe nil)
  (global-org-modern-mode 1))

;; External settings for `org-modern'
(setq org-ellipsis " 􀍠")
(setq org-hide-emphasis-markers t)
(setq org-auto-align-tags nil)
(setq org-tags-column 0)

;; Use this with `C-<return>'
(setq org-insert-heading-respect-content t)

;; Use this with `C-S-<return>'
(setq org-treat-insert-todo-heading-as-state-change t)

;; Prevent editing of text within folded subtree
(setq org-catch-invisible-edits 'show-and-error)

;; Better experiences jumping through headlines
(setq org-special-ctrl-a/e t)

;; Using the SF Pro font for symbols
(defun my/iconify-org-buffer ()
  (progn
    (push '("#+title:     " . ?􀈭) prettify-symbols-alist)
    (push '("#+identifier:" . ?􀅷) prettify-symbols-alist)
    (push '("#+date:      " . ?􀧵) prettify-symbols-alist)
    (push '("#+filetags:  " . ?􀋡) prettify-symbols-alist)
    (push '("#+begin_src"   . ?􀃤) prettify-symbols-alist)
    (push '("#+end_src" . ?􀅽) prettify-symbols-alist)
    (push '("#+begin_quote" . ?􀙤) prettify-symbols-alist)
    (push '("#+end_quote" . ?􀅽) prettify-symbols-alist)
    (push '("#+BEGIN: denote-links" .?􀋲) prettify-symbols-alist)
    (push '("#+END:" . ?􀅽) prettify-symbols-alist)
    (push '("#+RESULTS:" . ?􀎚) prettify-symbols-alist)
    (push '("SCHEDULED:" .?􀧞) prettify-symbols-alist)
    (push '("#+attr_org:" . ?􀌞) prettify-symbols-alist))
  (prettify-symbols-mode 1))
(add-hook 'org-mode-hook #'my/iconify-org-buffer)

;; Fold drawers by default
(setq org-cycle-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-fold-hide-drawer-all)

;; Fold titles by default
(setq org-startup-folded 'content)

;; Org fragments and overlays
(setq org-image-actual-width '(420))

;;; Org links
(setq org-return-follows-link t)

;; Open file links in current window
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;; Using shift-<arrow-keys> to select text
(setq org-support-shift-select t)


;; The Zettlekasten note-taking system by Denote
(use-package denote
  :straight t
  :config
  (setq denote-directory org-directory) ; Use `org-directory' as default
  (setq denote-known-keywords '(
                                 "robots"
				                         "poem"
				                         "sciences"
				                         "flashcards"
				                         "dust"
				                         "business"
				                         "billings"))
  (setq denote-save-buffer-after-creation t)

  ;; Denote for journaling
  (setq denote-journal-extras-directory
    (expand-file-name "stages/" denote-directory)) ; Subdirectory for journal files
  (setq denote-journal-extras-keyword "stages") ; Stages are journals
  (setq denote-journal-extras-title-format 'day-date-month-year)

  ;; Do not include date in notes
  (setq denote-org-front-matter
    "#+title:      %1$s
#+filetags:   %3$s
#+identifier: %4$s
\n")

  ;; No need confirmation using `denote-rename-file'
  (setq denote-rename-no-confirm t)
  :bind
  (:map global-map

    ;; Open today's note
    ("C-c d" . denote-journal-extras-new-or-existing-entry))
  (:map org-mode-map
    ("C-c i" . denote-link-or-create)
	  ("C-c b" . denote-backlinks)
	  ("C-c e" . denote-org-extras-extract-org-subtree))
  :hook (after-init . denote-journal-extras-new-or-existing-entry))

;; Extensions for Denote
(use-package denote-menu
  :straight t
  :config
  (setq denote-menu-title-column-width 45)

  ;; Remove denote journal entries from the menu
  (setq denote-menu-initial-regex
	  (mapconcat (lambda (keyword)
                 (concat "_" keyword))
		  denote-known-keywords "\\|"))
  :bind (:map org-mode-map
	        ("C-c m" . list-denotes)))

;; Custom functions for Denote
(defun my/denote-insert-links-current-month ()
  (interactive)
  (denote-add-links (format-time-string "%B")))

(defun my/denote-open-previous-file ()
  (interactive)
  (let* (
          (current-file (buffer-file-name))
          (directory (file-name-directory current-file))
          (files (directory-files directory t "\\`[^.]"))
          (sorted-files (sort files 'string<))
          (current-file-index (cl-position current-file sorted-files :test 'string=)))

    (when (and current-file-index (> current-file-index 0))
      (find-file (nth (1- current-file-index) sorted-files)))))

(defun my/denote-open-next-file ()
  (interactive)
  (let* (
          (current-file (buffer-file-name))
          (directory (file-name-directory current-file))
          (files (directory-files directory t "\\`[^.]"))
          (sorted-files (sort files 'string<))
          (current-file-index (cl-position current-file sorted-files :test 'string=)))

    (when (and current-file-index (< current-file-index (1- (length sorted-files))))
      (find-file (nth (1+ current-file-index) sorted-files)))))

(bind-keys :map org-mode-map
  ("s-<up>" . my/denote-open-previous-file)
  ("s-<down>" . my/denote-open-next-file))


;; Org LaTeX customizations
;; Use CDLaTeX to improve editing experiences
(use-package cdlatex
  :straight t
  :diminish (org-cdlatex-mode)
  :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))


;; Load languages for Org Babel

;; Do not ask for confirmation before executing
(setq
  org-link-elisp-confirm-function nil
  org-link-shell-confirm-function nil)

;; Org code blocks
(setq org-confirm-babel-evaluate nil)

(setq
  org-src-preserve-indentation t
  org-src-fontify-natively t
  org-src-tab-acts-natively t)

(org-babel-do-load-languages 'org-babel-load-languages
  '(
     (emacs-lisp . t)
     (python . t)))


;; Org-agenda
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(bind-keys :map global-map
	("C-c a" . org-agenda))

;; Org-agenda settings related to `org-modern'
(setq org-agenda-tags-column 0)
(setq org-agenda-block-separator ?─)
(setq org-agenda-time-grid
  '(
     (daily today require-timed)
	   (800 1000 1200 1400 1600 1800 2000)
     " ────── " "───────────────"))
(setq org-agenda-current-time-string
  "◀── now ─────────────────────────────────────────────────")


;; Org-Drill
;;
;; For `org-drill' specific files, see also `denote-known-keywords'
(use-package org-drill
  :straight t)


;; Useful functions
(defun my/org-mode-insert-get-button ()
  "Inserts a button that copies a user-defined string to clipboard."
  (interactive)
  (let ((content (read-string "Content: ")))
    (insert (format "[[elisp:(kill-new \"%s\")][GET]]" content))))


;; TTS implementation using OpenAI's API
(defun my/get_string_by_key_from_file (filename key)
  "Get content string of KEY from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (if (re-search-forward (format "^%s=\"\\([^\"]+\\)\"" key) nil t)
      (match-string 1)
      (error "Key %s not found in file %s" key filename))))

(defun my/get_environ_from_user_emacs_dir (key)
  "Get environ content by KEY from .env file in `user-emacs-directory'."
  (let ((filename (concat user-emacs-directory ".env")))
    (my/get_string_by_key_from_file filename key)))

(setq my/openai-api-key
  (my/get_environ_from_user_emacs_dir "OPENAI_API_KEY"))

(require 'json)

(defun my/speech-from-str-to-file (input-string output-file)
  "Send a text-to-speech request to the OpenAI API and save the
result to OUTPUT-FILE."
  (let* (
          (url "https://api.openai.com/v1/audio/speech")
          (url-request-method "POST")
          (url-request-extra-headers
            `(("Authorization" . ,(concat "Bearer " my/openai-api-key))
               ("Content-Type" . "application/json")))
          (url-request-data
            (json-encode `(("model" . "tts-1")
                            ("input" . ,input-string)
                            ("voice" . "echo"))))
          (buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n")
        (write-region (point) (point-max) output-file))
      (kill-buffer buffer))))

(defun my/generate-timestamp ()
  "Generate a timestamp in the format YYYYMMDDTHHMMSS."
  (format-time-string "%Y%m%dT%H%M%S"))

(setq my/speach-files-dir (concat org-directory "medi/"))

(defun my/speech-from-str-to-file-insert ()
  "Send the selected text to the OpenAI API and insert the result at
the point."
  (interactive)
  (if (use-region-p)
    (let* (
            (start (region-beginning))
	          (end (region-end))
	          (input-string (buffer-substring-no-properties start end))
	          (filename (concat my/speach-files-dir
			                  "speach-" (my/generate-timestamp) ".mp3"))
	          (button-string
	            (format "[[elisp:(emms-play-file \"%s\")][[􀊨]]]" filename)))
      (my/speech-from-str-to-file input-string filename)
	    (goto-char end)
      (insert (concat " " button-string))
	    (message (format "TTS finished to file %s" filename)))
    (message "No region selected")))

(bind-keys* :map org-mode-map
	("s-[ s" . my/speech-from-str-to-file-insert))

(provide 'init-org)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
