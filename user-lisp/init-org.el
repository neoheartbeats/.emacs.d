;;; init-org.el --- Org and note-taking workflow -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains Org, Denote, and LaTeX preview settings.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(use-package org
  :ensure nil
  :init
  (setopt org-directory "/Users/sthenno/uncodified/"
          org-default-notes-file (expand-file-name "notes.org" org-directory)
          org-persist-directory (locate-user-emacs-file "org-persist/")
          org-startup-with-link-previews t
          org-ellipsis " …"
          org-use-property-inheritance t
          org-auto-align-tags nil
          org-tags-column 0
          org-hide-emphasis-markers t
          org-hide-macro-markers t
          org-special-ctrl-a/e t
          org-cycle-hide-drawer-startup t
          org-image-align 'left
          org-image-actual-width '(420)
          org-image-max-width 'fill-column
          org-yank-image-save-method (expand-file-name "images/" org-directory)
          org-yank-dnd-method 'ask
          org-attach-method 'cp
          org-return-follows-link t
          org-support-shift-select t
          org-link-search-must-match-exact-headline nil
          org-confirm-babel-evaluate nil
          org-src-preserve-indentation t
          org-edit-src-content-indentation 0
          org-edit-src-persistent-message nil
          org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-src-window-setup 'plain
          org-src-ask-before-returning-to-edit-buffer nil
          org-export-allow-bind-keywords t)
  :config
  (setf (cdr (assq 'file org-link-frame-setup)) #'find-file)
  (add-hook 'org-mode-hook #'org-fold-hide-drawer-all)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python . t)
                                 (latex . t)
                                 (shell . t))))

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (setopt org-modern-list '((?- . "•"))
          org-modern-checkbox '((?X . "􀃰")
                                (?- . "􀃞")
                                (?\s . "􀂒"))
          org-modern-timestamp '(" %Y-%m-%d " . " %H:%M ")
          org-modern-block-name '(("src" . ("􀃥" "􀃥"))
                                  ("quote" . ("􁈏" "􁈐"))
                                  (t . t))
          org-modern-keyword '(("title" . "􀫘")
                               ("results" . "􀎛")
                               (t . t))))

(use-package denote
  :ensure t
  :config
  (setopt denote-directory org-directory
          denote-file-type 'org
          denote-known-keywords '("stages" "silos" "images" "papers")
          denote-save-buffers t
          denote-kill-buffers t
          denote-open-link-function #'find-file
          denote-org-front-matter "#+title: %1$s\n\n"
          denote-buffer-name-prefix "[uncodified] "
          denote-rename-buffer-format "%D"
          denote-sort-dired-extra-prompts nil
          denote-sort-dired-default-sort-component 'title
          denote-sort-dired-default-reverse-sort t)
  (denote-rename-buffer-mode 1)
  (add-hook 'dired-mode-hook #'denote-dired-mode))

(use-package denote-org
  :ensure t
  :config
  (setopt denote-org-store-link-to-heading 'context)
  (defun sthenno/denote-org-path-sorted-notes (directory)
    "Return a list of note files in DIRECTORY, sorted by name."
    (sort (seq-filter #'denote-file-is-note-p
                      (directory-files directory t "\\.org$"))
          #'string<)))

(use-package denote-journal
  :ensure t
  :config
  (setopt denote-journal-title-format "%e %B %Y"
          denote-journal-directory (expand-file-name "stages/" denote-directory)
          denote-journal-keyword '("stages"))

  (defun sthenno/denote-journal-find-stages-file-date (offset)
    "Open the Denote journal file OFFSET positions away from the current one."
    (let* ((buffer-file (buffer-file-name))
           (sorted-files (sthenno/denote-org-path-sorted-notes
                          denote-journal-directory))
           (current-file-index (cl-position buffer-file sorted-files
                                            :test #'string=)))
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

  (keymap-set org-mode-map "s-<up>" #'sthenno/denote-journal-entry-previous)
  (keymap-set org-mode-map "s-<down>" #'sthenno/denote-journal-entry-next)
  (keymap-global-set "C-c d" #'denote-journal-new-or-existing-entry))

;;; [TODO] Experimental
(use-package org-latex-preview
  :disabled t
  :ensure nil
  :after org
  :hook (org-mode . org-latex-preview-mode)
  :init
  (setopt org-startup-with-latex-preview t
          org-latex-preview-process-active-indicator nil
          org-latex-preview-process-default 'dvisvgm
          org-latex-packages-alist '(("" "siunitx" t)
                                     ("boldermath" "newpx" t)
                                     ("" "bm" t)
                                     ("" "physics2" t)
                                     ("normal" "fixdif" t))
          org-highlight-latex-and-related '(native)
          org-use-sub-superscripts '{})
  :config
  (defun sthenno/org-preview-fragments ()
    "Refresh LaTeX and link previews in the current Org buffer."
    (interactive)
    (call-interactively #'org-latex-preview-clear-cache)
    (org-latex-preview 'buffer)
    (org-link-preview-refresh))

  (keymap-set org-mode-map "C-p" #'sthenno/org-preview-fragments)

  (let ((dvisvgm (alist-get 'dvisvgm org-latex-preview-process-alist))
        (libgs "/opt/homebrew/opt/ghostscript/lib/libgs.dylib"))
    (plist-put dvisvgm :image-converter
               `(,(concat "dvisvgm --page=1- --optimize --clipjoin --relative"
                          " --no-fonts --libgs=" libgs
                          " --bbox=preview -v4 -o %B-%%9p.svg %f"))))

  (let* ((physics2-modules '(("" "ab")
                             ("" "ab.braket")
                             ("" "diagmat")
                             ("" "xmat")))
         (physics2-preamble
          (concat
           (mapconcat
            (lambda (module-spec)
              (let ((options (car module-spec))
                    (module (cadr module-spec)))
                (if (string-empty-p options)
                    (format "\\usephysicsmodule{%s}" module)
                  (format "\\usephysicsmodule[%s]{%s}" options module))))
            physics2-modules
            "\n")
           "\n"))
         (default-preamble
          "\\documentclass{article}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\usepackage{xcolor}"))
    (setq org-latex-preview-preamble
          (concat default-preamble
                  "\n" physics2-preamble
                  "\\DeclareMathOperator*{\\argmax}{arg\\,max}\n"
                  "\\DeclareMathOperator*{\\argmin}{arg\\,min}")))

  (let ((factor (- (/ (face-attribute 'default :height) 100.0)
                   0.025)))
    (plist-put org-latex-preview-appearance-options :scale factor)
    (plist-put org-latex-preview-appearance-options :zoom factor)))

(provide 'init-org)
