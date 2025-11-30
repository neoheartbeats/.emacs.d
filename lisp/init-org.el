;;; init-org.el --- Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This config is currently for a patched version of Org that is under development.
;; See https://code.tecosaur.net/tec/org-mode for more details.
;;
;; This file includes:
;;
;; - Org Mode basics
;; - TEC's `org-latex-preview' specific configurations
;; - Modern Org Mode
;; - Note-taking system using `denote'

;;; Code:
;;

;;; Setup default directory
;; (setq org-directory "/Users/sthenno/Tempestissimo/soulin/")
(setq org-directory "/Users/sthenno/安安素描本/")
(setq org-persist-directory (locate-user-emacs-file "org-persist/"))

;;; Org Mode buffer init behaviors
(setq org-startup-with-link-previews t)
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)

;; Fold titles by default
;; (setq org-startup-folded 'content)

;;; Install AUCTeX
(use-package tex :ensure auctex)

;; Use CDLaTeX to improve editing experiences
(use-package cdlatex
  :ensure t
  :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

;; Add “libgs” to `org-latex-preview-process-alist'
(setq org-latex-preview-process-default 'dvisvgm)
(let ((dvisvgm (alist-get 'dvisvgm org-latex-preview-process-alist))
      (libgs "/opt/homebrew/opt/ghostscript/lib/libgs.dylib"))
  (plist-put dvisvgm :image-converter
             `(,(concat "dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts"
                        " --libgs=" libgs
                        " --bbox=preview -v4 -o %B-%%9p.svg %f"))))

;; Enable `org-latex-preview-mode'
(add-hook 'org-mode-hook #'(lambda ()
                             (org-latex-preview-mode 1)))

;; Preview functions
(defun sthenno/org-preview-fragments ()
  (interactive)
  (call-interactively 'org-latex-preview-clear-cache)
  (org-latex-preview 'buffer)
  (org-link-preview-refresh))
(keymap-set org-mode-map "C-p" #'sthenno/org-preview-fragments)

(setq org-latex-packages-alist '(("" "siunitx"   t)

                                 ;; https://ctan.org/pkg/newtx
                                 ("libertinus" "newtx" t)

                                 ;; Load this after all math to give access to bold math
                                 ("" "bm" t)

                                 ;; Package physics2 requires to be loaded after font
                                 ;; packages. See https://ctan.org/pkg/physics2
                                 ("" "physics2" t)

                                 ;; Typeset the differential operators
                                 ("normal" "fixdif" t)))

;; Add additional modules required by LaTeX packages like physics2 to the preamble
(let* ((physics2-modules '(("" "ab")
                           ("" "ab.braket")
                           ("" "diagmat")
                           ("" "xmat")))
       (physics2-preamble (concat (mapconcat
                                   (lambda (m)
                                     (let ((options (car  m))
                                           (module  (cadr m)))
                                       (if (string= options "")
                                           (format "\\usephysicsmodule{%s}" module)
                                         (format "\\usephysicsmodule[%s]{%s}" options module))))
                                   physics2-modules
                                   "\n")
                                  "\n"))
       (default-preamble "\\documentclass{article}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\usepackage{xcolor}"))
  (setq org-latex-preview-preamble
        (concat default-preamble
                "\n" physics2-preamble
                "\\DeclareMathOperator*{\\argmax}{arg\\,max}\n"
                "\\DeclareMathOperator*{\\argmin}{arg\\,min}")))

(setq org-highlight-latex-and-related '(native)) ; Highlight inline LaTeX code
(setq org-use-sub-superscripts '{})
;; (setq org-pretty-entities t
;;       org-pretty-entities-include-sub-superscripts nil)

(let ((factor (- (/ (face-attribute 'default :height)
                    100.0)
                 0.025)))
  (plist-put org-latex-preview-appearance-options :scale factor)
  (plist-put org-latex-preview-appearance-options :zoom  factor))

;;; Modern Org mode theme
(use-package org-modern
  :ensure t
  :config
  (setq org-modern-label-border 0.25)
  ;; (setq org-modern-star 'replace)
  ;; (let ((stars "•"))
  ;;   (setq org-modern-replace-stars stars
  ;;         org-modern-hide-stars stars))
  ;; (setq org-modern-list '((?- . "•")))
  (setq org-modern-list nil)
  ;; (setq org-modern-checkbox '((?X  . "􀃰")
  ;;                             (?-  . "􀃞")
  ;;                             (?\s . "􀂒")))
  (setq org-modern-checkbox nil)
  (setq org-modern-todo nil)
  (setq org-modern-timestamp '(" %Y-%m-%d " . " %H:%M "))
  (setq org-modern-block-name
        '(("src"   . ("􀃥" "􀃥"))
          ("quote" . ("􁈏" "􁈐"))
          (t . t)))
  (setq org-modern-keyword '(("title"   . "􀫘")
                             ("results" . "􀎛")
                             (t . "‣ ")))
  ;; Hooks
  (add-hook 'org-mode-hook #'(lambda ()
                               (org-modern-mode 1))))

;; External settings for `org-modern'
(setq org-ellipsis " …")
(setq org-use-property-inheritance t)
(setq org-auto-align-tags nil)
(setq org-tags-column 0)
(setq org-hide-emphasis-markers t
      org-hide-macro-markers t)

;; Use this with "C-<return>"
(setq org-insert-heading-respect-content t)

;; Use this with "C-S-<return>"
(setq org-treat-insert-todo-heading-as-state-change t)

;; Better experiences jumping through headlines
(setq org-special-ctrl-a/e t)

;; Fold drawers by default
(setq org-cycle-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-fold-hide-drawer-all)

;;; Org fragments and overlays

;; Org images

(setq org-image-align 'left
      org-image-actual-width '(420)
      org-image-max-width 'fill-column)

;; (setq org-yank-dnd-method 'file-link)
;; (setq org-yank-image-save-method (expand-file-name "silos/" org-directory))

;;; Org Hyperlinks
(setq org-return-follows-link t)

;;; Open file links in current window
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;;; Using shift-<arrow-keys> to select text
(setq org-support-shift-select 'always) ; Everywhere except timestamps
(setq org-use-fast-todo-selection 'expert)

;;; The Zettlekasten note-taking system by Denote
(use-package denote
  :ensure t
  :config
  (setq denote-directory org-directory)
  (setq denote-file-type 'org)
  (setq denote-known-keywords
        '("stages" "silos" "images" "papers" "production" "technology")
        denote-infer-keywords t)
  (setq denote-save-buffers t
        denote-kill-buffers t)
  (setq denote-open-link-function #'find-file)

  ;; Image handling with `yank-media'
  (defvar sthenno/denote-images-directory "images"
    "Relative path to the images subdirectory in the Denote root.")

  (defun sthenno/denote-convert-to-png (data original-mimetype target-path)
    "Add DATA to TARGET-PATH. Convert to PNG if ORIGINAL-MIMETYPE is not png."
    (let* ((is-png (string= original-mimetype "image/png"))
           (temp-file (make-temp-file "emacs-yank-img-" nil
                                      (if is-png ".png" ".tmp"))))
      (with-temp-file temp-file
        (set-buffer-multibyte nil)
        (insert data))
      (if is-png
          (rename-file temp-file target-path t)
        (if (executable-find "convert")
            (progn
              (call-process "convert" nil nil nil temp-file target-path)
              (delete-file temp-file))
          (delete-file temp-file)
          (error "Image is not PNG and `convert' (ImageMagick) tool is missing")))))

  (defun sthenno/denote-yank-image-handler (mimetype data)
    "Save image as PNG to denote images folder and insert link."
    (let* ((denote-images-path
            (expand-file-name sthenno/denote-images-directory (denote-directory)))
           (id (format-time-string denote-id-format))
           (image-title (read-string "Image title: "))
           (slug (denote-sluggify-title image-title))
           (keyword "images")
           (filename (if (string-empty-p slug)
                         (format "%s__%s.png" id keyword)
                       (format "%s--%s__%s.png" id slug keyword)))
           (abs-filepath (expand-file-name filename denote-images-path))
           (image-path (file-relative-name abs-filepath
                                           (file-name-directory buffer-file-name))))
      (unless (file-exists-p denote-images-path)
        (make-directory denote-images-path t))
      (condition-case err
          (progn
            (sthenno/denote-convert-to-png data mimetype abs-filepath)
            (insert (format "#+attr_org: :width 240px\n[[%s]]" image-path))
            (org-display-inline-images)
            (message "Saved and converted to %s" image-path))
        (error
         (message "Failed to save image: %s" (error-message-string err))))))

  (defun sthenno/setup-org-yank-media ()
    "Register the image handler for all image types in Org-mode."
    (yank-media-handler "image/.*" #'sthenno/denote-yank-image-handler))
  (add-hook 'org-mode-hook #'sthenno/setup-org-yank-media)
  (keymap-set org-mode-map "s-." #'yank-media)

  ;; Do not include date, tags and ids in note files
  (setq denote-org-front-matter "#+title: %1$s\n\n")

  (denote-rename-buffer-mode 1)
  (setq denote-buffer-name-prefix "[安安素描本] ")
  (setq denote-rename-buffer-format "%D")

  ;; The `denote-rename-buffer-mode' can now show if a file has backlinks
  ;; (setq denote-rename-buffer-backlinks-indicator " ↔ ")

  ;; Do not issue any extra prompts. Always sort by the `title' file name component and
  ;; never do a reverse sort.
  (setq denote-sort-dired-extra-prompts nil
        denote-sort-dired-default-sort-component 'title
        denote-sort-dired-default-reverse-sort t)

  ;; Hooks
  (add-hook 'dired-mode-hook #'denote-dired-mode))

(use-package denote-org
  :ensure t
  :config
  (setq denote-org-store-link-to-heading 'context)
  (defun sthenno/denote-org-path-sorted-notes (directory)
    "Return a list of note files in DIRECTORY, sorted by name."
    (sort (seq-filter 'denote-file-is-note-p
                      (directory-files directory t "\\.org$"))
          'string<)))

(use-package denote-journal
  :ensure t
  :config
  (setq denote-journal-title-format "%e %B %Y"
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
(setq org-link-elisp-confirm-function nil
      org-link-shell-confirm-function nil)

;; Org code blocks
(setq org-confirm-babel-evaluate nil)

(setq org-src-preserve-indentation t
      org-edit-src-content-indentation 0
      org-edit-src-persistent-message nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-src-ask-before-returning-to-edit-buffer nil)

;; In addition to `org-src-fontify-natively'
(add-to-list 'org-src-lang-modes (cons "python" 'python))

;; (setq org-edit-src-turn-on-auto-save t)

;; Prefer lower-case drawers
(setq org-babel-results-keyword "results")

;; Using `S-<return>' instead
;; (setq org-babel-no-eval-on-ctrl-c-ctrl-c t)
;; (keymap-set org-mode-map "S-<return>" #'org-babel-execute-maybe)

;; Enable these languages for Org-Babel
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                         (python . t)))

;; Specific `org-babel-python-command'
(setq org-babel-python-command "/Users/sthenno/Tempestissimo/tempestissimo/.venv/bin/python")

;;; Org-Table
;; (setq org-table-use-standard-references t)

;;; Export
(setq org-export-allow-bind-keywords t)

(provide 'init-org)

;;; init-org.el ends here
