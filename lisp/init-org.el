;;; init-org.el --- Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

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

;; Setup default directory
;;
(setq org-directory "~/Developer/sthenno-notebook/")

;;; Org Mode buffer init behaviors
(setq org-startup-with-inline-images t
      org-startup-with-latex-preview t)

;; Fold titles by default
(setq org-startup-folded 'content)


;; Install AUCTeX
(use-package tex
  :ensure auctex)

;; Use CDLaTeX to improve editing experiences
(use-package cdlatex
  :ensure t
  :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

;; Default LaTeX preview image directory
(setq org-preview-latex-image-directory
      (expand-file-name "ltximg/" user-cache-directory))

(setq org-persist-directory (expand-file-name "org-persist" user-cache-directory))

;; Experimental: `org-latex-preview'
(add-hook 'org-mode-hook #'(lambda ()
                             (org-latex-preview-auto-mode 1)))

;; Preview functions
;;
(defun sthenno/org-preview-fragments ()
  (interactive)
  (call-interactively 'org-latex-preview-clear-cache)
  (org-latex-preview 'buffer)
  (org-redisplay-inline-images))

(bind-keys :map org-mode-map
           ("s-p" . sthenno/org-preview-fragments))

;;
(setq org-latex-packages-alist
      '(("T1" "fontenc" t)
        ("" "amsmath"   t)
        ("" "amssymb"   t)
        ("" "siunitx"   t)
        ("" "physics2"  t)
        ;; ("" "mlmodern"  t)
        ("" "newtx" t)

        ;; Load this after all math to give access to bold math
        ;; See https://ctan.math.illinois.edu/fonts/newtx/doc/newtxdoc.pdf
        ("" "bm" t)))

(setq org-latex-preview-preamble
      (concat org-latex-preview-preamble

              ;; The following is used by the physics2 package
              "\n\\usephysicsmodule{ab,ab.braket,diagmat,xmat}%"))

(setq org-highlight-latex-and-related '(native)) ; Highlight inline LaTeX code
(setq org-use-sub-superscripts '{})

(plist-put org-latex-preview-appearance-options :scale 1.0)
(plist-put org-latex-preview-appearance-options :zoom
           (- (/ (face-attribute 'default :height)
                 100.0)
              0.025))

(setq org-latex-preview-process-default 'dvisvgm)

(defvar sthenno/libgs-dylib-path "/opt/homebrew/opt/ghostscript/lib/libgs.dylib"
  "Path to Ghostscript shared library.")

(setq dvisvgm-image-converter-command
      (list (concat "dvisvgm --page=1- --optimize --clipjoin -R --no-font "
                    "--bbox=preview --exact-bbox "
                    "--libgs=" sthenno/libgs-dylib-path " "
                    "--progress=0 "
                    "-v4 -o %B-%%9p.svg %f")))

(setq org-latex-preview-process-alist
      `((dvisvgm
         :programs ("latex" "dvisvgm")
         :description "dvi > svg"
         :message "you need to install the programs: latex and dvisvgm."
         :image-input-type "dvi"
         :image-output-type "svg"
         :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
         :latex-precompiler ("%l -output-directory %o -ini -jobname=%b \"&%L\"
      mylatexformat.ltx %f")
         :image-converter ,dvisvgm-image-converter-command)))

;; [TODO] consult-reftex, see https://karthinks.com/software/reftex-in-org-mode/


;; Modern Org Mode theme
(use-package org-modern
  :ensure t
  :after (org)
  :config
  (setq org-modern-star 'fold
        org-modern-fold-stars '(("◉" . "○"))
        org-modern-hide-stars 'leading)

  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X  . "􀃰")
                              (?-  . "􀃞")
                              (?\s . "􀂒")))

  (setq org-modern-block-name '(("src" . ("􀃤" "􀃤"))))

  ;; From https://github.com/karthink/.emacs.d/blob/master/lisp/setup-org.el
  (defun sthenno/org-modern-spacing ()
    "Adjust line-spacing for `org-modern' to correct svg display.
      This is useful if using font Iosevka."
    (setq-local line-spacing (if org-modern-mode
                                 0.1
                               0.0)))
  (add-hook 'org-modern-mode-hook #'sthenno/org-modern-spacing)
  
  (with-eval-after-load 'org
    (global-org-modern-mode 1)))

;; External settings for `org-modern'
(setq org-ellipsis " 􀍠")
(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

(setq org-hide-emphasis-markers t)
(setq org-auto-align-tags nil)
(setq org-tags-column 0)
(setq org-fontify-whole-heading-line t)

;; Custom faces for Org emphasis
;;
(defface sthenno-org-emphasis-prefix '((t (:foreground "#00bcff" :height 0.8)))
  "Sthenno's prefix emphasis for Org.")

(defface sthenno-org-emphasis-on '((t (:underline t)))
  "Sthenno's emphasized emphasis for Org.")

(setq org-emphasis-alist '(("*" sthenno-org-emphasis-on)
                           ("_" sthenno-org-emphasis-prefix)
                           ("+" (:foreground "#ff5f5f"))
                           ("=" org-code)))

;; (defface sthenno-org-button '((t (:foreground "#00bcff" :box t))))
;; (custom-set-faces 'org-link sthenno-org-button)

;; Use this with `C-<return>'
(setq org-insert-heading-respect-content t)

;; Use this with `C-S-<return>'
(setq org-treat-insert-todo-heading-as-state-change t)

;; Better experiences jumping through headlines
(setq org-special-ctrl-a/e t)

;; Fold drawers by default
(setq org-cycle-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-fold-hide-drawer-all)

;; Org fragments and overlays
;;
;; Org images
;;

(setopt org-image-align 'center
        org-image-actual-width t
        org-image-max-width 0.4
        org-display-remote-inline-images 'cache)

(defun sthenno/org-display-inline-images (&optional include-linked refresh beg end)
  (interactive "P")
  (when (display-graphic-p)
    (when refresh
      (org-remove-inline-images beg end)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))
        (let* ((case-fold-search t)
               (file-extension-re (image-file-name-regexp))
               (link-abbrevs (mapcar #'car
                                     (append org-link-abbrev-alist-local
                                             org-link-abbrev-alist)))
               (file-types-re
                (format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?\\(?:file\\|attachment\\):\\)"
                        (if (not link-abbrevs) ""
                          (concat "\\|" (regexp-opt link-abbrevs))))))
          (while (re-search-forward file-types-re end t)
            (let* ((link (org-element-lineage
                          (save-match-data (org-element-context))
                          'link t))
                   (linktype (org-element-property :type link))
                   (inner-start (match-beginning 1))
                   (path
                    (cond
                     ((not link) nil)
                     ((or (not (org-element-contents-begin link))
                          include-linked)
                      (and (or (equal "file" linktype)
                               (equal "attachment" linktype))
                           (org-element-property :path link)))
                     ((not inner-start) nil)
                     (t
                      (org-with-point-at inner-start
                        (and (looking-at
                              (if (char-equal ?< (char-after inner-start))
                                  org-link-angle-re
                                org-link-plain-re))
                             (= (org-element-contents-end link)
                                (match-end 0))
                             (progn
                               (setq linktype (match-string 1))
                               (match-string 2))))))))
              (when (and path (string-match-p file-extension-re path))
                (let ((file (if (equal "attachment" linktype)
                                (progn
                                  (require 'org-attach)
                                  (ignore-errors (org-attach-expand path)))
                              (expand-file-name path))))
                  (when file (setq file (substitute-in-file-name file)))
                  (when (and file (file-exists-p file))
                    (let ((width (org-display-inline-image--width link))
                          (align (org-image--align link))
                          (old (get-char-property-and-overlay
                                (org-element-begin link)
                                'org-image-overlay)))
                      (if (and (car-safe old) refresh)
                          (image-flush (overlay-get (cdr old) 'display))
                        (let ((image (org--create-inline-image file width)))
                          (when image
                            (let ((ov (make-overlay
                                       (org-element-begin link)
                                       (progn
                                         (goto-char
                                          (org-element-end link))
                                         (unless (eolp) (skip-chars-backward " \t"))
                                         (point)))))
                              (image-flush image)
                              (overlay-put ov 'display image)
                              (overlay-put ov 'face 'default)
                              (overlay-put ov 'org-image-overlay t)
                              (overlay-put
                               ov 'modification-hooks
                               (list 'org-display-inline-remove-overlay))
                              (when (boundp 'image-map)
                                (overlay-put ov 'keymap image-map))
                              (when align

                                ;; Use fill-column to indicate the left-edge to display
                                (let ((edge fill-column))
                                  (overlay-put
                                   ov 'before-string
                                   (propertize
                                    " " 'face 'default
                                    'display
                                    (pcase align

                                      ;; Apply the edge for alignments
                                      ("center" `(space :align-to (- (0.5 . ,edge)
                                                                     (0.5 . ,image))))
                                      ("right"  `(space :align-to (- ,edge
                                                                     ,image))))))))
                              (push ov org-inline-image-overlays))))))))))))))))
(advice-add #'org-display-inline-images :override #'sthenno/org-display-inline-images)

(setq org-yank-dnd-method 'file-link)
(setq org-yank-image-save-method (expand-file-name "images/" org-directory))

;; Org links
(setq org-return-follows-link t)

;; Open file links in current window
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;; Using shift-<arrow-keys> to select text
(setq org-support-shift-select t)


;; The Zettlekasten note-taking system by Denote
(use-package denote
  :ensure t
  :after (org)
  :init

  ;; Hooks
  (add-hook 'emacs-startup-hook #'denote-journal-extras-new-or-existing-entry)
  (add-hook 'dired-mode-hook    #'denote-dired-mode)

  :config
  (setopt denote-directory org-directory) ; Use `org-directory' as default
  (setopt denote-known-keywords '("dates"))
  (setopt denote-prompts '(title))
  (setopt denote-save-buffers t)

  ;; Denote for journals
  (setopt denote-journal-extras-directory
          (expand-file-name "dates/" denote-directory)) ; Sub-directory for journal files
  (setopt denote-journal-extras-keyword "dates")        ; Stages are journals
  (setopt denote-journal-extras-title-format "%F")      ; Use ISO 8601 for titles

  ;; Do not include date, tags and ids in note files
  (setopt denote-org-front-matter "#+title: %1$s\n\n")
  
  ;; Automatically rename Denote buffers when opening them so that instead of their long
  ;; file name they have a literal "[D]" followed by the file's title.  Read the doc
  ;; string of `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)

  (setopt denote-rename-buffer-format "􀈬 %t%b")

  ;; Do not issue any extra prompts. Always sort by the `title' file name component and
  ;; never do a reverse sort.
  (setq denote-sort-dired-extra-prompts nil)
  (setq denote-sort-dired-default-sort-component 'title)
  (setq denote-sort-dired-default-reverse-sort t)

  ;; The `denote-rename-buffer-mode' can now show if a file has backlinks
  (setopt denote-rename-buffer-backlinks-indicator " 􀄾")
  :bind ((:map global-map
               ("C-c n" . denote)
               ("C-c d" . denote-journal-extras-new-or-existing-entry))
         (:map org-mode-map
               ("C-c i" . denote-link-or-create)
               ("C-c b" . denote-backlinks)
               ("C-c e" . denote-org-extras-extract-org-subtree)
               ("C-c k" . denote-rename-file-keywords))))

;; Extensions for Denote [TODO] Add `consult-denote' support

;; Custom functions for Denote [TODO]
(defun sthenno/denote-insert-links-current-month ()
  (interactive)
  (denote-add-links (format-time-string "%B")))

(defun sthenno/denote-open-previous-file ()
  (interactive)
  (let* ((current-file (buffer-file-name))
         (directory (file-name-directory current-file))
         (files (directory-files directory t "\\`[^.]"))
         (sorted-files (sort files 'string<))
         (current-file-index (cl-position current-file sorted-files :test 'string=)))

    (when (and current-file-index (> current-file-index 0))
      (find-file (nth (1- current-file-index) sorted-files)))))

(defun sthenno/denote-open-next-file ()
  (interactive)
  (let* ((current-file (buffer-file-name))
         (directory (file-name-directory current-file))
         (files (directory-files directory t "\\`[^.]"))
         (sorted-files (sort files 'string<))
         (current-file-index (cl-position current-file sorted-files :test 'string=)))

    (when (and current-file-index (< current-file-index (1- (length sorted-files))))
      (find-file (nth (1+ current-file-index) sorted-files)))))

(bind-keys :map org-mode-map
           ("s-<up>"   . sthenno/denote-open-previous-file)
           ("s-<down>" . sthenno/denote-open-next-file))


;; Load languages for Org Babel

;; Do not ask for confirmation before executing
(setq org-link-elisp-confirm-function nil
      org-link-shell-confirm-function nil)

;; Org code blocks
(setq org-confirm-babel-evaluate nil)

(setq org-src-preserve-indentation t
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

;; Enable these languages for Org-Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python     . t)
   (shell      . t)))


;; Org-agenda [TODO]
;;
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(keymap-global-set "C-c a" 'org-agenda)

(setq org-agenda-tags-column 0)
(setq org-agenda-block-separator ?─)
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ────── " "───────────────"))
(setq org-agenda-current-time-string
      "◀── now ─────────────────────────────────────────────────")

(provide 'init-org)
