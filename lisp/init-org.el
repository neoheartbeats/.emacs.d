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
;; (setq org-startup-folded 'content)

;;; Install AUCTeX
(use-package tex
  :ensure auctex)

;; Use CDLaTeX to improve editing experiences
;; This is temporarily disabled and replaced by `tempel'
;;
;; (use-package cdlatex
;;   :ensure t
;;   :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

;; Default LaTeX preview image directory
(setq org-preview-latex-image-directory
      (expand-file-name "ltximg/" user-cache-directory))

(setq org-persist-directory (expand-file-name "org-persist" user-cache-directory))

;; Experimental: `org-latex-preview'
(add-hook 'org-mode-hook #'(lambda ()
                             (org-latex-preview-auto-mode 1)))

;; Ignore scrolling commands for `org-latex-preview-auto-mode'
(setq org-latex-preview-auto-ignored-commands
      '(
        next-line previous-line
        pixel-scroll-precision
        scroll-up-command scroll-down-command))

;; Show entities as UTF8 characters
;; (setopt org-pretty-entities t
;;         org-pretty-entities-include-sub-superscripts nil)

;; Preview functions
;;
(defun sthenno/org-preview-fragments ()
  (interactive)
  (call-interactively 'org-latex-preview-clear-cache)
  (org-latex-preview 'buffer)
  (org-redisplay-inline-images))
(bind-keys :map org-mode-map
           ("s-p" . sthenno/org-preview-fragments))

(setq org-latex-packages-alist '(("T1" "fontenc" t)
                                 ("" "amsmath"   t)
                                 ("" "amssymb"   t)
                                 ("" "siunitx"   t)

                                 ;; Font packages
                                 ("libertinus" "newtx" t)
                                 
                                 ;; Load this after all math to give access to bold math
                                 ;; See https://ctan.org/pkg/newtx
                                 ("" "bm" t)

                                 ;; Package physics2 requires to be loaded after font
                                 ;; packages. See https://ctan.org/pkg/physics2
                                 ("" "physics2" t)

                                 ;; Differentiations
                                 ("normal" "fixdif")))

;; Add additional modules required by LaTeX packages like physics2 to the preamble
(let* ((physics2-modules '(("" "ab")
                           ("" "diagmat")
                           ("" "xmat")))
       (physics2-preamble (mapconcat
                           (lambda (m)
                             (let ((options (car  m))
                                   (module  (cadr m)))
                               (if (string= options "")
                                   (format "\\usephysicsmodule{%s}" module)
                                 (format "\\usephysicsmodule[%s]{%s}" options module))))
                           physics2-modules
                           "\n")))
  (unless (and org-latex-preview-preamble
               (string-match
                (regexp-quote physics2-preamble) org-latex-preview-preamble))
    (setq org-latex-preview-preamble (concat (or org-latex-preview-preamble "")
                                             "\n" physics2-preamble))))

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
      
      ;; The --optimise, --clipjoin, and --relative flags cause dvisvgm to do some extra
      ;; work to tidy up the SVG output, but barely add to the overall dvisvgm runtime
      ;; (<1% increace, from testing).
      `(,(concat "dvisvgm --page=1- --optimize --clipjoin -R --no-font --bbox=preview"
                 " --libgs=" sthenno/libgs-dylib-path

                 ;; Default is "-v3" here, but it does not seem to work correctly in my
                 ;; client.
                 " --progress=0 -v4 -o %B-%%9p.svg %f")))

(setq org-latex-preview-process-alist
      `((dvisvgm
         :programs ("latex" "dvisvgm")
         :description "dvi > svg"
         :message "you need to install the programs: latex and dvisvgm."
         :image-input-type "dvi"
         :image-output-type "svg"
         :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
         :latex-precompiler ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
         :image-converter ,dvisvgm-image-converter-command)))

;; Centering previews using Org's alignment system instead of adding edges to original
;; SVG files.
;;
;; By karthink:
;; https://discord.com/channels/406534637242810369/1056621127188881439/1288235109128081440

;; (defun sthenno/org-latex-preview-uncenter (ov)
;;   (overlay-put ov 'before-string nil))

;; (defun sthenno/org-latex-preview-recenter (ov)
;;   (overlay-put ov 'before-string (overlay-get ov 'justify)))

;; (defun sthenno/org-latex-preview-center (ov)
;;   (save-excursion
;;     (goto-char (overlay-start ov))
;;     (when-let* ((elem (org-element-context))
;;                 ((or (eq (org-element-type elem) 'latex-environment)
;;                      (string-match-p "^\\\\\\[" (org-element-property :value elem))))
;;                 (img (overlay-get ov 'display))
;;                 (prop `(space :align-to (- center (0.55 . ,img))))
;;                 (justify (propertize " " 'display prop 'face 'default)))
;;       (overlay-put ov 'justify justify)
;;       (overlay-put ov 'before-string (overlay-get ov 'justify)))))

;; (add-hook 'org-latex-preview-overlay-open-functions  #'sthenno/org-latex-preview-uncenter)
;; (add-hook 'org-latex-preview-overlay-close-functions #'sthenno/org-latex-preview-recenter)
;; (add-hook 'org-latex-preview-overlay-update-functions #'sthenno/org-latex-preview-center)

;; [TODO] consult-reftex, see https://karthinks.com/software/reftex-in-org-mode/

;;; Modern Org Mode theme
(use-package org-modern
  :ensure t
  :config
  (setq org-modern-star 'fold
        org-modern-fold-stars '(("▶" . "▼")
                                ("▷" . "▽"))
        org-modern-hide-stars 'leading)
  
  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X  . "􀃰")
                              (?-  . "􀃞")
                              (?\s . "􀂒")))

  (setq org-modern-block-name '(("src"    . ("􀃤" "􀂓"))
                                ("quote"  . ("􀈎" "􀂓"))
                                ("export" . ("􀣙" "􀂓"))))

  (setq org-modern-timestamp '(" %Y-%m-%d " . " %H:%M "))
  (setq org-modern-keyword '(("title"   . "􀉛")
                             ("results" . "􂨖")
                             (t . t)))

  (defun sthenno/org-modern-spacing ()
    "Adjust line-spacing for `org-modern' to correct svg display. This is
useful if using font Iosevka."
    (setq-local line-spacing (cond ((eq major-mode #'org-mode) 0.10)
                                   ((eq major-mode #'org-agenda-mode) 0.10)
                                   (t 0.0))))
  (add-hook 'org-mode-hook #'sthenno/org-modern-spacing)
  (add-hook 'org-agenda-finalize-hook #'sthenno/org-modern-spacing)

  ;; Hooks
  (add-hook 'org-mode-hook #'(lambda ()
                               (global-org-modern-mode 1))))

;; External settings for `org-modern'
(setq org-ellipsis " ")
(setq org-use-property-inheritance t)
(setq org-hide-emphasis-markers t)
(setq org-auto-align-tags nil)
(setq org-tags-column 0)

(defun sthenno/org-toggle-emphasis ()
  "Toggle hiding of Org emphasis markers."
  (interactive)
  (if org-hide-emphasis-markers
      (progn
        (setq-local org-hide-emphasis-markers nil)
        (message "Org emphasis markers are not hiding."))
    (progn
      (setq-local org-hide-emphasis-markers t)
      (message "Org emphasis markers are hiding."))))
(keymap-set org-mode-map "C-c e" #'sthenno/org-toggle-emphasis)

;; Custom faces for Org emphasis
;;
;; (defface sthenno-org-emphasis-prefix '((t (:foreground "#00bcff" :height 0.8)))
;;   "Sthenno's prefix emphasis for Org.")

;; (defface sthenno-org-emphasis-on '((t (:underline t)))
;;   "Sthenno's emphasized emphasis for Org.")

;; (setq org-emphasis-alist '(("*" sthenno-org-emphasis-on)
;;                            ("_" sthenno-org-emphasis-prefix)
;;                            ("+" (:foreground "#ff5f5f"))
;;                            ("=" org-code)))

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

;;; Org fragments and overlays
;;
;; Org images
;;

(setopt org-image-align 'center
        org-image-actual-width t
        org-image-max-width 0.4
        org-display-remote-inline-images 'cache)

(defun sthenno/org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these conventions:

  1. Its path is a file with an extension matching return value from
     `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous type.  In
     this case, that link must be a well-formed plain or angle link,
     i.e., it must have an explicit \"file\" or \"attachment\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with a text
description part will be inlined.  This can be nice for a quick look at
those images, but it does not reflect what exported files will look
like.

When optional argument REFRESH is non-nil, refresh existing images
between BEG and END.  This will create new image displays only if
necessary.

BEG and END define the considered part. They default to the buffer
boundaries with possible narrowing."
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
                (format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]
\\[\\(<?\\(?:file\\|attachment\\):\\)"
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

                                ;; Use fill-column to indicate the right-edge to display
                                (let ((edge fill-column))
                                  (overlay-put
                                   ov 'before-string
                                   (propertize
                                    " " 'face 'default
                                    'display
                                    (pcase align

                                      ;; Apply the edge for alignments
                                      ("center" `(space :align-to (- (0.55 . ,edge)
                                                                     (0.55 . ,image))))
                                      ("right"  `(space :align-to (- ,edge
                                                                     ,image))))))))
                              (push ov org-inline-image-overlays))))))))))))))))
(advice-add #'org-display-inline-images :override #'sthenno/org-display-inline-images)

(setq org-yank-dnd-method 'file-link)
(setq org-yank-image-save-method (expand-file-name "images/" org-directory))

;;; Org Hyperlinks
(require 'ol)
(setq org-return-follows-link t)

;; Support for links to kill strings as its content
(org-link-set-parameters "kill"
                         :follow #'sthenno/org-kill-open
                         :export #'sthenno/org-kill-export
                         :store #'sthenno/org-kill-store-link)

(defcustom sthenno/org-kill-command 'kill-new
  "The Emacs command to be used to kill a string as the latest kill in the
kill ring."
  :group 'org-link
  :type '(choice (const kill-new)
                 (const org-kill-new)))

(defun sthenno/org-kill-open (text _)
  "Make TEXT the latest kill in the kill ring."
  (funcall sthenno/org-kill-command text)
  (message "String %s killed." text))

(defun sthenno/org-kill-store-link (&optional _interactive?)
  "Store a link to a string."
  (let* ((text (buffer-substring-no-properties (region-beginning)
                                               (region-end)))
         (link (concat "kill:" text)))
    (org-link-store-props
     :type "kill"
     :link link
     :description text)))

(defun sthenno/org-kill-export (link description format _)
  "Export a kill link from Org files."
  (let ((text link)
        (desc (or description link)))
    (pcase format
      (_ text))))

(defun sthenno/org-kill-insert ()
  (interactive)
  (let* ((lo (region-beginning))
         (hi (region-end))
         (text (buffer-substring-no-properties lo hi)))
    (delete-region lo hi)
    (insert (format "[[kill:%s][%s]]" text text))))

;; Open file links in current window
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;;; Using shift-<arrow-keys> to select text
(setq org-support-shift-select t)

;;; The Zettlekasten note-taking system by Denote
(use-package denote
  :ensure t
  :after (org)
  :init

  ;; Hooks
  (add-hook 'emacs-startup-hook #'denote-journal-extras-new-or-existing-entry)
  (add-hook 'dired-mode-hook    #'denote-dired-mode)

  :config
  (setopt denote-directory org-directory) ; Use `org-directory' as default
  (setopt denote-file-type 'org)
  (setopt denote-prompts '(title))
  (setopt denote-save-buffers t
          denote-kill-buffers 'on-creation)

  ;; Denote for journals
  (setopt denote-journal-extras-directory
          (expand-file-name "stages/" denote-directory)) ; Sub-directory for journal files
  (setopt denote-journal-extras-keyword "stages")        ; Stages are journals
  (setopt denote-journal-extras-title-format "%F")       ; Use ISO 8601 for titles

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

  ;; Creating and linking notes
  (defun sthenno/denote-link-or-create ()
    "Improved note creation and linking for Denote."
    (interactive)
    (let ((denote-prompts nil)
          (denote-kill-buffers nil))
      (cond
       ((use-region-p)
        (let* ((denote-ignore-region-in-denote-command t)
               (path (buffer-substring-no-properties (region-beginning)
                                                     (region-end)))
               (regexp (format "--%s" path))
               (target (car (denote-directory-files regexp :omit-current))))
          (if target
              (progn
                (denote-link target
                             (denote-filetype-heuristics (buffer-file-name))
                             (denote--link-get-description target))
                (save-buffer)
                (message "Note linked to: %s" target))
            (progn
              (save-window-excursion
                (setq target (denote path '("notes"))))
              (denote-link target
                           (denote-filetype-heuristics (buffer-file-name))
                           (denote--link-get-description target))
              (save-buffer)
              (find-file target)
              (message "Note created at: %s" target)))))
       ((and (not (use-region-p))
             (org-at-heading-p))
        (denote-org-extras-extract-org-subtree)
        (save-buffer))
       (t
        (call-interactively #'denote-link-or-create))))
    (run-hooks 'sthenno/denote-link-or-create-hook))

  ;; The `denote-rename-buffer-mode' can now show if a file has backlinks
  (setopt denote-rename-buffer-backlinks-indicator " 􀄾")

  ;; Org subtrees
  (setopt denote-org-store-link-to-heading 'context)

  :bind ((:map global-map
               ("C-c o" . denote-open-or-create)
               ("C-c d" . denote-journal-extras-new-or-existing-entry))
         (:map org-mode-map
               ("s-i" . sthenno/denote-link-or-create)
               ("s-b" . denote-backlinks)
               ("s-r" . denote-region))))

;; Extensions for `denote'
;;
;; (use-package consult-denote
;;   :ensure t
;;   :config (consult-denote-mode 1))
;;
;; (use-package denote-menu
;;   :ensure t
;;   :after (denote)
;;   :config
;;   (setq denote-menu-date-column-width 25 
;;         denote-menu-title-column-width 60)

;;   ;; Remove Denote journal entries from the menu
;;   (setq denote-menu-initial-regex "_notes")

;;   ;; HACK
;;   (define-derived-mode denote-menu-mode tabulated-list-mode "Denote Menu"
;;     "Major mode for browsing a list of Denote files."
;;     :interactive nil
;;     (setq tabulated-list-format `[("􀧞" ,denote-menu-date-column-width t)
;;                                   ("􀉛" ,denote-menu-title-column-width t)])

;;     (setq tabulated-list-padding 5)
;;     (denote-menu-update-entries)
;;     (setq tabulated-list-sort-key '("􀧞" . t))
;;     (tabulated-list-init-header)    
;;     (tabulated-list-print))

;;   :bind (:map global-map
;;               ("s-o" . sthenno/denote-menu-open)))

;;; Custom functions for Denote
;;
;; (defun sthenno/denote-insert-links-current-month ()
;;   (interactive)
;;   (denote-add-links (format-time-string "%B")))

(defun sthenno/get-sorted-note-files (directory)
  "Return a list of note files in DIRECTORY, sorted by name."
  (sort (seq-filter 'denote-file-is-note-p
                    (directory-files directory t "\\`[^.]"))
        'string<))

(defun sthenno/denote-open-adjacent-file (direction)
  "Open the adjacent note file in the given DIRECTION (-1 for previous, +1 for next)."
  (let* ((current-file (buffer-file-name))
         (directory (file-name-directory current-file))
         (sorted-files (sthenno/get-sorted-note-files directory))
         (current-file-index (cl-position current-file sorted-files :test 'string=)))
    (if (null current-file-index)
        (message "Current file is not a note file.")
      (let ((adjacent-index (+ current-file-index direction)))
        (if (or (< adjacent-index 0)
                (>= adjacent-index (length sorted-files)))
            (message "No adjacent note file.")
          (find-file (nth adjacent-index sorted-files)))))))

(defun sthenno/denote-open-previous-file ()
  "Open the previous note file in the current directory."
  (interactive)
  (sthenno/denote-open-adjacent-file -1))

(defun sthenno/denote-open-next-file ()
  "Open the next note file in the current directory."
  (interactive)
  (sthenno/denote-open-adjacent-file +1))

;;; Experimental: org-node

;; (use-package org-node
;;   :after org
;;   :ensure t
;;   :config (org-node-cache-mode))


(bind-keys :map org-mode-map
           ("s-<up>"   . sthenno/denote-open-previous-file)
           ("s-<down>" . sthenno/denote-open-next-file))

;;; Load languages for Org Babel

;; Do not ask for confirmation before executing
(setq org-link-elisp-confirm-function nil
      org-link-shell-confirm-function nil)

;; Org code blocks
(setq org-confirm-babel-evaluate nil)

(setq org-src-preserve-indentation t
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

;; Enable these languages for Org-Babel
(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (python . t)
                               (shell . t)))

;; Define a function to copy the code block at point
(defun sthenno/org-copy-source-code-block ()
  "Copy the contents of the source code block at point."
  (interactive)
  (let ((e (org-element-context)))
    (if (eq (org-element-type e) 'src-block)
        (let ((sc ""))
          (save-restriction
            (org-narrow-to-subtree)
            (org-babel-map-src-blocks nil
              (setq sc (concat sc (org-no-properties body)))))
          (progn
            (kill-new sc)
            (message "Code block content copied")
            (run-hooks 'sthenno/org-copy-source-code-block-hook)))
      (message "Not in a code block"))))
(keymap-set org-mode-map "s-<mouse-1>" #'sthenno/org-copy-source-code-block)

;;; Org-Agenda

(require 'denote)
(setq org-agenda-files `(,denote-directory
                         ,denote-journal-extras-directory))

;; Bind keys
(defun sthenno/org-agenda-list-all-todos ()
  (interactive)
  (org-agenda nil "t"))
(keymap-global-set "C-c a" #'sthenno/org-agenda-list-all-todos)

(setq org-log-done 'time)

;; Make Org-Agenda look better
(setq org-agenda-overriding-header "")
(setq org-agenda-block-separator ?─)
(setq org-agenda-time-grid '((daily today require-timed)
                             (800 1000 1200 1400 1600 1800 2000)
                             " ────── " "───────────────")
      org-agenda-current-time-string "───────────────")

(setq org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t)

(setq org-agenda-span 5
      org-agenda-start-day "+0d")
(setq org-deadline-warning-days 4)

(setq org-agenda-category-icon-alist nil)
(setq org-agenda-prefix-format '((agenda . "%i %?-12t%s 􀐱 ")
                                 (todo   . "%i ○ ")
                                 (tags   . "%i")
                                 (search . "%i")))
(setq org-agenda-format-date "\n􀧞 %F\n")

(provide 'init-org)
