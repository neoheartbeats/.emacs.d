;;; init-org.el --- Org mode configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)
(require-package 'org-contrib)


;; Org default directory
(setq-default org-directory pes-org-path)

;; Open Org files with previewing
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)


(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Various preferences
(setq org-log-done t)
(setq org-edit-timestamp-down-means-later t)
(setq org-hide-emphasis-markers t)
(setq org-catch-invisible-edits 'show)
(setq org-export-coding-system 'utf-8)
(setq org-fast-tag-selection-single-key 'expert)
(setq org-html-validation-link nil)
(setq org-export-kill-product-buffer-when-displayed t)
(setq org-tags-column 80)
(setq org-fontify-whole-heading-line t)


(when (maybe-require-package 'org-modern)
  (setq org-modern-star '("􀄩"))
  (setq org-modern-hide-stars "􀄩")
  (setq org-modern-list '((?- . "•")))
  (setq org-modern-checkbox '((?X . "􀃠") (?- . "􀃞") (?\s . "􀂒")))
  (setq org-modern-block-name '(("src" . ("􀓪" "􀅽"))))
  (setq org-modern-table-vertical 1)
  (setq org-modern-keyword nil)
  (setq org-modern-block-fringe nil)

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))


;; Symbols in Org mode
(add-hook
 'org-mode-hook
 #'(lambda ()
     (setq prettify-symbols-alist
           '((":PROPERTIES:" . ?􀈣)
             (":ID:" . ?􀅳)
             (":END:" . ?􀅽)
             ("#+TITLE:" . ?􀎞)
             ("#+RESULTS:" . ?􀆀)
             ("#+ATTR_ORG:" . ?􀣋)
             ("SCHEDULED:" . ?􀧞)
             ("CLOSED:" .?􁜒)))
     (prettify-symbols-mode 1)))

(setq org-ellipsis " 􀍠")
(setq org-hide-emphasis-markers t)

;; Draw fringes in Org mode
(defun pes-toggle-internal-fringes ()
  (setq left-margin-width 8)
  (setq right-margin-width 8)
  (set-window-buffer nil (current-buffer)))

(add-hook 'org-mode-hook #'pes-toggle-internal-fringes)


(setq org-catch-invisible-edits 'show)
(setq org-insert-heading-respect-content t)


;; Fold drawers by default
(setq org-hide-drawer-startup t)
(add-hook 'org-mode-hook #'org-hide-drawer-all)


;; Org images
(with-eval-after-load 'org
  (setq org-image-actual-width '(300)) ; Fallback to `300'
  (define-key
   org-mode-map (kbd "s-p")
   (lambda ()
     (interactive)
     (org-latex-preview)
     (org-display-inline-images))))


;; Org links
(setq org-return-follows-link t)
(setq org-link-elisp-confirm-function nil)

(setq-default org-link-frame-setup ; Open files in current frame
              (cl-acons 'file #'find-file org-link-frame-setup))

;; Using shift-<arrow-keys> to select text
(setq org-support-shift-select t)

;; Load languages
(with-eval-after-load 'org

  ;; Org source code blocks
  (setq-default org-confirm-babel-evaluate nil)
  (setq-default org-src-preserve-indentation t)
  (setq-default org-src-fontify-natively t)
  (setq-default org-src-tab-acts-natively t)
  (setq-default org-edit-src-content-indentation 0)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t) (emacs-lisp . t) (python . t) (latex . t))))


;; Org mode text edition
;; Number of empty lines needed to keep an empty line between collapsed trees
(setq-default org-cycle-separator-lines 2)


(when (require-package 'org-roam)
  (require-package 'emacsql-sqlite-builtin)
  (setq org-roam-database-connector 'sqlite-builtin))

(setq org-roam-db-location (expand-file-name "org-roam.db" org-directory))
(setq org-roam-directory org-directory)
(setq org-roam-dailies-directory "dates/")
(setq org-roam-completion-everywhere t)
(setq org-roam-db-gc-threshold most-positive-fixnum)

;; Capture template for `org-roam-dailies'
(setq org-roam-dailies-capture-templates
      '(("d"
         "default"
         entry
         "\n* %?"
         :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d • %A>\n")
         :empty-lines 1)))

;; Default capture template for notes
(setq org-roam-capture-templates
      '(("d"
         "default"
         plain
         "%?"
         :target (file+head "notes/${slug}.org" "#+TITLE: ${title}\n")
         :empty-lines 1
         :unnarrowed t
         :immediate-finish t)))

(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode 1)

  (global-unset-key (kbd "s-n"))
  (define-key global-map (kbd "s-n j") 'org-roam-dailies-goto-today)

  (define-key org-mode-map (kbd "s-n n") 'org-roam-node-insert)
  (define-key org-mode-map (kbd "s-n a") 'org-roam-alias-add)
  (define-key org-mode-map (kbd "s-n f") 'org-roam-node-find)

  (define-key org-mode-map (kbd "s-<up>") 'org-roam-dailies-goto-previous-note)
  (define-key org-mode-map (kbd "s-<down>") 'org-roam-dailies-goto-next-note))

;; Open today's note when startup
(add-hook
 'after-init-hook
 #'(lambda ()
     (interactive)
     (org-roam-dailies-goto-today)
     (save-buffer)
     (goto-char (point-max))))


(require-package 'auctex)

(setq org-latex-default-packages-alist nil)

;; (setq org-latex-remove-logfiles nil)

(setq org-latex-logfiles-extensions
      '("bcf"
        "blg"
        "fdb_latexmk"
        "fls"
        "figlist"
        "idx"
        "nav"
        "out"
        "ptc"
        "run.xml"
        "snm"
        "toc"
        "vrb"))

(setq org-latex-packages-alist
      '(("" "mathtools" t)
        ("retain-explicit-decimal-marker" "siunitx" t)
        ("version=4" "mhchem" t)))

(setq-default org-latex-preview-ltxpng-directory
              (expand-file-name "ltximg/" user-emacs-directory))

(setq org-preview-latex-default-process 'dvisvgm)
(setq
 org-preview-latex-process-alist
 '((dvisvgm
    :programs ("xelatex" "dvisvgm")
    :description "xdv > svg"
    :image-input-type "xdv"
    :image-output-type "svg"
    :image-size-adjust (1.7 . 1.5)
    :latex-compiler ("xelatex-dev --no-pdf -output-directory %o %f") ;; -interaction nonstopmode
    :image-converter ("dvisvgm %f --scale=%S --output=%O"))))

(setq-default org-format-latex-options
              (progn
                (plist-put org-format-latex-options :background "Transparent")
                (plist-put org-format-latex-options :scale 1.70)))

;; Match the text baseline of an LaTeX fragment to the surrounding text
(defun pes-org--latex-header-preview (orig &rest args)
  "Setup dedicated `org-format-latex-header'
to `pes-org--match-text-baseline-ascent'."
  (let ((org-format-latex-header ;;
         "\\documentclass[preview]{standalone}
\\usepackage[usenames]{color}
  [PACKAGES]
  [DEFAULT-PACKAGES]"))
    (apply orig args)))

(defun pes-org--match-text-baseline-ascent (imagefile)
  "Set `:ascent' to match the text baseline of an image to the surrounding text.
  Calculate `ascent' with the data collected in IMAGEFILE."
  (advice-add 'org-create-formula-image :around #'pes-org--latex-header-preview)
  (let* ((viewbox
          (split-string
           (xml-get-attribute (car (xml-parse-file imagefile)) 'viewBox)))
         (min-y (string-to-number (nth 1 viewbox)))
         (height (string-to-number (nth 3 viewbox)))
         (ascent (round (* -100 (/ min-y height)))))
    (if (or (< ascent 0) (>= ascent 100))
        'center
      ascent)))

(defun org--make-preview-overlay (beg end image &optional imagetype)
  "Build an overlay between BEG and END using IMAGE file.
  Argument IMAGETYPE is the extension of the displayed image,
  as a string.  It defaults to \"png\"."
  (let ((ov (make-overlay beg end))
        (imagetype (or (intern imagetype) 'png)))
    (let ((ascent (pes-org--match-text-baseline-ascent image)))
      (overlay-put ov 'org-overlay-type 'org-latex-overlay)
      (overlay-put ov 'evaporate t)
      (overlay-put
       ov
       'modification-hooks
       (list (lambda (o _flag _beg _end &optional _l) (delete-overlay o))))
      (overlay-put
       ov 'display (list 'image :type imagetype :file image :ascent ascent)))))


(when (maybe-require-package 'cdlatex)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (with-eval-after-load 'cdlatex
    (diminish 'org-cdlatex-mode)))


(provide 'init-org)
;;; init-org.el ends here
