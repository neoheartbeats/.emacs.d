;;; init-org.el --- Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
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
(require 'org)

;; Setup default directory
;;
(setq org-directory "~/Developer/sthenno-notebook/")

;;; Org Mode buffer init behaviors
(setq org-startup-with-inline-images t)

;; Fold titles by default
;; (setq org-startup-folded 'content)


;; Install AUCTeX
;; (use-package tex
;;   :straight auctex)

;; (straight-use-package 'auctex)

;; Use CDLaTeX to improve editing experiences
;; (use-package cdlatex
;;   :straight t
;;   :diminish (org-cdlatex-mode)
;;   :config (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

;; Default LaTeX preview image directory
;; (setq org-preview-latex-image-directory
;;       (expand-file-name "ltximg/" user-cache-directory))

;; (setq org-persist-directory (expand-file-name "org-persist" user-cache-directory))

;; Experimental: `org-latex-preview'
;;
;; (require 'org-latex-preview)

;; (add-hook 'org-latex-preview-auto-ignored-commands #'next-line)
;; (add-hook 'org-latex-preview-auto-ignored-commands #'previous-line)
;; (add-hook 'org-latex-preview-auto-ignored-commands #'scroll-up-command)
;; (add-hook 'org-latex-preview-auto-ignored-commands #'scroll-down-command)
;; (add-hook 'org-latex-preview-auto-ignored-commands #'scroll-other-window)
;; (add-hook 'org-latex-preview-auto-ignored-commands #'scroll-other-window-down)

;; (add-hook 'org-mode-hook #'(lambda ()
;;                              (org-latex-preview-auto-mode 1)))

;; Preview functions
;;
(defun sthenno/org-preview-fragments ()
  (interactive)
  (call-interactively 'org-latex-preview-clear-cache)
  ;; (org-latex-preview 'buffer)
  (org-redisplay-inline-images))

(bind-keys :map org-mode-map
           ("s-p" . sthenno/org-preview-fragments))

;;
;; (setq org-latex-packages-alist
;;       '(("T1" "fontenc" t)
;;         ("" "amsmath"   t)
;;         ("" "amssymb"   t)
;;         ("" "siunitx"   t)
;;         ("" "physics2"  t)

;;         ;; ("" "mlmodern" t)
;;         ("etbb" "newtx" t)

;;         ;; Load this after all math to give access to bold math
;;         ;; See https://ctan.math.illinois.edu/fonts/newtx/doc/newtxdoc.pdf
;;         ("" "bm" t)))

;; (setq org-latex-preview-preamble
;;       (concat org-latex-preview-preamble

;;               ;; The following is used by the physics2 package
;;               "\n\\usephysicsmodule{ab,ab.braket,diagmat,xmat}%"))

;; (setq org-highlight-latex-and-related '(native)) ; Highlight inline LaTeX code
;; (setq org-use-sub-superscripts '{})

;; (plist-put org-latex-preview-appearance-options :scale 1.0)
;; (plist-put org-latex-preview-appearance-options :zoom
;;            (- (/ (face-attribute 'default :height) 110.0) 0.025))

;;
;; The `org-latex-preview' process
;;

;; (setq org-latex-preview-process-default 'dvisvgm)

;; (defvar sthenno/libgs-dylib-path "/opt/homebrew/opt/ghostscript/lib/libgs.dylib"
;;   "Path to Ghostscript shared library.")

;; (setq dvisvgm-image-converter-command
;;       (list (concat "dvisvgm --page=1- --optimize --clipjoin -R --no-font "
;;                     "--bbox=preview --exact-bbox "
;;                     "--libgs=" sthenno/libgs-dylib-path " "
;;                     "--progress=0 "
;;                     "-v4 -o %B-%%9p.svg %f")))

;; (setq org-latex-preview-process-alist
;;       `((dvisvgm
;;          :programs ("latex" "dvisvgm")
;;          :description "dvi > svg"
;;          :message "you need to install the programs: latex and dvisvgm."
;;          :image-input-type "dvi"
;;          :image-output-type "svg"
;;          :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
;;          :latex-precompiler ("%l -output-directory %o -ini -jobname=%b \"&%L\"
;;       mylatexformat.ltx %f")
;;          :image-converter ,dvisvgm-image-converter-command)))

;; [TODO] consult-reftex, see https://karthinks.com/software/reftex-in-org-mode/


;;; Modern Org Mode theme
;;
(use-package org-modern
  :straight t
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

  (global-org-modern-mode 1))

;; External settings for `org-modern'
(setq org-ellipsis " 􀍠")
(setq org-hide-emphasis-markers t)
(setq org-auto-align-tags nil)
(setq org-tags-column 0)
(setq org-fontify-whole-heading-line t)

;; Custom faces for Org emphasis
;;
(defface sthenno-org-emphasis-prefix '((t (:foreground "#00bcff" :height 0.8)))
  "Sthenno's prefix emphasis for Org.")

(setq org-emphasis-alist '(("*" bold)
                           ("_" sthenno-org-emphasis-prefix)
                           ("=" org-verbatim verbatim)
                           ("+" (:strike-through t))))

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

(setq org-image-align 'left
      org-image-actual-width '(240))

(setq org-yank-dnd-method 'file-link)
(setq org-yank-image-save-method (expand-file-name "images/" org-directory))

;; Org links
(setq org-return-follows-link t)

;; Open file links in current window
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

;; Using shift-<arrow-keys> to select text
(setq org-support-shift-select t)


;;
;; The Zettlekasten note-taking system by Denote
;;
(use-package denote
  :straight t
  :config
  (setq denote-directory org-directory) ; Use `org-directory' as default
  (setq denote-known-keywords '("dates"
                                "blog"))
  (setq denote-prompts '(title))
  (setq denote-save-buffers t)

  ;; Denote for journals
  (setq denote-journal-extras-directory
        (expand-file-name "dates/" denote-directory)) ; Sub-directory for journal files
  (setq denote-journal-extras-keyword "dates")        ; Stages are journals
  (setq denote-journal-extras-title-format "%F")      ; Use ISO 8601 for titles

  ;; Do not include date, tags and ids in note files
  (setq denote-org-front-matter "#+TITLE: %1$s. 􀙤\n\n")

  :bind
  (:map global-map

        ;; Open today's note
        ("C-c d" . denote-journal-extras-new-or-existing-entry))
  (:map org-mode-map
        ("C-c i" . denote-link-or-create)
        ("C-c b" . denote-backlinks)
        ("C-c e" . denote-org-extras-extract-org-subtree)
        ("C-c k" . denote-rename-file-keywords))
  :hook (after-init . denote-journal-extras-new-or-existing-entry))

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

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (python     . t)))


;; Org-agenda [TODO]
;; (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
;; (bind-keys :map global-map
;;            ("C-c a" . org-agenda))

;; Org-agenda settings related to `org-modern'
;; (setq org-agenda-tags-column 0)
;; (setq org-agenda-block-separator ?─)
;; (setq org-agenda-time-grid
;;       '((daily today require-timed)
;;         (800 1000 1200 1400 1600 1800 2000)
;;         " ────── " "───────────────"))
;; (setq org-agenda-current-time-string
;;       "◀── now ─────────────────────────────────────────────────")


;; Useful functions
;; (defun sthenno/org-mode-insert-get-button ()
;;   "Inserts a button that copies a user-defined string to clipboard."
;;   (interactive)
;;   (let ((content (read-string "Content: ")))
;;     (insert (format "[[elisp:(kill-new \"%s\")][GET]]" content))))


;;; TODO
;;
;; Integrate with built-in Python API -> `init-eglot'
;;
;; TTS implementation using OpenAI's API
;; (defun sthenno/content-by-key-from-file (filename key)
;;   "Get content string of KEY from FILENAME."
;;   (with-temp-buffer
;;     (insert-file-contents filename)
;;     (goto-char (point-min))
;;     (if (re-search-forward (format "^%s=\"\\([^\"]+\\)\"" key) nil t)
;;  (match-string 1)
;;       (error "Key %s not found in file %s" key filename))))

;; (defun sthenno/environ-from-user-emacs-dir (key)
;;   "Get environ content by KEY from .env file in `user-emacs-directory'."
;;   (let ((filename (concat user-emacs-directory ".env")))
;;     (sthenno/content-by-key-from-file filename key)))

;; (setq sthenno/openai-api-key
;;       (sthenno/environ-from-user-emacs-dir "OPENAI_API_KEY"))

;; (require 'json)

;; (defun sthenno/speech-from-str-to-file (input-string output-file)
;;   "Send a text-to-speech request to the OpenAI API and save the
;; result to OUTPUT-FILE."
;;   (let* ((url "https://api.openai.com/v1/audio/speech")
;;          (url-request-method "POST")
;;          (url-request-extra-headers
;;           `(("Authorization" . ,(concat "Bearer " sthenno/openai-api-key))
;;             ("Content-Type" . "application/json")))
;;          (url-request-data
;;           (json-encode `(("model" . "tts-1")
;;                          ("input" . ,input-string)
;;                          ("voice" . "echo"))))
;;          (buffer (url-retrieve-synchronously url)))
;;     (when buffer
;;       (with-current-buffer buffer
;;         (goto-char (point-min))
;;         (re-search-forward "\n\n")
;;         (write-region (point) (point-max) output-file))
;;       (kill-buffer buffer))))

;; (defun sthenno/generate-timestamp ()
;;   "Generate a timestamp in the format YYYYMMDDTHHMMSS."
;;   (format-time-string "%Y%m%dT%H%M%S"))

;; (setq sthenno/speach-files-dir (concat org-directory "medi/"))

;; (defun sthenno/speech-from-str-to-file-insert ()
;;   "Send the selected text to the OpenAI API and insert the result at
;; the point."
;;   (interactive)
;;   (if (use-region-p)
;;       (let* ((start (region-beginning))
;;       (end (region-end))
;;       (input-string (buffer-substring-no-properties start end))
;;       (filename (concat sthenno/speach-files-dir
;;                 "speach-" (sthenno/generate-timestamp) ".mp3"))
;;       (button-string
;;        (format "[[elisp:(emms-play-file \"%s\")][[􀊨]]]" filename)))
;;  (sthenno/speech-from-str-to-file input-string filename)
;;  (goto-char end)
;;  (insert (concat " " button-string))
;;  (message (format "TTS finished to file %s" filename)))
;;     (message "No region selected")))

;; (bind-keys* :map org-mode-map
;;      ("s-[ s" . sthenno/speech-from-str-to-file-insert))

;; (defun sthenno/play-speach-current-heading ()
;;   "Play the speach audio if there is exactly one in current heading."
;;   (interactive)
;;   (save-excursion
;;     (org-back-to-heading t)
;;     (let ((heading-end (save-excursion
;;           (outline-next-heading)
;;           (point)))
;;           (button-count 0)
;;           button-pos)

;;       ;; Count the number of "[􀊨]" buttons and record the position
;;       ;; of the button
;;       (while (re-search-forward "\\[􀊨\\]" heading-end t)
;;         (setq button-count (1+ button-count))
;;         (setq button-pos (match-beginning 0)))

;;       ;; Check if there is exactly one button
;;       (if (= button-count 1)
;;           (progn
;;             (goto-char button-pos)
;;             (org-open-at-point))
;;         (message
;;   "There must be exactly one \"[􀊨]\" button in current heading")))))


;; Modules for language learning
;; (use-package org-drill
;;   :straight t
;;   :config
;;   (setq org-drill-learn-fraction 0.5)
;;   (setq org-drill-maximum-items-per-session 20)
;;   (add-hook 'org-drill-display-answer-hook #'sthenno/play-speach-current-heading))

(provide 'init-org)
