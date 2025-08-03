;;; init-comp.el --- Modern completion system  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file includes:
;; - completion styles enhancement using `orderless'
;; - minibuffer enhancement using `vertico' and `consult'
;; - pop-up completions by `corfu' as frontend and `cape' as backend
;;
;; NOTE: Package `embark' is not included in this config due to my personal preferences.
;; Template's setups such as that for `abbrev' are placed separately in `init-temp' but
;; `dabbrev' is configured in this file under current decision.

;;; Code:

;;; Build the completion framework
;;
;; Completion and minibuffer basics
;;

;; Prevent auto-composition of characters around point
;; (setq composition-break-at-point t)

;; Completion basics. See also `orderless'
;;

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold nil)   ; Always show candidates in menu

;; Emacs 30: `cape-dict' is used instead.
;; NOTE: `setopt' is necessary.
(setopt text-mode-ispell-word-completion nil)

;; Do not use the TAB key for `completion-at-point'.
(setq tab-always-indent 'complete)
(setq tab-first-completion nil)

;; minibuffer
(setq echo-keystrokes 0.05)             ; Display the key pressed immediately
(setq resize-mini-windows t)

;; Help buffer
(setq help-window-select t)

;; Support opening new minibuffers from inside existing minibuffers
(setq enable-recursive-minibuffers t)
(setq read-minibuffer-restore-windows nil)

;; Hide undefined commands in M-x
(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq minibuffer-default-prompt-format " [%s]"
      minibuffer-completion-auto-choose t
      minibuffer-visible-completions nil)

;; Do not allow the cursor in the minibuffer prompt
(setopt minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
(setq crm-prompt (format "%s %%p" (propertize "[%d]" 'face 'shadow)))
(file-name-shadow-mode 1)

;;; Use the `orderless' completion style
(use-package orderless
  :ensure t
  :init
  (setq completion-pcm-leading-wildcard t) ; Emacs 31: make `partial-completion' behave like `substring'

  ;; The basic completion style is specified as fallback in addition to orderless in
  ;; order to ensure that completion commands rely on dynamic completion tables
  (setq-default completion-styles '(orderless basic)
                completion-category-defaults nil
                completion-category-overrides '((file (styles partial-completion))

                                                ;; There is further configuration for
                                                ;; better `eglot' support. See also
                                                ;; `init-eglot'
                                                (eglot      (styles orderless))
                                                (eglot-capf (styles orderless))))
  :config
  (orderless-define-completion-style orderless-literal-only
    (orderless-style-dispatchers nil)
    (orderless-matching-styles '(orderless-literal)))
  (defun sthenno/completion-style-corfu ()
    (interactive)
    (setq-local completion-styles '(orderless-literal-only basic)
                completion-category-overrides nil
                completion-category-defaults nil))
  (setq-default orderless-matching-styles
                '(orderless-literal orderless-prefixes)))

;; Sort candidates by `minibuffer-sort-by-history'
(setq completions-sort 'historical
      completion-auto-help nil
      completion-show-help nil
      completion-show-inline-help nil)

;; Ignore cases for completions
(setq-default completion-ignore-case t)
(setq-default read-file-name-completion-ignore-case t
              read-buffer-completion-ignore-case t)

;;; Completions in minibuffers
(use-package vertico
  :ensure t
  :demand t
  :init
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)

  ;; Disable showing the *Completions* buffer that conflicts with `vertico' if using
  ;; `ffap-menu'
  (advice-add #'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args))))

  ;; Use `consult-completion-in-region' if `vertico' is enabled.
  (setq completion-in-region-function (lambda (&rest args)
                                        (apply (if vertico-mode
                                                   #'consult-completion-in-region
                                                 #'completion--in-region)
                                               args)))

  ;; Hooks
  (add-hook 'after-init-hook #'vertico-mode)

  :config
  (setq vertico-count 15)
  (setq vertico-resize t)
  (setq vertico-scroll-margin 4)
  (setq vertico-cycle nil)

  ;; Add some simple indicator symbols here to make things clear
  (setq vertico-count-format (cons "[ %-6s ] " "%s of %s"))

  ;; Do not render italic fonts
  (set-face-attribute 'vertico-group-title nil :slant 'normal)

  ;; Correct file path when changed (tidy shadowed file names)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  :bind ((:map vertico-map
               ("<tab>"       . vertico-insert)
               ("<return>"    . vertico-directory-enter)
               ("<backspace>" . vertico-directory-delete-char))))

;;; Rich annotations for minibuffer
(use-package marginalia
  :ensure t
  :init
  (setq marginalia-field-width 50)
  (setq marginalia-separator " ")
  (setq marginalia-align 'left)
  (setq marginalia-align-offset 4)
  (marginalia-mode 1))

;;; Consult is useful previewing current content in buffer
(use-package consult
  :ensure t
  :init
  (setq register-preview-delay 0.125
        register-preview-function #'consult-register-format)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3
        consult-async-input-debounce 0.50
        consult-async-input-throttle 0.75)
  (setq consult-narrow-key nil)

  ;; Use `consult-ripgrep' instead of `project-find-regexp' in `project'
  (keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)
  (cl-nsubstitute-if '(consult-ripgrep "Find regexp")
                     (pcase-lambda (`(,cmd _))
                       (eq cmd #'project-find-regexp))
                     project-switch-commands)

  ;; Customize `consult' minibuffer prompts
  (setq consult-buffer-sources '(consult--source-modified-buffer
                                 consult--source-buffer
                                 consult--source-recent-file))

  (consult-customize consult-buffer
                     :prompt "Buffer → ")

  ;; Shorten recent files in `consult-buffer'
  (defun sthenno/consult--source-recentf-items ()
    (let ((ht (consult--buffer-file-hash))
          file-name-handler-alist
          items)
      (dolist (file recentf-list (nreverse items))
        (unless (eq (aref file 0) ?/)
          (setq file (expand-file-name file)))
        (unless (gethash file ht)
          (push (propertize
                 (file-name-nondirectory file)
                 'multi-category `(file . ,file))
                items)))))
  (plist-put consult--source-recent-file
             :items #'sthenno/consult--source-recentf-items)

  :bind ((:map global-map
               ("s-b" . consult-buffer)
               ("C-x b" . consult-buffer)
               ("C-s" . consult-line)
               ("s-;" . consult-goto-line)
               ("C-v" . consult-yank-pop)
               ("s-m" . consult-imenu-multi)
               ("s-n" . consult-recent-file)
               ("M-i" . consult-info)
               ("M-s" . consult-ripgrep))
         (:map consult-narrow-map
               ("?" . consult-narrow-help))))

;;; Dabbrev settings
(use-package dabbrev
  :config

  ;; Better letter cases
  (setq dabbrev-case-distinction 'case-replace
        dabbrev-case-replace 'case-replace
        dabbrev-case-fold-search nil
        dabbrev-upcase-means-case-search t)

  (defun sthenno/dabbrev-elisp ()
    "Setup `dabbrev' for `emacs-lisp-mode'."
    (setq-local dabbrev-case-fold-search nil)
    (setq-local dabbrev-case-replace nil))
  (add-hook 'emacs-lisp-mode-hook #'sthenno/dabbrev-elisp)

  ;; Ignore these for `dabbrev'
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; Add extensions for the completion backend
(use-package cape
  :ensure t
  :init

  ;; Dict
  (setq cape-dict-case-fold t
        cape-dict-case-replace t)
  (setq cape-dict-limit 40)

  ;; Setup Capfs
  (defun sthenno/capf-eglot ()
    (setq-local completion-at-point-functions
                `(,(cape-capf-super
                    (cape-capf-buster #'eglot-completion-at-point)
                    #'cape-dabbrev)
                  cape-file)
                cape-dabbrev-min-length 4))
  (add-hook 'eglot-managed-mode-hook #'sthenno/capf-eglot)

  (defun sthenno/capf-elisp ()
    (setq-local completion-at-point-functions
                `(,(cape-capf-super
                    (cape-capf-predicate
                     #'elisp-completion-at-point
                     #'(lambda (cand)
                         (or (not (keywordp cand))
                             (eq (char-after (car completion-in-region--data)) ?:))))
                    #'cape-dabbrev)
                  cape-file)
                cape-dabbrev-min-length 2))
  (add-hook 'emacs-lisp-mode-hook #'sthenno/capf-elisp)

  (with-eval-after-load 'denote-org

    ;; Setup campletion-at-point functions for `denote' at Org.
    (defvar sthenno/denote-org-link-completions nil
      "Cache of (TITLE . ID) alist for Denote notes, used for Org link completion.")

    (defun sthenno/denote-org-link-refresh ()
      "Refresh the cache of Denote note titles and IDs for link completion.
Uses Denote’s APIs to get all notes in `denote-directory', then caches
their titles and identifiers. The cache is persisted across sessions
using `org-persist'."
      (setq sthenno/denote-org-link-completions
            (let ((files (denote-directory-files nil nil t)))
              (mapcar (lambda (file)
                        (let* ((ft    (denote-filetype-heuristics file))
                               (title (denote-retrieve-title-or-filename file ft))
                               (id    (denote-retrieve-filename-identifier file)))
                          (cons title id)))
                      files)))
      sthenno/denote-org-link-completions)

    (defun sthenno/denote-org-link-get-candidates ()
      "Return the list of note title candidates for completion, loading cache
if needed."
      (or sthenno/denote-org-link-completions
          (sthenno/denote-org-link-refresh)))

    (defun sthenno/denote-completion-at-point ()
      "Completion-at-point function for inserting Denote links in Org mode.
Triggers only inside an open Org link “([[...]])”, completing to
“[[denote:ID][Title]]”."
      (let* ((pos (point))
             (open-pos  (save-excursion (search-backward "[[" nil t)))
             (close-pos (save-excursion (search-backward "]]" nil t))))
        (when (and open-pos
                   (or (not close-pos)
                       (< close-pos open-pos))
                   (save-excursion
                     (goto-char (+ open-pos 2))
                     (not (search-forward "]" pos t))))
          (let* ((beg (copy-marker (+ open-pos 2)))
                 (end (copy-marker pos t))
                 (cands (sthenno/denote-org-link-get-candidates)))
            (when cands
              (list beg end (mapcar #'car cands)
                    :annotation-function (lambda (title)
                                           (when-let ((id (cdr (assoc title cands))))
                                             (format " D [%s]"
                                                     (if (fboundp 'denote-id-to-date)
                                                         (denote-id-to-date id)
                                                       ""))))
                    :exit-function (lambda (selection _status)
                                     (when selection
                                       (let ((id (cdr (assoc selection cands))))
                                         (when id
                                           (delete-region beg end)
                                           (insert (format "denote:%s][%s" id selection))))))))))))
    (defun sthenno/capf-text ()
      (setq-local completion-at-point-functions
                  `(,(cape-capf-super
                      (cape-capf-prefix-length #'cape-dict 4)
                      #'sthenno/denote-completion-at-point)
                    cape-elisp-block
                    cape-file
                    cape-dabbrev)
                  cape-dabbrev-min-length 5))
    (add-hook 'text-mode-hook #'sthenno/capf-text))

  :config
  (defun sthenno/cape--symbol-annotation (sym)
    "Return kind of SYM with customized annotations."
    (setq sym (intern-soft sym))
    (cond ((special-form-p sym)    " <s>")
          ((macrop sym)            " <macro>")
          ((commandp sym)          " <cmd>")
          ((fboundp sym)           " <func>")
          ((custom-variable-p sym) " <custom>")
          ((boundp sym)            " <v>")
          ((featurep sym)          " <feat>")
          ((facep sym)             " <face>")
          (t " <sym>")))
  (define-advice cape--symbol-annotation (:override (sym)
                                                    sthenno/cape--symbol-annotation)
    "Modify the annotation for boundp symbols."
    (sthenno/cape--symbol-annotation sym))

  (setq cape--dabbrev-properties (list :annotation-function (lambda (_)
                                                              ;; FIXME: how to avoid
                                                              ;; parsing this as a
                                                              ;; docstring?
                                                              ""
                                                              " <dab>")
                                       :company-kind (lambda (_) 'text)
                                       :exclusive 'no)))

;;; The main completion frontend by Corfu
(use-package corfu
  :ensure t
  :demand t
  :init (add-hook 'after-init-hook #'(lambda ()
                                       (global-corfu-mode 1)))
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.05           ; Making this to 0 is too expensive
        corfu-auto-prefix 1)
  (setq corfu-count 8
        corfu-scroll-margin 4)
  (setq corfu-min-width 20
        corfu-max-width 40)
  (setq corfu-quit-at-boundary 'separator
        corfu-separator ?\s             ; Use space
        corfu-quit-no-match 'separator  ; Don't quit if there is `corfu-separator'
                                        ; inserted
        corfu-preview-current 'insert   ; Preview first candidate. Insert on input if
                                        ; only one
        corfu-on-exact-match 'insert)
  (setq corfu-cycle t)
  (setq corfu-preselect 'directory)

  ;; Performance optimization
  (defun sthenno/corfu-eshell-setup ()
    (setq-local corfu-auto nil)
    (corfu-mode 1)
    (keymap-set corfu-map "RET" #'corfu-send))
  (add-hook 'eshell-mode-hook #'sthenno/corfu-eshell-setup)

  (add-hook 'corfu-mode-hook #'sthenno/completion-style-corfu)
  (keymap-set corfu-map "RET" #'corfu-insert)

  ;; Combined sorting
  (defun sthenno/corfu-combined-sort (candidates)
    "Sort CANDIDATES using both display-sort-function and corfu-sort-function."
    (let ((candidates
           (let ((display-sort-func (corfu--metadata-get 'display-sort-function)))
             (if display-sort-func
                 (funcall display-sort-func candidates)
               candidates))))
      (if corfu-sort-function
          (funcall corfu-sort-function candidates)
        candidates)))
  (setq corfu-sort-override-function #'sthenno/corfu-combined-sort)

  ;; Maintain a list of recently selected candidates
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; Popup candidates info
  (setq corfu-popupinfo-delay '(0.5 . 0.25))
  (setq corfu-popupinfo-hide nil)
  (setq corfu-popupinfo-max-width 80
        corfu-popupinfo-min-width 20)
  (add-hook 'prog-mode-hook #'(lambda ()
                                (corfu-popupinfo-mode 1)))
  :bind (:map corfu-map
              ("<down>"   . corfu-next)
              ("TAB"      . corfu-complete)
              ([tab]      . corfu-complete)
              ("<up>"     . corfu-previous)
              ("<escape>" . corfu-quit)))

(provide 'init-comp)

;;; init-comp.el ends here
