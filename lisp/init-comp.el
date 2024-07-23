;;; init-comp.el --- Modern completion system -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file includes:
;; - completion styles enhancement using `orderless'
;; - minibuffer enhancement using `vertico' and `consult'
;; - pop-up completions by `corfu' as frontend and `cape' as backend
;;
;; NOTE: Package `embark' is not included in this config due to my personal preferences.
;; Template's setups such as that for `abbrev' are placed separately in `init-temp' but
;; `dabbrev' is configured in this file under current decision.

;;; Code:
;;

;;; Build the completion framework
;;
;; Completion and minibuffer basics
;;

;; Completion basics. See also `orderless'
;;

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold nil)

;; Emacs 30: `cape-dict' is used instead.
;; NOTE: `setopt' is necessary.
(setopt text-mode-ispell-word-completion nil)

;; Do not use the TAB key for `completion-at-point'.
(setq tab-always-indent t)
(setq tab-first-completion nil)

;; minibuffer
(setq echo-keystrokes 0.05           ; Display the key pressed immediately
      echo-keystrokes-help t)        ; Display help info for keystrokes in the echo area

;; Support opening new minibuffers from inside existing minibuffers
(setq enable-recursive-minibuffers t)

;; Hide undefined commands in M-x
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (cursor-intangible-mode 1)))

;; Use the `orderless' completion style
(use-package orderless
  :ensure t
  :config

  ;; The basic completion style is specified as fallback in addition to orderless in
  ;; order to ensure that completion commands rely on dynamic completion tables
  ;;
  (setq completion-styles '(basic orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))

                                        ;; There is further configuration for better
                                        ;; `eglot' support. See also `init-eglot'
                                        (eglot      (styles orderless))
                                        (eglot-capf (styles orderless)))))

;; Ignore cases for completions
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Minibuffer faces
(setq text-quoting-style 'straight)


;; Completions in minibuffers
(use-package vertico
  :ensure t
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
  (defun sthenno/vertico-on ()
    (interactive)
    (vertico-mode 1))

  (defun sthenno/vertico-off ()
    (interactive)
    (vertico-mode -1))

  (add-hook 'after-init-hook #'sthenno/vertico-on)

  :config
  (setq vertico-count 8)
  (setq vertico-scroll-margin 4)
  (setq vertico-cycle nil)

  ;; Add some simple indicator symbols here to make things clear
  (setq vertico-count-format (cons "◇ %-6s ◈ " "%s of %s"))

  ;; Do not render italic fonts
  (set-face-attribute 'vertico-group-title nil :slant 'normal)

  ;; Prefix candidates
  (defvar sthenno/vertico-using-prefix-symbol-p t)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context
          ((and sthenno/vertico-using-prefix-symbol-p
                (not (bound-and-true-p vertico-flat-mode)))
           (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (modus-themes-with-colors
      (if (= vertico--index index)
          (concat (propertize "◉ " 'face
                              `(:background ,bg-hl-line :inherit modus-themes-prompt))
                  cand)
        (concat (propertize "○ " 'face `(:foreground ,fg-dim))
                cand))))

  ;; Candidate display transformations
  ;;
  (defvar sthenno/vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context
          ((not sthenno/vertico-transform-functions) null))
    (dolist (fun (ensure-list sthenno/vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun sthenno/vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'marginalia-file-priv-dir)
      file))


  ;; Multiform
  ;;
  (defun sthenno/vertico-muiltiform-on ()
    (interactive)
    (vertico-multiform-mode 1))

  (defun sthenno/vertico-muiltiform-off ()
    (interactive)
    (vertico-multiform-mode -1))

  (sthenno/vertico-muiltiform-on)

  (add-to-list
   'vertico-multiform-categories
   '(file (sthenno/vertico-transform-functions . sthenno/vertico-highlight-directory)
          (vertico-cycle . t)))

  (add-to-list 'vertico-multiform-categories '(buffer (vertico-cycle . t)))

  ;; Additions for moving up and down directories in `find-file'
  ;;

  ;; Update minibuffer history with candidate insertions
  (define-advice vertico-insert (:after (&rest _) vertico-insert-add-history)
    "Make vertico-insert add to the minibuffer history."
    (unless (eq minibuffer-history-variable t)
      (add-to-history minibuffer-history-variable (minibuffer-contents))))

  ;; Pre-select previous directory when entering parent directory from `find-file'
  ;;

  (defvar previous-directory nil
    "The directory that was just left. It is set when leaving a directory and
    set back to nil once it is used in the parent directory.")

  (defun set-previous-directory ()
    "Set the directory that was just exited from within find-file."
    (when (> (minibuffer-prompt-end) (point))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (minibuffer-prompt-end) t)
          (setq previous-directory (buffer-substring (1+ (point)) (point-max)))
          (when (not (string-suffix-p "/" previous-directory))
            (setq previous-directory nil))
          t))))
  (advice-add #'vertico-directory-up :before #'set-previous-directory)

  ;; Advise `vertico--update' to select the previous directory
  (define-advice vertico--update (:after (&rest _) choose-candidate)
    "Pick the previous directory rather than the prompt after updating candidates."
    (cond
     (previous-directory                ; select previous directory
      (setq vertico--index (or (seq-position vertico--candidates previous-directory)
                               vertico--index))
      (setq previous-directory nil))))

  ;; Correct file path when changed (tidy shadowed file names)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  :bind ((:map vertico-map
               ("<tab>"       . vertico-insert)
               ("s-<right>"   . vertico-insert)
               ("<return>"    . vertico-directory-enter)
               ("s-<down>"    . vertico-directory-enter)
               ("s-<left>"    . vertico-directory-up)
               ("<backspace>" . vertico-directory-delete-char))))


;; Rich annotations for minibuffer
(use-package marginalia
  :ensure t
  :init
  (setq marginalia-field-width 45)
  (setq marginalia-separator " ")
  (setq marginalia-align 'left)
  (setq marginalia-align-offset 4)
  (marginalia-mode 1))


;; Consult is useful previewing current content in buffer
(use-package consult
  :ensure t
  :init
  (setq register-preview-delay 0.05
        register-preview-function #'consult-register-format)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config

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

  (consult-customize consult-line
                     :add-history (seq-some #'thing-at-point '(region symbol))
                     :initial (thing-at-point 'symbol))

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
               ("C-d" . consult-buffer)
               ("C-s" . consult-line)
               ("C-f" . consult-line-multi)
               ("C-v" . consult-yank-pop)
               ("s-m" . consult-imenu-multi)
               ("s-n" . consult-recent-file)
               ("M-i" . consult-info))
         (:map org-mode-map
               ("s-m" . consult-org-heading)
               ("M-a" . consult-org-agenda))))


;; Dabbrev settings
(use-package dabbrev
  :config

  ;; Better letter cases
  (setq dabbrev-case-distinction t
        dabbrev-upcase-means-case-search t)

  (defun sthenno/dabbrev-elisp ()
    "Setup `dabbrev' for `emacs-lisp-mode'."
    (setq-local dabbrev-case-fold-search nil)
    (setq-local dabbrev-case-replace nil))
  (add-hook 'emacs-lisp-mode-hook #'sthenno/dabbrev-elisp)

  ;; Ignore these for `dabbrev'
  ;;
  ;; (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")

  (add-to-list 'dabbrev-ignored-buffer-modes #'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes #'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes #'tags-table-mode))

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
                `(,(cape-capf-super (cape-capf-buster
                                     #'eglot-completion-at-point)
                                    #'cape-dabbrev)
                  cape-file)
                cape-dabbrev-min-length 4))
  (add-hook 'eglot-managed-mode-hook #'sthenno/capf-eglot)

  (defun sthenno/capf-elisp ()
    (setq-local completion-at-point-functions
                `(,(cape-capf-super
                    (cape-capf-predicate
                     #'cape-elisp-symbol
                     #'(lambda (cand)
                         (or (not (keywordp cand))
                             (eq (char-after (car completion-in-region--data)) ?:))))
                    #'cape-dabbrev)
                  cape-file)
                cape-dabbrev-min-length 4))
  (add-hook 'emacs-lisp-mode-hook #'sthenno/capf-elisp)

  (defun sthenno/capf-text ()
    (setq-local completion-at-point-functions
                `(,(cape-capf-super #'cape-dict
                                    #'cape-dabbrev)
                  ,(cape-capf-inside-code #'cape-elisp-symbol)
                  cape-file
                  cape-elisp-block)
                cape-dabbrev-min-length 2))
  (add-hook 'text-mode-hook #'sthenno/capf-text)

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

  (setq cape--dabbrev-properties (list :annotation-function (lambda (_) " <dab>")
                                       :company-kind (lambda (_) 'text)
                                       :exclusive 'no)))


;; The main completion frontend by Corfu
(use-package corfu
  :ensure t
  :init (add-hook 'after-init-hook #'(lambda ()
                                       (global-corfu-mode 1)))
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.05           ; Making this to 0 is too expensive
        corfu-auto-prefix 2)

  (setq corfu-count 8
        corfu-scroll-margin 4)

  (setq corfu-min-width 5
        corfu-max-width 40)

  (setq corfu-quit-at-boundary t
        corfu-quit-no-match t
        corfu-on-exact-match 'quit)

  (setq corfu-preview-current nil)
  (setq corfu-cycle nil)

  ;; Performance optimization
  ;;
  (defun sthenno/corfu-eshell-setup ()
    (setq-local corfu-auto nil)
    (corfu-mode 1)
    (keymap-set corfu-map "RET" #'corfu-send))

  (add-hook 'eshell-mode-hook #'sthenno/corfu-eshell-setup)

  ;; Use special key to insert
  ;;
  ;; Like first, but select the prompt if it is a directory
  (setq corfu-preselect 'directory)

  ;; Use convenient keys to insert candidates of `corfu--candidates'. Since the first
  ;; candidate is usually pre-selected, it is better to trigger `corfu--insert'
  ;; depending on different conditions.
  ;;
  (defun sthenno/corfu-insert-key (key)
    "Insert selected candidate in `corfu--candidates' and KEY.

Do not `corfu--insert' if
  - `corfu--candidates' is empty |
  - `corfu--preselect'  is the prompt |
  - `corfu--index'      is the first candidate in `corfu--candidates'.

Do not insert KEY if `char-after' point is not empty."

    ;; Check if `corfu--insert'
    (let ((c (cond ((equal key "SPC") ?\s)
                   (t (aref key 0)))))
      (if (> corfu--index 0)
          (progn
            (corfu--insert 'finished)

            ;; Check if insert key
            (let ((p (or (not (char-after))
                         (= (char-after) ?\s)
                         (= (char-after) ?\n))))
              (if p (insert c)
                (corfu-quit)))
            (corfu-quit))
        (progn
          (corfu-quit)
          (insert c)))))

  (dolist (k '("SPC" "." "," ":" ")" "}" "]" "'"))
    (keymap-set corfu-map k #'(lambda ()
                                (interactive)
                                (sthenno/corfu-insert-key k))))

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
  ;;
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; Popup candidates info
  ;;
  (setq corfu-popupinfo-delay '(0.25 . 0.05))
  (setq corfu-popupinfo-hide nil)

  (setq corfu-popupinfo-max-width 40
        corfu-popupinfo-min-width 20)

  (add-hook 'prog-mode-hook #'(lambda ()
                                (corfu-popupinfo-mode 1)))

  :bind (:map corfu-map
              ("<down>"   . corfu-next)
              ("<tab>"    . corfu-next)
              ("<up>"     . corfu-previous)
              ("<escape>" . corfu-quit)))

(provide 'init-comp)
