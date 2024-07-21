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
;; XXX: Template's setups such as that for `abbrev' are placed separately in
;; `init-temp' but `dabbrev' is configured in this file under current decision.

;;; Code:
;;

;;; Build the completion framework
;;
;; Completion and minibuffer basics
;;

;; Completion basics. See also `orderless'
;; TAB cycle if there are only few candidates
;;
(setq completion-cycle-threshold nil)

;; Emacs 30: `cape-dict' is used instead.
;; NOTE: `setopt' is necessary.
(setopt text-mode-ispell-word-completion nil)

;; Enable indentation+completion using the TAB key `completion-at-point' is often
;; bound to M-TAB
(setq tab-always-indent 'complete)

;; minibuffer
;;
;; (setq minibuffer-default-prompt-format " [%s]")

(setq echo-keystrokes 0.05           ; Display the key pressed immediately
      echo-keystrokes-help t)        ; Display help info for keystrokes in the echo area

;; Support opening new minibuffers from inside existing minibuffers
(setq enable-recursive-minibuffers t)

;; Hide undefined commands in M-x
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Use the `orderless' completion style
;;
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
                                        (eglot      (styles basic orderless))
                                        (eglot-capf (styles basic orderless)))))

;; Ignore cases for completions
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)


;; Completions in minibuffers
;;
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

  (vertico-mode 1)

  :config
  (setq vertico-count 8)
  (setq vertico-scroll-margin 4)
  (setq vertico-cycle t)

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

  ;; Additions for moving up and down directories in `find-file'
  ;;

  ;; Update minibuffer history with candidate insertions
  (defun vertico-insert-add-history ()
    "Make `vertico-insert' add to the minibuffer history."
    (unless (eq minibuffer-history-variable t)
      (add-to-history minibuffer-history-variable (minibuffer-contents))))

  (advice-add 'vertico-insert :after #'vertico-insert-add-history)

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
;;
(use-package marginalia
  :ensure t
  :init
  (setq marginalia-field-width 88)
  (setq marginalia-separator (propertize " :  " 'face '(:inherit shadow)))
  (setq marginalia-align 'left)
  (setq marginalia-align-offset 4)
  (marginalia-mode 1))


;;; Consult is useful previewing current content in buffer
;;
;; See also `beframe', `consult-denote'.
;;
(use-package consult
  :ensure t
  :after (org)
  :init
  (setq register-preview-delay 0.05
        register-preview-function #'consult-register-format)

  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Customize `consult' minibuffer prompts
  (declare-function consult-buffer "consult")

  (defun sthenno/consult-buffer (&optional sources)
    "Like `consult-buffer', but use a different `:prompt' string."
    (interactive)
    (let ((selected (consult--multi (or sources consult-buffer-sources)
                                    :require-match
                                    (confirm-nonexistent-file-or-buffer)
                                    :prompt "Buffer → "
                                    :history 'consult--buffer-history
                                    :sort nil)))

      ;; For non-matching candidates, fall back to buffer creation
      (unless (plist-get (cdr selected) :match)
        (consult--buffer-action (car selected)))))

  (advice-add #'sthenno/consult-buffer :override #'consult-buffer)

  :config

  ;; Previewing files in find-file
  ;;
  (setq read-file-name-function #'consult-find-file-with-preview)

  (defun consult-find-file-with-preview (prompt &optional
                                                dir default mustmatch initial pred)
    (interactive)
    (let ((default-directory (or dir default-directory))
          (minibuffer-completing-file-name t))
      (consult--read #'read-file-name-internal :state (consult--file-preview)
                     :prompt prompt
                     :initial initial
                     :require-match mustmatch
                     :predicate pred)))

  ;; Use `consult-ripgrep' instead of `project-find-regexp' in `project'
  ;;
  (keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)
  (cl-nsubstitute-if '(consult-ripgrep "Find regexp")
                     (pcase-lambda (`(,cmd _))
                       (eq cmd #'project-find-regexp))
                     project-switch-commands)

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

;; Beframe: Isolate Emacs buffers per frame
;;
;; This package is simply used to filter and group minibuffer candidates
;;
(use-package beframe
  :ensure t
  :after (consult)
  :init
  (setq beframe-global-buffers nil
        beframe-create-frame-scratch-buffer nil)
  (beframe-mode 1)

  ;; Integration with `consult-buffer'
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state   "consult")
  (declare-function consult--buffer-display "consult")

  (defface beframe-buffer
    '((t :inherit font-lock-string-face))
    "Face for `consult' framed buffers.")

  (defun sthenno/beframe-buffer-names-sorted (&optional frame)
    "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
Optional argument FRAME, return the list of buffers of FRAME."
    (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

  (defvar beframe-consult-source
    `( :name     "Frame-specific buffers"
       :narrow   ?s
       :category buffer
       :face     beframe-buffer
       :history  beframe-history
       :items    ,#'sthenno/beframe-buffer-names-sorted
       :action   ,#'consult--buffer-display
       :state    ,#'consult--buffer-state))

  (add-to-list 'consult-buffer-sources 'beframe-consult-source))


;; Dabbrev settings
;;
(use-package dabbrev
  :config

  ;; Better letter cases
  (setq dabbrev-case-distinction t
        dabbrev-case-replace nil
        dabbrev-case-fold-search t
        dabbrev-upcase-means-case-search t)

  ;; Ignore these for `dabbrev'
  ;; See https://github.com/minad/corfu
  ;;
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")

  (add-to-list 'dabbrev-ignored-buffer-modes #'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes #'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes #'tags-table-mode))

;; Add extensions for the completion backend
;;
(use-package cape
  :ensure t
  :config
  (setq cape-dabbrev-min-length 2)

  ;; Dict
  ;;
  (setq cape-dict-case-fold t
        cape-dict-case-replace nil)
  (setq cape-dict-limit 40)

  (defun completion-at-point-functions-setup (capfs-map-alist)
    "Set up completion at point functions based on CAPFS-MAP-ALIST.
CAPFS-MAP-ALIST is an association list where each key is a major mode symbol
and each value is a list of functions to add to `completion-at-point-functions'."
    (dolist (mode-func-pair capfs-map-alist)
      (let ((mode (car mode-func-pair))
            (functions (cdr mode-func-pair)))

        ;; Add functions to specific major mode
        (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                  (lambda ()
                    (dolist (func functions)
                      (add-to-list 'completion-at-point-functions func)))))))

  (defvar sthenno/capfs-map-alist '((prog       . (cape-dict
                                                   cape-file
                                                   cape-abbrev
                                                   cape-dabbrev))
                                    (emacs-lisp . (cape-dict
                                                   cape-file
                                                   cape-elisp-symbol
                                                   cape-abbrev
                                                   cape-dabbrev))
                                    (text       . (cape-dict
                                                   cape-abbrev
                                                   cape-dabbrev))
                                    (org        . (cape-dict
                                                   cape-file
                                                   cape-abbrev
                                                   cape-dabbrev)))
    "An alist of (MODE-NAME . CAPFS) to append.")

  (completion-at-point-functions-setup sthenno/capfs-map-alist))


;; The main completion frontend by Corfu
;;
(use-package corfu
  :ensure t
  :init (global-corfu-mode 1)
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
