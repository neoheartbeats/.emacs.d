;;; init-comp.el --- Modern completion system -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:
;;

;;; Build the completion framework
(use-package emacs
  :init

  ;;; Completion basics. See also `orderless'
  ;;
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Only list the commands of the current modes
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; Emacs 30: `cape-dict' is used instead
  (setq text-mode-ispell-word-completion nil)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package minibuffer
  :init
  (setq minibuffer-default-prompt-format " [%s]")
  (setq echo-keystrokes 0.02)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Use the `orderless' completion style
(use-package orderless
  :straight t
  :config
  (setq orderless-component-separator " +\\|[-/]") ; Spaces, hyphen or slash
  (setq completion-styles '(orderless flex basic)
        completion-category-overrides '((file (styles . (partial-completion orderless)))
                                        (eglot (styles . (orderless))))))

;; Ignore cases
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)


;;; Completion for minibuffers
(use-package vertico
  :straight t
  :init (vertico-mode 1)
  :config
  (setq vertico-count 10)
  (setq vertico-scroll-margin 4)
  (setq vertico-cycle t)
  (setq vertico-count-format (cons "%-6s " "[%s/%s]"))

  ;; Do not render italic fonts
  (set-face-attribute 'vertico-group-title nil :slant 'normal)

  ;; Prefix candidates
  (defvar +vertico-current-arrow t)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                   (not (bound-and-true-p vertico-flat-mode)))
                                              (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (= vertico--index index)
        (concat #("◉ " 0 1 (face modus-themes-prompt)) cand)
      (concat #("○ " 0 1 (face shadow)) cand)))

  (use-package vertico-multiform
    :init (vertico-multiform-mode 1)
    :config
    (defvar +vertico-transform-functions nil)

    (cl-defmethod vertico--format-candidate :around
      (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
      (dolist (fun (ensure-list +vertico-transform-functions))
        (setq cand (funcall fun cand)))
      (cl-call-next-method cand prefix suffix index start))

    (defun sort-directories-first (files)

      ;; Still sort by history position, length and alphabetically
      (setq files (vertico-sort-history-length-alpha files))

      ;; But then move directories first
      (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
             (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

    (defun +vertico-highlight-directory (file)
      "If FILE ends with a slash, highlight it as a directory."
      (if (string-suffix-p "/" file)
          (propertize file 'face 'dired-directory)
        file))

    ;; add-to-list works if 'file isn't already in the alist
    ;; setq can be used but will overwrite all existing values
    (add-to-list 'vertico-multiform-categories
                 '(file

                   ;; this is also defined in the wiki, uncomment if used
                   (vertico-sort-function . sort-directories-first)
                   (+vertico-transform-functions . +vertico-highlight-directory))))

  ;; Additions for moving up and down directories in `find-file'
  (use-package vertico-directory
    :config

    ;; Update minibuffer history with candidate insertions
    (defun vertico-insert-add-history ()
      "Make `vertico-insert' add to the minibuffer history."
      (unless (eq minibuffer-history-variable t)
        (add-to-history minibuffer-history-variable (minibuffer-contents))))

    (advice-add 'vertico-insert :after #'vertico-insert-add-history)

    ;; Pre-select previous directory when entering parent directory from within `find-file'
    ;;
    ;; Advise `vertico-directory-up' to save the directory being exited
    (defvar previous-directory nil
      "The directory that was just left. It is set when leaving a directory and
    set back to nil once it is used in the parent directory.")

    (defun set-previous-directory ()
      "Set the directory that was just exited from within find-file."
      (when (> (minibuffer-prompt-end) (point))
        (save-excursion
          (goto-char (1- (point)))
          (when (search-backward "/" (minibuffer-prompt-end) t)

            ;; Set parent directory
            (setq previous-directory (buffer-substring (1+ (point)) (point-max)))

            ;; Set back to nil if not sorting by directories or what was deleted is not a directory
            (when (not (string-suffix-p "/" previous-directory))
              (setq previous-directory nil))
            t))))

    (advice-add #'vertico-directory-up :before #'set-previous-directory)

    ;; Advise `vertico--update' to select the previous directory
    (define-advice vertico--update (:after (&rest _) choose-candidate)
      "Pick the previous directory rather than the prompt after updating candidates."
      (cond
       (previous-directory ; select previous directory
        (setq vertico--index (or (seq-position vertico--candidates previous-directory)
                                 vertico--index))
        (setq previous-directory nil))))

    ;; Left-truncate `recentf' filename candidates
    (defun my/vertico-truncate-candidates (args)
      (if-let ((arg (car args))
               (type (get-text-property 0 'multi-category arg))
               ((eq (car-safe type) 'file))
               (w (max 30 (- (window-width) 38)))
               (l (length arg))
               ((> l w)))
          (setcar args (concat "…" (truncate-string-to-width arg l (- l w)))))
      args)
    (advice-add #'vertico--format-candidate :filter-args #'my/vertico-truncate-candidates)

    ;; Correct file path when changed (tidy shadowed file names)
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

    :bind (:map vertico-map
                ("<return>" . vertico-directory-enter)
                ("<backspace>" . vertico-directory-delete-char)
                ("M-<backspace>" . vertico-directory-delete-word)))

  :bind ((:map vertico-map
               ("<tab>" . vertico-insert))))

;; Support opening new minibuffers from inside existing minibuffers
;; (setq enable-recursive-minibuffers t)

;; Disable showing the *Completions* buffer that conflicts with vertico
;; if using `ffap-menu'
(advice-add #'ffap-menu-ask :around
            (lambda (&rest args)
              (cl-letf (((symbol-function #'minibuffer-completion-help)
                         #'ignore))
                (apply args))))


;; Rich annotations for minibuffer
;;
;; [TODO] Propertize `marginalia'
;;
(use-package marginalia
  :straight t
  :init (marginalia-mode 1))


;; Consult is useful previewing current content in buffer
(use-package consult
  :straight t
  :init
  (global-set-key (kbd "s-b") 'switch-to-buffer)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key
   [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key
   [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)

  ;; Framework for mode-specific buffer indexes
  (global-set-key [remap imenu] 'consult-imenu)

  :config

  ;; Back to last visited by C-s C-s if using `consult-line'
  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))
  (consult-customize consult-line :keymap my-consult-line-map)

  ;; https://github.com/minad/consult/wiki#add-category-specific-minibuffer-keybindings
  (defun define-minibuffer-key (key &rest defs)
    "Define KEY conditionally in the minibuffer.
DEFS is a plist associating completion categories to commands."
    (define-key minibuffer-local-map key
		        (list 'menu-item nil defs :filter
		              (lambda (d)
			            (plist-get d (completion-metadata-get
				                      (completion-metadata (minibuffer-contents)
							                               minibuffer-completion-table
							                               minibuffer-completion-predicate)
				                      'category))))))

  (define-minibuffer-key "\C-s"
			             'consult-location #'previous-history-element
			             'file #'consult-find-for-minibuffer)

  :bind (:map global-map
	          ("C-s" . consult-line)
	          ("M-s" . consult-ripgrep)
              ("C-v" . consult-yank-from-kill-ring)
              ("s-m" . consult-imenu)))


;; Beframe (beframe.el): Isolate Emacs buffers per frame
(use-package beframe
  :straight t
  :config
  (setq beframe-global-buffers nil
        beframe-create-frame-scratch-buffer nil)
  (beframe-mode 1)
  
  ;; Integration with `consult-buffer'
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defun my-beframe-buffer-names-sorted (&optional frame)
      "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
      (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

    (defvar beframe-consult-source
      `( :name     "Frame-specific buffers"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'my-beframe-buffer-names-sorted
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe-consult-source)))


;; Dabbrev settings
(use-package dabbrev
  :config

  ;; Better letter casesx
  (setq dabbrev-case-distinction nil
	    dabbrev-case-replace nil
	    dabbrev-case-fold-search t
	    dabbrev-upcase-means-case-search t)

  ;; See https://github.com/minad/corfu
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")

  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes #'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes #'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes #'tags-table-mode))

;; Add extensions for the completion backend
(use-package cape
  :straight t
  :config
  (setq cape-dabbrev-min-length 4)

  (defun completion-at-point-functions-setup (capfs-map)
    "Set up completion at point functions based on CAPFS-MAP.

CAPFS-MAP is an association list where each key is a major mode symbol
and each value is a list of functions to add to `completion-at-point-functions'."
    (dolist (mode-func-pair capfs-map)
      (let ((mode (car mode-func-pair))
            (functions (cdr mode-func-pair)))

        ;; Add functions to specific major mode
        (add-hook (intern (concat (symbol-name mode) "-hook"))
                  (lambda ()
                    (dolist (func functions)
                      (add-to-list 'completion-at-point-functions func)))))))

  (defvar my/capfs-map
    '((prog-mode . (cape-dict
                    cape-file))
      (emacs-lisp-mode . (cape-dict
                          cape-file
                          cape-elisp-symbol))
      (org-mode . (cape-dict
                   cape-elisp-block
                   cape-file)))
    "An alist of (mode . list-of-capfs) to append.
The elements in list-of-capfs further down the list have deeper priority in completion.")

  (completion-at-point-functions-setup my/capfs-map))


;; The main completion frontend by Corfu
(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :init (global-corfu-mode 1)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.02 ; Making this to 0 is too expensive
        corfu-auto-prefix 2)

  (setq corfu-quit-at-boundary 'separator
        corfu-quit-no-match t)

  (setq corfu-cycle t)
  (setq corfu-preview-current nil)
  (setq corfu-preselect 'directory) ; Auto select the first except directories

  ;; Maintain a list of recently selected candidates
  ;; This requires `savehist-mode' is enabled
  (require 'corfu-history)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  :bind (:map corfu-map
              ("<down>" . corfu-next)
	          ("<tab>" . corfu-next)
              ("<up>" . corfu-previous)
	          ("s-<tab>" . corfu-previous)
              ("<escape>" . corfu-quit)))

(provide 'init-comp)
;;;
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;
