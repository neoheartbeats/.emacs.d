;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file enhances the editing experience in Emacs.

;;; Code:
;;


;;
;; Global functions for editing enhancement
;;

(defun my/delete-current-line ()
  "Delete the current line."
  (interactive)
  (delete-region (line-beginning-position) (line-beginning-position 2))
  (run-hooks 'my/delete-current-line-hook))

(defun my/delete-to-beginning-of-line ()
  "Delete from the current position to the beginning of the line."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(global-set-key (kbd "s-<backspace>") #'my/delete-current-line)
(global-set-key (kbd "C-<backspace>") #'my/delete-to-beginning-of-line)

(defun my/open-newline-below ()
  "Open a new line below the current one and move the cursor to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "s-<return>") #'my/open-newline-below)

;; Pretty-print
;;
;; XXX: Pretty-print comes from various mechanisms respecting to the current programming
;; language. For example, formatters like Black are preferred over `indent-region' and
;; `untabify' for formatting Python code since a formatter usually provides a full
;; combination of executions that covers the basic functions that Emacs provides. Thus,
;; behaviors of `my/pretty-print-current-buffer' should be considered separately for
;; each particular case especially when `my/enable-pretty-print-on-save' is demanded.
;;
;; NOTE: I did not intentionally distinguish between Indent, Pretty-print, and Format,
;; although there are minor differences in their applicable scenarios. Any
;; formatting-related concepts in this configuration is collectively referred to as
;; Pretty-print.
;;
(defun indent-current-buffer ()
  "Indent current buffer.
If `major-mode' is `python-mode', abort."
  (interactive)
  (if (derived-mode-p 'python-mode)
      (message "Indentation does not support for Python.")
    (save-excursion
      (indent-region (point-min) (point-max) nil))
    (run-hooks 'indent-current-buffer-hook)))

(defun indent-current-comment-buffer ()
  "Indent comment for current buffer."
  (interactive)
  (let ((lo (point-min))
        (hi (point-max)))
    (save-excursion
      (setq hi (copy-marker hi))
      (goto-char lo)
      (while (< (point) hi)
        (if (comment-search-forward hi t)
            (comment-indent)
          (goto-char hi))))))

(defun untabify-current-buffer ()
  "Convert all tabs to multiple spaces for current buffer."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max)))
  (run-hooks 'untabify-current-buffer-hook))

(defun my/pretty-print-current-buffer ()
  "Pretty print current buffer."
  (interactive)
  (save-excursion
    (indent-current-buffer)
    (indent-current-comment-buffer)
    (untabify-current-buffer)
    (delete-trailing-whitespace))
  (run-hooks 'my/pretty-print-current-buffer-hook))

(defun my/enable-pretty-print-on-save ()
  "Enable pretty print before save in `emacs-lisp-mode'."
  (add-hook 'before-save-hook #'my/pretty-print-current-buffer nil t))

(add-hook 'emacs-lisp-mode-hook #'my/enable-pretty-print-on-save)

(global-set-key (kbd "s-p") #'my/pretty-print-current-buffer)


;; TODO: Vundo
;;
(use-package vundo
  :straight t
  :bind (:map global-map
              ("C-z" . vundo)))


;; Fold code lines
(use-package hideshow
  :config
  (defun my-hs-set-up-overlay (ov)
    (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0]) ; A plus sign
    (modus-themes-with-colors
      (when (eq 'code (overlay-get ov 'hs))
        (let* ((marker-string "*fringe-dummy*")
               (marker-length (length marker-string))
               (display-string " 􀄪 "))
          (put-text-property 0 marker-length 'display
                             (list 'left-fringe 'hs-marker 'fringe-face)
                             marker-string)
          (overlay-put ov 'before-string marker-string)
          (put-text-property 1 (1- (length display-string))
                             'face `(:foreground ,fg-changed :background ,bg-changed)
                             display-string)
          (overlay-put ov 'display display-string)))))

  (setq hs-set-up-overlay #'my-hs-set-up-overlay)

  (add-hook 'prog-mode-hook #'(lambda ()
                                (hs-minor-mode 1)))
  :bind (:map prog-mode-map
              ("M-<tab>" . hs-toggle-hiding)
              ("M--"     . hs-hide-all)
              ("M-="     . hs-show-all)))


;;; Incremental Search using `isearch'
;;
;; Also check `consult'
;;
(use-package isearch
  :config (setq isearch-allow-scroll t)
  :bind
  (:map global-map
        ("s-f" . isearch-forward))
  (:map isearch-mode-map
        ("<down>"   . isearch-repeat-forward)
        ("<up>"     . isearch-repeat-backward)
        ("<escape>" . isearch-exit)
        ("<return>" . isearch-exit)))


;; HACK: Inhibit passing these delimiters
;;
;; (defun my-inhibit-specific-delimiters ()
;;   "Remove the following from current `syntax-table'. This disables syntax highlighting
;; and auto-paring for such entries."
;;   (modify-syntax-entry ?< "." (syntax-table))
;;   (modify-syntax-entry ?> "." (syntax-table)))

;; Automatic pair parenthesis
(use-package elec-pair
  :init

  ;; `python-mode' disables `electric-indent-mode' by default since Python does not
  ;; lend itself to fully automatic indentation. Org Mode should disable this for the
  ;; same reason.
  ;; XXX: Side-effects come to `org-babel' is under discovering.
  (add-hook 'org-mode-hook #'(lambda ()
                               (setq electric-indent-inhibit t)))

  :config (electric-pair-mode 1))

(use-package paren
  :init (setq show-paren-delay 0.05
              show-paren-highlight-openparen t
              show-paren-style 'mixed   ; Highlight parenthesis matched off-screen
              show-paren-when-point-inside-paren t))

;; Using rainbow delimiters
(use-package rainbow-delimiters
  :straight t
  :diminish (rainbow-delimiters-mode)
  :config (add-hook 'prog-mode-hook #'(lambda ()
                                        (rainbow-delimiters-mode 1))))

;; Show docstring in echo area
(use-package eldoc
  :diminish (eldoc-mode)
  :init (setq eldoc-idle-delay 0.05))

;;; Deletions
;;
;; Delete selection if you insert
;;
(use-package delsel
  :init (add-hook 'after-init-hook #'(lambda ()
                                       (delete-selection-mode 1))))

(setq backward-delete-char-untabify-method 'hungry)

;; Automatically reload files was modified by external program
(use-package autorevert
  :diminish (auto-revert-mode)
  :init (add-hook 'after-init-hook #'(lambda ()
                                       (global-auto-revert-mode 1))))

;;; Fill columns
;;
;; Face `fill-column-indicator' is set in `init-gui-frames'
;;
(add-hook 'after-init-hook #'(lambda ()
                               (global-display-fill-column-indicator-mode 1)))

;; Display line numbers
(setq-default display-line-numbers-width 4)
(add-hook 'prog-mode-hook #'(lambda ()
                              (display-line-numbers-mode 1)))


(use-package pulsar
  :straight t
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.05
        pulsar-iterations 15)

  ;; Hooks
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-cyan)
  (add-hook 'indent-current-buffer-hook #'pulsar-pulse-line-cyan)
  (add-hook 'after-save-hook #'pulsar-pulse-line-green)

  (add-hook 'my/delete-current-line-hook #'pulsar-pulse-line-magenta)
  (add-hook 'my/cycle-to-next-buffer-hook #'pulsar-pulse-line-cyan)
  (add-hook 'my/cycle-to-previous-buffer-hook #'pulsar-pulse-line-cyan)

  (add-hook 'isearch-mode-end-hook #'pulsar-highlight-line)

  (add-hook 'split-window-below-focus-hook #'pulsar-highlight-line)
  (add-hook 'split-window-right-focus-hook #'pulsar-highlight-line)

  (pulsar-global-mode 1))


(use-package indent-bars
  :straight (indent-bars
             :type git
             :host github
             :repo "jdtsmith/indent-bars")
  :init
  (setq indent-bars-treesit-support t
        indent-bars-treesit-ignore-blank-lines-types '("module"))

  ;; Stipple-based pixle-toggling is not supported by NS built Emacs
  (setq indent-bars-prefer-character t)

  :config
  (setq indent-bars-color '(highlight :face-bg t :blend 0.4)
        indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
        indent-bars-highlight-current-depth '(:blend 0.8)
        indent-bars-starting-column 0
        indent-bars-display-on-blank-lines t)

  (add-hook 'python-ts-mode-hook #'(lambda ()
                                     (indent-bars-mode 1))))


;;
;; Highlight these keywords in code comments
;;

(use-package hl-todo
  :straight t
  :init
  (defun my-hl-todo-faces-setup ()
    (modus-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("TODO"  . ,prose-todo)
              ("FIXME" . ,err)
              ("XXXX*" . ,err)
              ("NOTE"  . ,fg-changed)
              ("HACK"  . ,fg-changed)))))

  (add-hook 'after-init-hook #'my-hl-todo-faces-setup)

  (global-hl-todo-mode 1))


;;; Spell checking using Jinx
;;
;; GNU Aspell is used as the backend by default.
;; Enchant need to be installed first. Use "brew install enchant" on macOS.
;; Personal dictionary is located at "~/.config/enchant/en_US.dic" on macOS.
;;
(use-package jinx
  :straight t
  :init
  (setq jinx-languages "en_US"
        jinx-menu-suggestions 5)

  (setq jinx--save-keys
        `((?= . ,#'jinx--save-personal)))

  ;; Overwrite these functions
  ;;
  (cl-defun jinx--correct-overlay (overlay &key info initial)
    "Correct word at OVERLAY.
Optionally show prompt INFO and insert INITIAL input."
    (catch 'jinx--goto
      (let* ((word (buffer-substring-no-properties
                    (overlay-start overlay) (overlay-end overlay)))
             (choice
              (jinx--correct-highlight overlay
                (lambda ()
                  (when (or (< (point) (window-start)) (> (point) (window-end nil t)))
                    (recenter))
                  (minibuffer-with-setup-hook
                      #'jinx--correct-setup
                    (or (completing-read
                         (format "Correct '%s'%s: " word (or info ""))
                         (jinx--correct-table
                          (jinx--correct-suggestions word))
                         nil nil initial t word)
                        word)))))
             (len (length choice)))
        (pcase (and (> len 0) (assq (aref choice 0) jinx--save-keys))
          (`(,key . ,fun)
           (funcall fun 'save key
                    (if (> len 1) (substring-no-properties choice 1) word))
           (jinx--recheck-overlays))
          ((guard (not (equal choice word)))
           (jinx--correct-replace overlay choice)))
        nil)))

  (defun jinx--correct-suggestions (word)
    "Retrieve suggestions for WORD from all dictionaries."
    (let ((ht (make-hash-table :test #'equal))
          (list nil))
      (dolist (dict jinx--dicts)
        (let* ((desc (jinx--mod-describe dict))
               (group (format "Suggestions from dictionary '%s' - %s"
                              (car desc) (cdr desc))))
          (dolist (w (jinx--mod-suggest dict word))
            (setq list (jinx--add-suggestion list ht w group)))))
      (dolist (w (jinx--session-suggestions word))
        (setq list (jinx--add-suggestion list ht w "Suggestions from session")))
      (cl-loop for (key . fun) in jinx--save-keys
               for actions = (funcall fun nil key word) do
               (when (and actions (not (consp (car actions))))
                 (setq actions (list actions)))
               (cl-loop for (k w a) in actions do
                        (push (propertize
                               (concat (propertize (if (stringp k) k (char-to-string k))
                                                   'face 'jinx-save 'rear-nonsticky t)
                                       w)
                               'jinx--group "Accept"
                               'jinx--suffix
                               (format #(" [%s]" 0 5 (face jinx-annotation)) a))
                              list)))
      (nreverse list)))

  ;; Hooks
  ;;
  (add-hook 'org-mode-hook #'(lambda ()
                               (jinx-mode 1)))

  :bind (:map jinx-overlay-map
              ("SPC" . jinx-correct)))

(provide 'init-editing-utils)
