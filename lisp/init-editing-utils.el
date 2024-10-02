;;; init-editing-utils.el --- Editing helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file enhances the editing experience in Emacs.

;;; Code:
;;


;; Global functions for editing enhancement
;;

(defun sthenno/delete-current-line ()
  "Delete the current line."
  (interactive)
  (delete-region (line-beginning-position) (line-beginning-position 2))
  (run-hooks 'sthenno/delete-current-line-hook))

(defun sthenno/delete-to-beginning-of-line ()
  "Delete from the current position to the beginning of the line."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(global-set-key (kbd "s-<backspace>") #'sthenno/delete-current-line)
(global-set-key (kbd "C-<backspace>") #'sthenno/delete-to-beginning-of-line)

(defun sthenno/open-newline-below ()
  "Open a new line below the current one and move the cursor to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "s-<return>") #'sthenno/open-newline-below)

;; To move between balanced expressions more efficiently
(keymap-set prog-mode-map "s-<down>" #'forward-list)
(keymap-set prog-mode-map "s-<up>"   #'backward-list)

(keymap-global-set "M-<down>" #'forward-paragraph)
(keymap-global-set "M-<up>"   #'backward-paragraph)


;; Pretty-print
;;
;; XXX: Pretty-print comes from various mechanisms respecting to the current programming
;; language. For example, formatters like Black are preferred over `indent-region' and
;; `untabify' for formatting Python code since a formatter usually provides a full
;; combination of executions that covers the basic functions that Emacs provides. Thus,
;; behaviors of `sthenno/pretty-print-current-buffer' should be considered separately
;; for each particular case especially when `sthenno/enable-pretty-print-on-save' is
;; demanded.
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
      (indent-region (point-min) (point-max) nil))))

(defun indent-current-buffer-comment ()
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
    (untabify (point-min) (point-max))))

(defun sthenno/pretty-print-current-buffer ()
  "Pretty-print current buffer."
  (interactive)
  (save-excursion
    (indent-current-buffer)
    (indent-current-buffer-comment)
    (untabify-current-buffer))
  (run-hooks 'sthenno/pretty-print-current-buffer-hook))

(keymap-set emacs-lisp-mode-map "s-p" #'sthenno/pretty-print-current-buffer)

(defun sthenno/enable-pretty-print-auto ()
  "Enable pretty-print before saving in `emacs-lisp-mode'."
  (add-hook 'before-save-hook #'sthenno/pretty-print-current-buffer nil t))
(add-hook 'emacs-lisp-mode-hook #'sthenno/enable-pretty-print-auto)

;; Inhibit passing these delimiters
(defun sthenno/inhibit-specific-delimiters ()
  "Remove the following from current `syntax-table'. This disables syntax highlighting
and auto-paring for such entries."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))
(add-hook 'org-mode-hook #'sthenno/inhibit-specific-delimiters)

;; Automatic pairing parenthesis
;;
;; `python-mode' disables `electric-indent-mode' by default since Python does not
;; lend itself to fully automatic indentation. Org Mode should disable this for the
;; same reason.
;; XXX: Side-effects come to `org-babel' is under discovering.
;;

(electric-pair-mode 1)
(setq electric-pair-pairs '((?\" . ?\")
                            (?\{ . ?\})))

;; Show parenthesis
(setopt show-paren-delay 0.05
        show-paren-style 'mixed)

;; Cursor faces
(setopt cursor-type '(bar . 1))

(setopt mouse-highlight nil)
(blink-cursor-mode -1)


;; Using rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'(lambda ()
                                      (rainbow-delimiters-mode 1))))


;; Show doc-string in echo area
(use-package eldoc
  :init (setq eldoc-idle-delay 0.05))


;; Deletions
;;
;; Delete selection if you insert
;;
(delete-selection-mode 1)

;; Delete all tabs and spaces
(setq backward-delete-char-untabify-method 'hungry)

;; Automatically reload files was modified by external program
(global-auto-revert-mode 1)


;; Fill columns
;;
;; Face `fill-column-indicator' is set in `init-gui-frames'
;;
;; (global-display-fill-column-indicator-mode 1)
(add-hook 'prog-mode-hook #'(lambda ()
                              (display-fill-column-indicator-mode 1)))

;; Display line numbers
(setq-default display-line-numbers-width 4)
(add-hook 'prog-mode-hook #'(lambda ()
                              (display-line-numbers-mode 1)))


(use-package pulsar
  :ensure t
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.05
        pulsar-iterations 15)

  ;; Hooks
  ;;
  ;; Pulsing
  (add-hook 'after-save-hook #'pulsar-pulse-line-green)

  (add-hook 'sthenno/delete-current-line-hook #'pulsar-pulse-line-magenta)
  (add-hook 'sthenno/cycle-to-next-buffer-hook #'pulsar-pulse-line-cyan)
  (add-hook 'sthenno/cycle-to-previous-buffer-hook #'pulsar-pulse-line-cyan)

  (add-hook 'sthenno/org-copy-source-code-block-hook #'pulsar-pulse-line-cyan)

  ;; Highlighting
  (add-hook 'split-window-below-focus-hook #'pulsar-highlight-line)
  (add-hook 'split-window-right-focus-hook #'pulsar-highlight-line)

  (add-hook 'after-init-hook #'(lambda ()
                                 (pulsar-global-mode 1))))


(use-package indent-bars
  :ensure t
  :init
  (setq indent-bars-treesit-support t
        indent-bars-treesit-ignore-blank-lines-types '("module"))

  ;; Stipple-based pixel-toggling is not supported by NS built Emacs
  (setq indent-bars-prefer-character t)

  :config
  (setq indent-bars-color '(highlight :face-bg t :blend 0.4)
        indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
        indent-bars-highlight-current-depth '(:blend 0.8)
        indent-bars-starting-column 0
        indent-bars-display-on-blank-lines t)

  ;; Hooks
  (add-hook 'python-mode-hook #'(lambda ()
                                  (indent-bars-mode 1))))


;; Highlight these keywords in code comments
;;
(use-package hl-todo
  :ensure t
  :config
  (defun sthenno/hl-todo-faces-setup ()
    (modus-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("TODO"  . ,prose-todo)
              ("FIXME" . ,err)
              ("XXXX*" . ,err)
              ("NOTE"  . ,fg-changed)
              ("HACK"  . ,fg-changed))))

    (global-hl-todo-mode 1))

  (add-hook 'prog-mode-hook #'sthenno/hl-todo-faces-setup))


;; Edit multiple occurrences in the same way simultaneously
(use-package iedit
  :ensure t)


;;; Spell checking using Jinx
;;
;; GNU Aspell is used as the backend by default.
;; Enchant need to be installed first. Use "brew install enchant" on macOS.
;; Personal dictionary is located at "~/.config/enchant/en_US.dic" on macOS.

(use-package jinx
  :ensure t
  :init
  (setq jinx-languages "en_US"
        jinx-menu-suggestions 5)

  (setq jinx--save-keys
        `((?= . ,#'jinx--save-personal)))

  ;; HACK
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
                                         (when (or (< (point) (window-start))
                                                   (> (point) (window-end nil t)))
                                           (recenter))
                                         (minibuffer-with-setup-hook
                                             #'jinx--correct-setup
                                           (or (completing-read
                                                (format "Correct '%s'%s: " word
                                                        (or info ""))
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
                               (concat (propertize (if (stringp k) k
                                                     (char-to-string k))
                                                   'face 'jinx-save 'rear-nonsticky t)
                                       w)
                               'jinx--group "Accept"
                               'jinx--suffix
                               (format #(" [%s]" 0 5 (face jinx-annotation)) a))
                              list)))
      (nreverse list)))

  ;; Hooks
  ;;
  ;; (add-hook 'emacs-startup-hook #'(lambda ()
  ;;                                   (global-jinx-mode 1)))
  ;; (add-hook 'text-mode-hook #'(lambda ()
  ;;                               (jinx-mode 1)))

  :bind (:map jinx-overlay-map
              ("C-SPC" . jinx-correct)))

(provide 'init-editing-utils)
