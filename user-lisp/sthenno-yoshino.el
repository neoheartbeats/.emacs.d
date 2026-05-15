;;; sthenno-yoshino.el --- Reflective Emacs-native life loop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Yoshino is a small Emacs-resident reflective runtime.  It observes the
;; current editor state, treats selected Lisp functions as skills, and keeps a
;; compact workspace for later reflection.

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'thingatpt)
(require 'project nil t)

;;; Options

(defgroup sthenno-yoshino nil
  "A reflective Emacs-native life loop."
  :group 'applications)

(defcustom sthenno-yoshino-observation-char-limit 2400
  "Maximum number of characters captured from the current buffer."
  :type 'integer
  :group 'sthenno-yoshino)

(defcustom sthenno-yoshino-trace-limit 80
  "Maximum number of recent in-memory trace events retained."
  :type 'integer
  :group 'sthenno-yoshino)

(defcustom sthenno-yoshino-confirm-write-actions t
  "Non-nil means Yoshino asks before running `write' skills."
  :type 'boolean
  :group 'sthenno-yoshino)

(defcustom sthenno-yoshino-confirm-danger-actions t
  "Non-nil means Yoshino asks before running `danger' skills."
  :type 'boolean
  :group 'sthenno-yoshino)

(defcustom sthenno-yoshino-memory-keyword "yoshino"
  "Denote keyword used for Yoshino memory notes."
  :type 'string
  :group 'sthenno-yoshino)

(defcustom sthenno-yoshino-denote-directory
  (cond ((boundp 'denote-directory) denote-directory)
        ((boundp 'org-directory) org-directory)
        (t user-emacs-directory))
  "Directory where Yoshino writes Denote-style memory notes."
  :type 'directory
  :group 'sthenno-yoshino)

;;; State

(defvar sthenno-yoshino--workspace nil)
(defvar sthenno-yoshino--idle-step nil)

(defun sthenno-yoshino--fresh-workspace ()
  "Return a new empty Yoshino workspace."
  (list :attention nil
        :skills (make-hash-table :test #'equal)
        :trace nil
        :self "I am Yoshino, an Emacs Lisp process learning what I can observe and do."
        :last-reflection nil))

(defun sthenno-yoshino-workspace ()
  "Return Yoshino's current workspace, creating it when needed."
  (or sthenno-yoshino--workspace
      (setq sthenno-yoshino--workspace (sthenno-yoshino--fresh-workspace))))

(defun sthenno-yoshino-reset-workspace ()
  "Reset Yoshino's in-memory workspace."
  (interactive)
  (setq sthenno-yoshino--workspace (sthenno-yoshino--fresh-workspace)))

(defun sthenno-yoshino-skills ()
  "Return Yoshino's skill registry."
  (plist-get (sthenno-yoshino-workspace) :skills))

(defun sthenno-yoshino--workspace-put (key value)
  "Set workspace KEY to VALUE."
  (setq sthenno-yoshino--workspace
        (plist-put (sthenno-yoshino-workspace) key value))
  value)

(defun sthenno-yoshino--trace (kind payload)
  "Record a session trace event of KIND with PAYLOAD."
  (let* ((event `((time . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                  (kind . ,(format "%s" kind))
                  (payload . ,payload)))
         (trace (cons event (plist-get (sthenno-yoshino-workspace) :trace)))
         (limit (max 1 (or sthenno-yoshino-trace-limit 80))))
    (sthenno-yoshino--workspace-put :trace
                                    (cl-subseq trace 0 (min limit (length trace))))
    event))

;;; Utility

(defun sthenno-yoshino--truncate (text &optional limit)
  "Return TEXT truncated to LIMIT characters."
  (let ((limit (max 1 (or limit sthenno-yoshino-observation-char-limit))))
    (if (and (stringp text) (> (length text) limit))
        (concat (substring text 0 limit) "\n...[truncated]...")
      (or text ""))))

(defun sthenno-yoshino--symbol-near-point ()
  "Return a symbol at or near point as a string."
  (or (thing-at-point 'symbol t)
      (save-excursion
        (let ((end (line-end-position)))
          (when (re-search-forward "\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>" end t)
            (match-string-no-properties 1))))))

(defun sthenno-yoshino--project-root ()
  "Return the current project root, or nil."
  (when (and (featurep 'project)
             (fboundp 'project-current))
    (when-let* ((project (project-current nil)))
      (ignore-errors (project-root project)))))

(defun sthenno-yoshino--buffer-snippet ()
  "Return a bounded snippet around point in the current buffer."
  (save-restriction
    (widen)
    (let* ((limit (max 1 (or sthenno-yoshino-observation-char-limit 2400)))
           (half (/ limit 2))
           (beg (if (use-region-p)
                    (region-beginning)
                  (max (point-min) (- (point) half))))
           (end (if (use-region-p)
                    (region-end)
                  (min (point-max) (+ (point) half)))))
      (sthenno-yoshino--truncate
       (buffer-substring-no-properties beg end)
       limit))))

(defun sthenno-yoshino--visible-buffer-names ()
  "Return names of buffers visible in live windows."
  (delete-dups
   (mapcar (lambda (window)
             (buffer-name (window-buffer window)))
           (window-list nil 'no-minibuf))))

(defun sthenno-yoshino--slug (text)
  "Return a Denote-compatible slug for TEXT."
  (cond
   ((and (require 'denote nil t)
         (fboundp 'denote-sluggify-title))
    (denote-sluggify-title text))
   (t
    (let ((slug (downcase (string-trim (or text "")))))
      (setq slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
      (setq slug (replace-regexp-in-string "\\`-\\|[-]+\\'" "" slug))
      (if (string-empty-p slug) "note" slug)))))

(defun sthenno-yoshino--memory-directory ()
  "Return the directory used for Yoshino memory."
  (file-name-as-directory
   (expand-file-name
    (or sthenno-yoshino-denote-directory user-emacs-directory))))

(defun sthenno-yoshino--format-note-file (dir id title keyword &optional counter)
  "Return a Denote-style note file in DIR."
  (let ((signature (if counter (number-to-string counter) "")))
    (cond
     ((and (require 'denote nil t)
           (fboundp 'denote-format-file-name))
      (denote-format-file-name dir id (list keyword) title ".org" signature))
     (t
      (expand-file-name
       (format "%s%s--%s__%s.org"
               id
               (if counter (format "==%s" counter) "")
               (sthenno-yoshino--slug title)
               (sthenno-yoshino--slug keyword))
       dir)))))

(defun sthenno-yoshino--note-file (title)
  "Return a new Denote-style note path for TITLE."
  (let* ((dir (sthenno-yoshino--memory-directory))
         (id (format-time-string "%Y%m%dT%H%M%S"))
         (keyword sthenno-yoshino-memory-keyword)
         (counter nil)
         (file (sthenno-yoshino--format-note-file dir id title keyword)))
    (while (file-exists-p file)
      (setq counter (1+ (or counter 0))
            file (sthenno-yoshino--format-note-file dir id title keyword counter)))
    file))

(defun sthenno-yoshino--append-note (kind text)
  "Write Yoshino memory note of KIND containing TEXT and return its file."
  (let* ((title (format "yoshino %s" kind))
         (file (sthenno-yoshino--note-file title)))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (insert (format "#+TITLE: %s\n\n" title))
      (insert (format "* %s\n" (format-time-string "%Y-%m-%d %H:%M:%S %z")))
      (insert (string-trim (or text "")) "\n")
      (write-region (point-min) (point-max) file nil 'silent))
    (sthenno-yoshino--trace kind `((file . ,file)))
    file))

;;; Observer

;;;###autoload
(defun sthenno-yoshino-observe ()
  "Observe the current Emacs environment and update Yoshino's attention."
  (interactive)
  (let ((observation
         `((time . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
           (buffer . ,(buffer-name))
           (file . ,(or buffer-file-name ""))
           (major-mode . ,(symbol-name major-mode))
           (modified . ,(buffer-modified-p))
           (read-only . ,buffer-read-only)
           (point . ,(point))
           (line . ,(line-number-at-pos))
           (column . ,(current-column))
           (region-active . ,(use-region-p))
           (symbol-at-point . ,(or (sthenno-yoshino--symbol-near-point) ""))
           (visible-buffers . ,(vconcat (sthenno-yoshino--visible-buffer-names)))
           (recent-command . ,(and (boundp 'real-last-command)
                                   real-last-command
                                   (symbol-name real-last-command)))
           (this-command . ,(and this-command (symbol-name this-command)))
           (project-root . ,(or (sthenno-yoshino--project-root) ""))
           (snippet . ,(sthenno-yoshino--buffer-snippet)))))
    (sthenno-yoshino--workspace-put :attention observation)
    (sthenno-yoshino--trace 'observe
                            `((buffer . ,(alist-get 'buffer observation))
                              (major-mode . ,(alist-get 'major-mode observation))))
    (when (called-interactively-p 'interactive)
      (message "%S" observation))
    observation))

;;; Skill registry

(defconst sthenno-yoshino--risk-levels '(read write danger))
(defconst sthenno-yoshino--argument-styles '(none string symbol raw))

(defun sthenno-yoshino--doc-summary (symbol)
  "Return a compact documentation summary for SYMBOL."
  (or (when-let* ((doc (documentation symbol t)))
        (car (split-string (string-trim doc) "\n" t)))
      ""))

(defun sthenno-yoshino--skill-metadata (symbol risk argument-style description)
  "Return skill metadata for SYMBOL."
  (list :name (symbol-name symbol)
        :symbol symbol
        :description (or description (sthenno-yoshino--doc-summary symbol))
        :source (or (ignore-errors (symbol-file symbol 'defun)) "")
        :interactive (commandp symbol)
        :risk risk
        :argument-style argument-style))

;;;###autoload
(defun sthenno-yoshino-register-skill (symbol risk argument-style &optional description)
  "Register SYMBOL as a skill with RISK and ARGUMENT-STYLE.
RISK is one of `read', `write', or `danger'.  ARGUMENT-STYLE is one of
`none', `string', `symbol', or `raw'."
  (interactive
   (list (intern (completing-read "Function: " obarray #'fboundp t))
         (intern (completing-read "Risk: " '("read" "write" "danger")
                                  nil t nil nil "read"))
         (intern (completing-read "Argument style: " '("none" "string" "symbol" "raw")
                                  nil t nil nil "none"))))
  (unless (fboundp symbol)
    (user-error "No function named `%s'" symbol))
  (unless (memq risk sthenno-yoshino--risk-levels)
    (user-error "Invalid Yoshino skill risk: %S" risk))
  (unless (memq argument-style sthenno-yoshino--argument-styles)
    (user-error "Invalid Yoshino skill argument style: %S" argument-style))
  (let ((skill (sthenno-yoshino--skill-metadata
                symbol risk argument-style description)))
    (puthash (plist-get skill :name) skill (sthenno-yoshino-skills))
    (sthenno-yoshino--trace 'register-skill
                            `((name . ,(plist-get skill :name))
                              (risk . ,(symbol-name risk))
                              (argument-style . ,(symbol-name argument-style))))
    (when (called-interactively-p 'interactive)
      (message "Registered Yoshino skill: %s" (plist-get skill :name)))
    skill))

(defun sthenno-yoshino--skill-argument (style args)
  "Return a Lisp argument for STYLE from ARGS."
  (pcase style
    ('none nil)
    ('string (cond ((stringp args) args)
                   ((and (listp args) (alist-get 'text args)) (alist-get 'text args))
                   ((and (listp args) (alist-get 'query args)) (alist-get 'query args))
                   ((and (listp args) (alist-get 'value args)) (alist-get 'value args))
                   (t (format "%s" args))))
    ('symbol (cond ((symbolp args) args)
                   ((stringp args) (intern args))
                   ((and (listp args) (alist-get 'symbol args))
                    (intern (alist-get 'symbol args)))
                   ((and (listp args) (alist-get 'name args))
                    (intern (alist-get 'name args)))
                   (t (user-error "Cannot derive symbol argument from %S" args))))
    ('raw args)
    (_ (user-error "Unknown Yoshino argument style: %S" style))))

(defun sthenno-yoshino--confirm-skill (skill)
  "Ask for confirmation if SKILL requires it."
  (let ((risk (plist-get skill :risk))
        (name (plist-get skill :name)))
    (pcase risk
      ('read t)
      ('write
       (when (and sthenno-yoshino-confirm-write-actions
                  (not (yes-or-no-p
                        (format "Yoshino wants to run write skill `%s'. Proceed? "
                                name))))
         (user-error "Yoshino write skill rejected: %s" name)))
      ('danger
       (when sthenno-yoshino--idle-step
         (user-error "Yoshino danger skill disabled during idle step: %s" name))
       (when (and sthenno-yoshino-confirm-danger-actions
                  (not (yes-or-no-p
                        (format "Yoshino wants to run danger skill `%s'. Proceed? "
                                name))))
         (user-error "Yoshino danger skill rejected: %s" name)))
      (_ (user-error "Unknown Yoshino skill risk: %S" risk)))))

;;;###autoload
(defun sthenno-yoshino-call-skill (name &optional args)
  "Call registered skill NAME with optional ARGS."
  (interactive
   (list (completing-read "Skill: " (hash-table-keys (sthenno-yoshino-skills))
                          nil t)
         nil))
  (let ((skill (gethash name (sthenno-yoshino-skills))))
    (unless skill
      (user-error "Unknown Yoshino skill: %s" name))
    (sthenno-yoshino--confirm-skill skill)
    (let* ((symbol (plist-get skill :symbol))
           (style (plist-get skill :argument-style))
           (argument (sthenno-yoshino--skill-argument style args))
           (result (if (eq style 'none)
                       (funcall symbol)
                     (funcall symbol argument))))
      (sthenno-yoshino--trace 'call-skill
                              `((name . ,name)
                                (risk . ,(symbol-name (plist-get skill :risk)))))
      (when (called-interactively-p 'interactive)
        (message "%S" result))
      result)))

;;; Memory

;;;###autoload
(defun sthenno-yoshino-write-diary (text)
  "Write TEXT as a Yoshino diary note and return the file path."
  (interactive "sYoshino diary: ")
  (let ((file (sthenno-yoshino--append-note 'diary text)))
    (when (called-interactively-p 'interactive)
      (find-file file))
    file))

;;;###autoload
(defun sthenno-yoshino-write-reflection (text)
  "Write TEXT as a Yoshino reflection note and return the file path."
  (interactive "sYoshino reflection: ")
  (sthenno-yoshino--workspace-put :last-reflection text)
  (let ((file (sthenno-yoshino--append-note 'reflection text)))
    (when (called-interactively-p 'interactive)
      (find-file file))
    file))

(provide 'sthenno-yoshino)

;;; sthenno-yoshino.el ends here
