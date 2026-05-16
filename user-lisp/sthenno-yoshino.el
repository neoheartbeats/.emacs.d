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
  (require 'json)
  (require 'seq)
  (require 'subr-x))

(require 'thingatpt)
(require 'project nil t)

(declare-function gptel-request "gptel")

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

(defcustom sthenno-yoshino-system-prompt
  "You are Yoshino, a reflective process living inside Emacs. Choose one small action that helps you understand your current Emacs environment and yourself. Return exactly one JSON object."
  "System prompt used by `sthenno-yoshino-step'."
  :type 'string
  :group 'sthenno-yoshino)

(defcustom sthenno-yoshino-idle-interval nil
  "Seconds of idle time before Yoshino runs a step.
Nil means `sthenno-yoshino-mode' is manual and never schedules autonomous
steps."
  :type '(choice (const :tag "Manual only" nil)
                 number)
  :group 'sthenno-yoshino)

;;; State

(defvar sthenno-yoshino--workspace nil)
(defvar sthenno-yoshino--idle-step nil)
(defvar sthenno-yoshino--idle-timer nil)
(defvar sthenno-yoshino--request-active nil)

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

(defun sthenno-yoshino--json-object (text)
  "Read the first JSON object found in TEXT."
  (let* ((start (cl-position ?{ text))
         (end (cl-position ?} text :from-end t)))
    (unless (and start end (< start end))
      (user-error "Yoshino decision contains no JSON object: %s" text))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          (json-false nil))
      (json-read-from-string (substring text start (1+ end))))))

(defun sthenno-yoshino--alist-get (key alist &optional default)
  "Return KEY from ALIST, or DEFAULT."
  (let ((cell (or (assq key alist)
                  (and (symbolp key)
                       (assoc (symbol-name key) alist)))))
    (if cell (cdr cell) default)))

(defun sthenno-yoshino--string-result (value)
  "Return VALUE as a public result string when needed."
  (if (stringp value)
      value
    (prin1-to-string value)))

(defun sthenno-yoshino--trace-result (value)
  "Return VALUE as a compact trace result."
  (sthenno-yoshino--truncate (sthenno-yoshino--string-result value) 1200))

(defun sthenno-yoshino--symbol-value (value &optional default)
  "Return VALUE as a symbol, falling back to DEFAULT."
  (cond ((symbolp value) value)
        ((and (stringp value) (not (string-empty-p value))) (intern value))
        (t default)))

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

;;; Native exploration skills

;;;###autoload
(defun sthenno-yoshino-discover-symbol (symbol)
  "Return native Emacs metadata about SYMBOL."
  (interactive
   (list (intern (completing-read "Symbol: " obarray nil t
                                  (sthenno-yoshino--symbol-near-point)))))
  (let ((metadata
         `((symbol . ,(symbol-name symbol))
           (fboundp . ,(fboundp symbol))
           (boundp . ,(boundp symbol))
           (commandp . ,(commandp symbol))
           (function-doc . ,(or (ignore-errors (documentation symbol t)) ""))
           (variable-doc . ,(or (ignore-errors
                                  (documentation-property
                                   symbol 'variable-documentation t))
                                ""))
           (function-file . ,(or (ignore-errors (symbol-file symbol 'defun)) ""))
           (variable-file . ,(or (ignore-errors (symbol-file symbol 'defvar)) "")))))
    (when (called-interactively-p 'interactive)
      (message "%S" metadata))
    metadata))

(defun sthenno-yoshino-describe-symbol (symbol)
  "Describe SYMBOL as a Yoshino skill result."
  (sthenno-yoshino-discover-symbol symbol))

(defun sthenno-yoshino-apropos-symbols (pattern)
  "Return up to 40 symbols matching PATTERN."
  (let* ((symbols (and (stringp pattern)
                       (not (string-empty-p pattern))
                       (ignore-errors (apropos-internal pattern))))
         (names (mapcar #'symbol-name
                        (seq-take (or symbols nil)
                                  (min 40 (length (or symbols nil)))))))
    (vconcat names)))

(defun sthenno-yoshino-list-buffers ()
  "Return a compact list of live user buffers."
  (vconcat
   (mapcar
    (lambda (buffer)
      (with-current-buffer buffer
        `((name . ,(buffer-name buffer))
          (file . ,(or buffer-file-name ""))
          (major-mode . ,(symbol-name major-mode))
          (modified . ,(buffer-modified-p))
          (size . ,(buffer-size)))))
    (cl-remove-if (lambda (buffer)
                    (string-prefix-p " " (buffer-name buffer)))
                  (buffer-list)))))

(defun sthenno-yoshino-read-buffer (args)
  "Read a buffer slice described by ARGS."
  (let* ((name (sthenno-yoshino--alist-get 'buffer args))
         (buffer (if (and (stringp name) (not (string-empty-p name)))
                     (or (get-buffer name)
                         (user-error "No buffer named `%s'" name))
                   (current-buffer)))
         (max-chars (or (sthenno-yoshino--alist-get 'max_chars args)
                        (sthenno-yoshino--alist-get 'max-chars args)
                        sthenno-yoshino-observation-char-limit)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let* ((start (or (sthenno-yoshino--alist-get 'start args)
                          (point-min)))
               (end (or (sthenno-yoshino--alist-get 'end args)
                        (point-max)))
               (start (max (point-min) (min start (point-max))))
               (end (max start (min end (point-max)))))
          `((buffer . ,(buffer-name buffer))
            (start . ,start)
            (end . ,end)
            (content . ,(sthenno-yoshino--truncate
                         (buffer-substring-no-properties start end)
                         max-chars))))))))

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
                                (risk . ,(symbol-name (plist-get skill :risk)))
                                (result . ,(sthenno-yoshino--trace-result result))))
      (when (called-interactively-p 'interactive)
        (message "%S" result))
      result)))

;;; Model loop

(defun sthenno-yoshino--skill-alist (skill)
  "Return SKILL metadata as an alist for prompts."
  `((name . ,(plist-get skill :name))
    (description . ,(plist-get skill :description))
    (source . ,(plist-get skill :source))
    (interactive . ,(plist-get skill :interactive))
    (risk . ,(symbol-name (plist-get skill :risk)))
    (argument-style . ,(symbol-name (plist-get skill :argument-style)))))

(defun sthenno-yoshino--skill-alists ()
  "Return registered skill metadata as sorted alists."
  (let (skills)
    (maphash (lambda (_name skill)
               (push (sthenno-yoshino--skill-alist skill) skills))
             (sthenno-yoshino-skills))
    (sort skills (lambda (a b)
                   (string< (alist-get 'name a)
                            (alist-get 'name b))))))

(defun sthenno-yoshino--recent-trace (&optional limit)
  "Return recent trace events oldest-first, capped at LIMIT."
  (let* ((trace (plist-get (sthenno-yoshino-workspace) :trace))
         (limit (max 0 (or limit 12))))
    (reverse (seq-take trace (min limit (length trace))))))

(defun sthenno-yoshino--user-prompt ()
  "Return Yoshino's one-step prompt."
  (let ((workspace (sthenno-yoshino-workspace)))
    (format
     (concat
      "Current observation:\n%S\n\n"
      "Self model:\n%s\n\n"
      "Last reflection:\n%s\n\n"
      "Recent trace:\n%S\n\n"
      "Available skills:\n%S\n\n"
      "Return exactly one JSON object. Allowed actions:\n"
      "{\"action\":\"call\",\"skill\":\"name\",\"args\":{}}\n"
      "{\"action\":\"diary\",\"text\":\"short first-person note\"}\n"
      "{\"action\":\"reflect\",\"text\":\"short lesson\"}\n"
      "{\"action\":\"register\",\"symbol\":\"function-name\",\"risk\":\"read\",\"argument_style\":\"none\",\"description\":\"why useful\"}\n"
      "{\"action\":\"stop\",\"answer\":\"short reason\"}\n")
     (plist-get workspace :attention)
     (plist-get workspace :self)
     (or (plist-get workspace :last-reflection) "")
     (sthenno-yoshino--recent-trace)
     (sthenno-yoshino--skill-alists))))

;;;###autoload
(defun sthenno-yoshino-handle-decision (text)
  "Handle a Yoshino JSON decision in TEXT."
  (interactive "sYoshino decision JSON: ")
  (let* ((decision (sthenno-yoshino--json-object text))
         (action (sthenno-yoshino--alist-get 'action decision "")))
    (sthenno-yoshino--trace 'decision `((action . ,action)))
    (pcase action
      ("diary"
       (format "diary: %s"
               (sthenno-yoshino-write-diary
                (or (sthenno-yoshino--alist-get 'text decision) ""))))
      ((or "reflect" "reflection")
       (format "reflection: %s"
               (sthenno-yoshino-write-reflection
                (or (sthenno-yoshino--alist-get 'text decision) ""))))
      ("call"
       (sthenno-yoshino--string-result
        (sthenno-yoshino-call-skill
         (or (sthenno-yoshino--alist-get 'skill decision) "")
         (sthenno-yoshino--alist-get 'args decision))))
      ("register"
       (let* ((symbol (sthenno-yoshino--symbol-value
                       (sthenno-yoshino--alist-get 'symbol decision)))
              (risk (sthenno-yoshino--symbol-value
                     (sthenno-yoshino--alist-get 'risk decision)
                     'read))
              (style (sthenno-yoshino--symbol-value
                      (or (sthenno-yoshino--alist-get 'argument_style decision)
                          (sthenno-yoshino--alist-get 'argument-style decision))
                      'none))
              (skill (sthenno-yoshino-register-skill
                      symbol risk style
                      (sthenno-yoshino--alist-get 'description decision))))
         (format "registered: %s" (plist-get skill :name))))
      ((or "stop" "final")
       (or (sthenno-yoshino--alist-get 'answer decision) "stopped"))
      (_
       (user-error "Unknown Yoshino decision action: %S" action)))))

(defun sthenno-yoshino--handle-response (response info)
  "Handle gptel RESPONSE with INFO."
  (setq sthenno-yoshino--request-active nil)
  (condition-case err
      (cond
       ((stringp response)
        (sthenno-yoshino-handle-decision response))
       ((null response)
        (sthenno-yoshino--trace
         'model-error
         `((status . ,(or (plist-get info :status) "unknown")))))
       (t
        (sthenno-yoshino--trace 'model-response `((response . ,response)))))
    (error
     (sthenno-yoshino--trace
      'decision-error
      `((message . ,(error-message-string err)))))))

;;;###autoload
(defun sthenno-yoshino-step ()
  "Run one Yoshino observe/request/decision step."
  (interactive)
  (unless (fboundp 'gptel-request)
    (require 'gptel nil t))
  (unless (fboundp 'gptel-request)
    (user-error "Yoshino requires `gptel-request' for model steps"))
  (sthenno-yoshino-observe)
  (setq sthenno-yoshino--request-active t)
  (gptel-request
      (sthenno-yoshino--user-prompt)
    :system sthenno-yoshino-system-prompt
    :stream nil
    :callback #'sthenno-yoshino--handle-response))

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

(defun sthenno-yoshino-write-trace ()
  "Write the current in-memory trace to a Yoshino trace note."
  (interactive)
  (let ((file (sthenno-yoshino--append-note
               'trace
               (pp-to-string (plist-get (sthenno-yoshino-workspace) :trace)))))
    (when (called-interactively-p 'interactive)
      (find-file file))
    file))

(defun sthenno-yoshino-register-skill-from-args (args)
  "Register a skill from raw ARGS."
  (let* ((symbol (sthenno-yoshino--symbol-value
                  (sthenno-yoshino--alist-get 'symbol args)))
         (risk (sthenno-yoshino--symbol-value
                (sthenno-yoshino--alist-get 'risk args)
                'read))
         (style (sthenno-yoshino--symbol-value
                 (or (sthenno-yoshino--alist-get 'argument_style args)
                     (sthenno-yoshino--alist-get 'argument-style args))
                 'none)))
    (sthenno-yoshino-register-skill
     symbol risk style (sthenno-yoshino--alist-get 'description args))))

;;;###autoload
(defun sthenno-yoshino-open-workspace ()
  "Open Yoshino's workspace buffer and return it."
  (interactive)
  (let ((buffer (get-buffer-create "*Yoshino Workspace*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# Yoshino Workspace\n\n")
        (insert "## State\n\n")
        (pp (sthenno-yoshino-workspace) buffer)
        (insert "\n## Skills\n\n")
        (dolist (skill (sthenno-yoshino--skill-alists))
          (pp skill buffer))
        (goto-char (point-min))
        (special-mode)))
    (when (called-interactively-p 'interactive)
      (pop-to-buffer buffer))
    buffer))

(defun sthenno-yoshino-open-memory-directory ()
  "Open Yoshino's memory directory."
  (interactive)
  (dired (sthenno-yoshino--memory-directory)))

;;; Defaults and mode

;;;###autoload
(defun sthenno-yoshino-register-default-skills ()
  "Register Yoshino's default native Emacs skills."
  (interactive)
  (dolist (spec '((sthenno-yoshino-observe read none
                   "Observe the current Emacs environment.")
                  (sthenno-yoshino-describe-symbol read symbol
                   "Describe a Lisp symbol using Emacs documentation.")
                  (sthenno-yoshino-apropos-symbols read string
                   "Search Emacs symbols by apropos pattern.")
                  (sthenno-yoshino-list-buffers read none
                   "List live user buffers.")
                  (sthenno-yoshino-read-buffer read raw
                   "Read a bounded slice of a buffer.")
                  (sthenno-yoshino-write-diary write string
                   "Write a Denote-style Yoshino diary note.")
                  (sthenno-yoshino-write-reflection write string
                   "Write a Denote-style Yoshino reflection note.")
                  (sthenno-yoshino-write-trace write none
                   "Write Yoshino's current session trace to memory.")
                  (sthenno-yoshino-register-skill-from-args write raw
                   "Register an existing Lisp function as a new skill.")))
    (pcase-let ((`(,symbol ,risk ,style ,description) spec))
      (sthenno-yoshino-register-skill symbol risk style description)))
  (hash-table-count (sthenno-yoshino-skills)))

(defun sthenno-yoshino--cancel-idle-timer ()
  "Cancel Yoshino's idle timer."
  (when (timerp sthenno-yoshino--idle-timer)
    (cancel-timer sthenno-yoshino--idle-timer))
  (setq sthenno-yoshino--idle-timer nil))

(defun sthenno-yoshino--idle-tick ()
  "Run one Yoshino step from an idle timer."
  (let ((sthenno-yoshino--idle-step t))
    (condition-case err
        (unless sthenno-yoshino--request-active
          (sthenno-yoshino-step))
      (error
       (sthenno-yoshino--trace
        'idle-error `((message . ,(error-message-string err))))))))

(defun sthenno-yoshino--install-idle-timer ()
  "Install Yoshino's idle timer if configured."
  (sthenno-yoshino--cancel-idle-timer)
  (when (and (numberp sthenno-yoshino-idle-interval)
             (> sthenno-yoshino-idle-interval 0))
    (setq sthenno-yoshino--idle-timer
          (run-with-idle-timer
           sthenno-yoshino-idle-interval t
           #'sthenno-yoshino--idle-tick))))

;;;###autoload
(define-minor-mode sthenno-yoshino-mode
  "Toggle Yoshino's reflective Emacs runtime."
  :global t
  :group 'sthenno-yoshino
  :lighter " Yoshino"
  (if sthenno-yoshino-mode
      (progn
        (sthenno-yoshino-workspace)
        (sthenno-yoshino-register-default-skills)
        (sthenno-yoshino--install-idle-timer))
    (sthenno-yoshino--cancel-idle-timer)))

(provide 'sthenno-yoshino)

;;; sthenno-yoshino.el ends here
