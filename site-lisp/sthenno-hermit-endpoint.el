;;; ear-agent-mvp.el --- Minimal reflective Emacs LLM agent -*- lexical-binding: t; -*-

;; MVP goals:
;; - Emacs as POMDP-like environment
;; - M-x / Lisp symbols as discoverable skills
;; - observe -> plan -> act -> verify -> remember loop
;; - No third-party Emacs package required
;;
;; Usage:
;;   (load-file "/path/to/ear-agent-mvp.el")
;;   M-x ear-agent-mode
;;   M-x ear-agent-ask
;;
;; Configure an OpenAI-compatible endpoint:
;;   export EAR_AGENT_ENDPOINT="http://localhost:8000/v1/chat/completions"
;;   export EAR_AGENT_MODEL="your-model"
;;   export EAR_AGENT_API_KEY="optional-key"
;;
;; Or in Emacs:
;;   (setq ear-agent-endpoint "http://localhost:8000/v1/chat/completions")
;;   (setq ear-agent-model "your-model")
;;   (setq ear-agent-api-key "...")

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'thingatpt)
(require 'project nil t)
(require 'xref nil t)

(defgroup ear-agent nil
  "Minimal reflective LLM agent living inside Emacs."
  :group 'applications)

(defcustom ear-agent-endpoint
  (or (getenv "EAR_AGENT_ENDPOINT")
      (let ((base (getenv "OPENAI_BASE_URL")))
        (when (and base (not (string-empty-p base)))
          (concat (string-remove-suffix "/" base) "/v1/chat/completions")))
      "http://192.168.100.207:8000/v1/chat/completions")
  "OpenAI-compatible chat-completions endpoint."
  :type 'string
  :group 'ear-agent)

(defcustom ear-agent-model
  (or (getenv "EAR_AGENT_MODEL") "sthenno")
  "Model name for the OpenAI-compatible endpoint. Set EAR_AGENT_MODEL."
  :type 'string
  :group 'ear-agent)

(defcustom ear-agent-api-key
  (or (getenv "EAR_AGENT_API_KEY")
      (getenv "OPENAI_API_KEY")
      "sk-tmp")
  "API key. Empty means no Authorization header."
  :type 'string
  :group 'ear-agent)

(defcustom ear-agent-temperature 0.7
  "Sampling temperature used by the agent."
  :type 'number
  :group 'ear-agent)

(defcustom ear-agent-max-steps 8
  "Maximum observe/act loop steps per user request."
  :type 'integer
  :group 'ear-agent)

(defcustom ear-agent-context-char-limit 12000
  "Maximum characters included from current buffer observation."
  :type 'integer
  :group 'ear-agent)

(defcustom ear-agent-tool-output-char-limit 16000
  "Maximum characters returned from a single tool observation."
  :type 'integer
  :group 'ear-agent)

(defcustom ear-agent-auto-approve-readonly t
  "If non-nil, read-only skills run without confirmation."
  :type 'boolean
  :group 'ear-agent)

(defcustom ear-agent-confirm-mutating-actions t
  "If non-nil, ask before mutating skills execute."
  :type 'boolean
  :group 'ear-agent)

(defcustom ear-agent-memory-directory
  (expand-file-name "ear-agent/" user-emacs-directory)
  "Directory for episodic memory and logs."
  :type 'directory
  :group 'ear-agent)

(defcustom ear-agent-custom-skills-file
  (expand-file-name "custom-skills.el" ear-agent-memory-directory)
  "File used by install_skill for generated skill code."
  :type 'file
  :group 'ear-agent)

(defvar ear-agent-buffer-name "*EAR Agent*")
(defvar ear-agent--skills (make-hash-table :test 'equal))
(defvar ear-agent--origin-buffer nil)
(defvar ear-agent--last-messages nil)

(defun ear-agent--ensure-dir ()
  (unless (file-directory-p ear-agent-memory-directory)
    (make-directory ear-agent-memory-directory t)))

(defun ear-agent--memory-file ()
  (ear-agent--ensure-dir)
  (expand-file-name "memory.org" ear-agent-memory-directory))

(defun ear-agent--event-log-file ()
  (ear-agent--ensure-dir)
  (expand-file-name "events.jsonl" ear-agent-memory-directory))

(defun ear-agent--truncate (s &optional limit)
  (let ((n (or limit ear-agent-tool-output-char-limit)))
    (if (and (stringp s) (> (length s) n))
        (concat (substring s 0 n) "\n...[truncated]...")
      s)))

(defun ear-agent--json-bool (x)
  (if x t :json-false))

(defun ear-agent--alist-get (key alist &optional default)
  (let ((v (alist-get key alist nil nil #'string=)))
    (if v v default)))

(defun ear-agent--alist-get* (key alist &optional default)
  "Like `ear-agent--alist-get', but allows JSON false/nil defaults."
  (let ((cell (assoc key alist)))
    (if cell (cdr cell) default)))

(defun ear-agent--as-int (x &optional default)
  (cond ((integerp x) x)
        ((numberp x) (truncate x))
        ((stringp x) (or (ignore-errors (string-to-number x)) default))
        (t default)))

(defun ear-agent--as-bool (x)
  (and x (not (eq x :json-false))))

(defun ear-agent--json-encode-pretty (obj)
  (let ((json-encoding-pretty-print t))
    (json-encode obj)))

(defun ear-agent--chat-buffer ()
  (get-buffer-create ear-agent-buffer-name))

(defun ear-agent-chat ()
  "Open the EAR Agent chat buffer."
  (interactive)
  (pop-to-buffer (ear-agent--chat-buffer)))

(defun ear-agent--insert-chat (speaker text)
  (with-current-buffer (ear-agent--chat-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "\n## %s\n%s\n" speaker text)))))

(defun ear-agent--origin-buffer ()
  (or (and ear-agent--origin-buffer (get-buffer ear-agent--origin-buffer))
      (current-buffer)))

(defun ear-agent--resolve-buffer (&optional name)
  (cond
   ((and (stringp name) (not (string-empty-p name)))
    (or (get-buffer name) (error "No such buffer: %s" name)))
   (t (ear-agent--origin-buffer))))

(defun ear-agent--project-root (&optional buffer)
  (when (and (featurep 'project) (fboundp 'project-current))
    (with-current-buffer (or buffer (ear-agent--origin-buffer))
      (when-let* ((p (project-current nil)))
        (ignore-errors (project-root p))))))

(defun ear-agent--buffer-snippet (buffer &optional limit)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let* ((n (or limit ear-agent-context-char-limit))
             (half (/ n 2))
             (beg (if (use-region-p)
                      (region-beginning)
                    (max (point-min) (- (point) half))))
             (end (if (use-region-p)
                      (region-end)
                    (min (point-max) (+ (point) half)))))
        (ear-agent--truncate
         (buffer-substring-no-properties beg end)
         n)))))

(defun ear-agent--line-col (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    `(("line" . ,(line-number-at-pos))
      ("column" . ,(current-column))
      ("point" . ,(point)))))

(defun ear-agent--append-file (file text &optional append)
  (make-directory (file-name-directory file) t)
  (with-temp-buffer
    (insert text)
    (write-region (point-min) (point-max) file append 'silent)))

(defun ear-agent--log-event (kind payload)
  (ear-agent--append-file
   (ear-agent--event-log-file)
   (concat (json-encode
            `(("time" . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
              ("kind" . ,kind)
              ("payload" . ,payload)))
           "\n")
   t))

;;; Skill registry

(defun ear-agent-register-skill (name description schema readonly fn)
  "Register a skill.
NAME is a string. DESCRIPTION is shown to the model. SCHEMA is a JSON-like alist.
READONLY means the skill should not mutate Emacs/files/process state.
FN receives one argument: decoded JSON args alist."
  (puthash name
           (list :description description
                 :schema schema
                 :readonly readonly
                 :fn fn)
           ear-agent--skills))

(defun ear-agent--skill-specs ()
  (let (xs)
    (maphash
     (lambda (name skill)
       (push `(("name" . ,name)
               ("description" . ,(plist-get skill :description))
               ("readonly" . ,(ear-agent--json-bool (plist-get skill :readonly)))
               ("schema" . ,(plist-get skill :schema)))
             xs))
     ear-agent--skills)
    (vconcat (sort xs (lambda (a b)
                        (string< (cdr (assoc "name" a))
                                 (cdr (assoc "name" b))))))))

(defun ear-agent--tool-result-string (x)
  (ear-agent--truncate
   (cond ((stringp x) x)
         (t (ear-agent--json-encode-pretty x)))))

(defun ear-agent--confirm-tool (name args readonly)
  (cond
   ((and readonly ear-agent-auto-approve-readonly) t)
   ((and (not readonly) ear-agent-confirm-mutating-actions)
    (yes-or-no-p
     (format "EAR Agent wants to run mutating skill `%s' with args: %s. Proceed? "
             name (ear-agent--json-encode-pretty args))))
   (t t)))

(defun ear-agent--execute-tool (name args)
  (let ((skill (gethash name ear-agent--skills)))
    (unless skill
      (error "Unknown skill: %s" name))
    (let ((readonly (plist-get skill :readonly))
          (fn (plist-get skill :fn)))
      (unless (ear-agent--confirm-tool name args readonly)
        (error "User rejected tool: %s" name))
      (let ((result (funcall fn args)))
        (ear-agent--log-event "tool"
                              `(("name" . ,name)
                                ("args" . ,args)
                                ("readonly" . ,(ear-agent--json-bool readonly))))
        (ear-agent--tool-result-string result)))))

;;; Built-in skills

(defun ear-agent-skill-observe (_args)
  (let* ((buf (ear-agent--origin-buffer))
         (root (ear-agent--project-root buf)))
    (with-current-buffer buf
      `(("buffer" . ,(buffer-name buf))
        ("file" . ,(or buffer-file-name ""))
        ("major_mode" . ,(symbol-name major-mode))
        ("modified" . ,(ear-agent--json-bool (buffer-modified-p)))
        ("read_only" . ,(ear-agent--json-bool buffer-read-only))
        ("project_root" . ,(or root ""))
        ("position" . ,(ear-agent--line-col))
        ("symbol_at_point" . ,(or (thing-at-point 'symbol t) ""))
        ("region_active" . ,(ear-agent--json-bool (use-region-p)))
        ("snippet" . ,(ear-agent--buffer-snippet buf))))))

(defun ear-agent-skill-list-buffers (_args)
  (vconcat
   (mapcar
    (lambda (b)
      (with-current-buffer b
        `(("name" . ,(buffer-name b))
          ("major_mode" . ,(symbol-name major-mode))
          ("file" . ,(or buffer-file-name ""))
          ("modified" . ,(ear-agent--json-bool (buffer-modified-p)))
          ("size" . ,(buffer-size)))))
    (cl-remove-if (lambda (b)
                    (string-prefix-p " " (buffer-name b)))
                  (buffer-list)))))

(defun ear-agent-skill-read-buffer (args)
  (let* ((name (ear-agent--alist-get "buffer" args ""))
         (max-chars (ear-agent--as-int (ear-agent--alist-get "max_chars" args)
                                       ear-agent-context-char-limit))
         (start (ear-agent--as-int (ear-agent--alist-get "start" args) nil))
         (end (ear-agent--as-int (ear-agent--alist-get "end" args) nil))
         (buf (ear-agent--resolve-buffer name)))
    (with-current-buffer buf
      (save-restriction
        (widen)
        (let* ((beg (or start (point-min)))
               (fin (or end (point-max))))
          (setq beg (max (point-min) (min beg (point-max))))
          (setq fin (max beg (min fin (point-max))))
          `(("buffer" . ,(buffer-name buf))
            ("start" . ,beg)
            ("end" . ,fin)
            ("content" . ,(ear-agent--truncate
                           (buffer-substring-no-properties beg fin)
                           max-chars))))))))

(defun ear-agent-skill-describe-symbol (args)
  (let* ((name (or (ear-agent--alist-get "symbol" args)
                   (with-current-buffer (ear-agent--origin-buffer)
                     (thing-at-point 'symbol t))
                   ""))
         (sym (and (not (string-empty-p name)) (intern-soft name))))
    (if (not sym)
        `(("symbol" . ,name) ("found" . :json-false))
      `(("symbol" . ,(symbol-name sym))
        ("found" . t)
        ("fboundp" . ,(ear-agent--json-bool (fboundp sym)))
        ("boundp" . ,(ear-agent--json-bool (boundp sym)))
        ("function_doc" . ,(or (ignore-errors (documentation sym t)) ""))
        ("variable_doc" . ,(or (ignore-errors
                                 (documentation-property sym 'variable-documentation t))
                               ""))
        ("function_file" . ,(or (ignore-errors (symbol-file sym 'defun)) ""))
        ("variable_file" . ,(or (ignore-errors (symbol-file sym 'defvar)) ""))
        ("function_type" . ,(cond ((macrop sym) "macro")
                                  ((commandp sym) "interactive-command")
                                  ((fboundp sym) "function")
                                  (t "")))))))

(defun ear-agent-skill-apropos-symbols (args)
  (let* ((pattern (ear-agent--alist-get "pattern" args ""))
         (limit (ear-agent--as-int (ear-agent--alist-get "limit" args) 50))
         (syms (if (string-empty-p pattern)
                   nil
                 (ignore-errors (apropos-internal pattern))))
         (names (mapcar #'symbol-name (cl-subseq syms 0 (min limit (length syms))))))
    `(("pattern" . ,pattern)
      ("count" . ,(length syms))
      ("symbols" . ,(vconcat names)))))

(defun ear-agent--xref-item->alist (x)
  (let* ((summary (ignore-errors (xref-item-summary x)))
         (loc (ignore-errors (xref-item-location x)))
         (marker (ignore-errors (xref-location-marker loc)))
         (file (when (markerp marker)
                 (buffer-file-name (marker-buffer marker))))
         (line (when (markerp marker)
                 (with-current-buffer (marker-buffer marker)
                   (line-number-at-pos marker))))
         (col (when (markerp marker)
                (with-current-buffer (marker-buffer marker)
                  (save-excursion
                    (goto-char marker)
                    (current-column))))))
    `(("summary" . ,(or summary ""))
      ("file" . ,(or file ""))
      ("line" . ,(or line 0))
      ("column" . ,(or col 0)))))

(defun ear-agent-skill-xref-definitions (args)
  (let* ((identifier (or (ear-agent--alist-get "identifier" args)
                         (with-current-buffer (ear-agent--origin-buffer)
                           (thing-at-point 'symbol t))
                         ""))
         (limit (ear-agent--as-int (ear-agent--alist-get "limit" args) 20)))
    (with-current-buffer (ear-agent--origin-buffer)
      (if (or (string-empty-p identifier)
              (not (featurep 'xref))
              (not (fboundp 'xref-find-backend)))
          `(("identifier" . ,identifier)
            ("definitions" . []))
        (condition-case err
            (let* ((backend (xref-find-backend))
                   (defs (and backend (xref-backend-definitions backend identifier))))
              `(("identifier" . ,identifier)
                ("definitions" . ,(vconcat
                                   (mapcar #'ear-agent--xref-item->alist
                                           (cl-subseq defs 0 (min limit (length defs))))))))
          (error `(("identifier" . ,identifier)
                   ("error" . ,(error-message-string err))
                   ("definitions" . []))))))))

(defun ear-agent-skill-project-grep (args)
  (let* ((pattern (ear-agent--alist-get "pattern" args ""))
         (root (or (ear-agent--alist-get "root" args)
                   (ear-agent--project-root)
                   default-directory))
         (limit (ear-agent--as-int (ear-agent--alist-get "limit" args) 80)))
    (unless (executable-find "grep")
      (error "grep not found"))
    (unless (file-directory-p root)
      (error "Bad root: %s" root))
    (if (string-empty-p pattern)
        `(("root" . ,root) ("pattern" . ,pattern) ("matches" . []))
      (with-temp-buffer
        (let ((status (process-file
                       "grep" nil t nil
                       "-RIn" "--exclude-dir=.git" "--exclude-dir=node_modules"
                       "--exclude-dir=.cache" "--exclude-dir=dist" "--exclude-dir=build"
                       pattern root)))
          (let* ((raw (buffer-string))
                 (lines (split-string raw "\n" t))
                 (slice (cl-subseq lines 0 (min limit (length lines)))))
            `(("root" . ,root)
              ("pattern" . ,pattern)
              ("status" . ,(if (integerp status) status -1))
              ("total_returned" . ,(length slice))
              ("truncated" . ,(ear-agent--json-bool (> (length lines) limit)))
              ("matches" . ,(vconcat slice)))))))))

(defun ear-agent-skill-insert-text (args)
  (let* ((name (ear-agent--alist-get "buffer" args ""))
         (text (ear-agent--alist-get "text" args ""))
         (position (ear-agent--alist-get "position" args "point"))
         (buf (ear-agent--resolve-buffer name)))
    (with-current-buffer buf
      (let ((pos (if (equal position "point")
                     (point)
                   (ear-agent--as-int position (point)))))
        (goto-char (max (point-min) (min pos (point-max))))
        (insert text)
        `(("buffer" . ,(buffer-name buf))
          ("inserted_chars" . ,(length text))
          ("point" . ,(point)))))))

(defun ear-agent-skill-replace-region (args)
  (let* ((name (ear-agent--alist-get "buffer" args ""))
         (text (ear-agent--alist-get "text" args ""))
         (buf (ear-agent--resolve-buffer name)))
    (with-current-buffer buf
      (let* ((beg (ear-agent--as-int (ear-agent--alist-get "start" args)
                                     (if (use-region-p) (region-beginning) (point))))
             (end (ear-agent--as-int (ear-agent--alist-get "end" args)
                                     (if (use-region-p) (region-end) (point)))))
        (setq beg (max (point-min) (min beg (point-max))))
        (setq end (max beg (min end (point-max))))
        (delete-region beg end)
        (goto-char beg)
        (insert text)
        `(("buffer" . ,(buffer-name buf))
          ("start" . ,beg)
          ("end_before" . ,end)
          ("inserted_chars" . ,(length text)))))))

(defun ear-agent-skill-write-file (args)
  (let* ((path (expand-file-name (ear-agent--alist-get "path" args "")))
         (content (ear-agent--alist-get "content" args ""))
         (append (ear-agent--as-bool (ear-agent--alist-get* "append" args nil))))
    (unless (not (string-empty-p path))
      (error "Missing path"))
    (ear-agent--append-file path content append)
    `(("path" . ,path)
      ("append" . ,(ear-agent--json-bool append))
      ("written_chars" . ,(length content)))))

(defun ear-agent-skill-run-shell (args)
  (let* ((cmd (ear-agent--alist-get "command" args ""))
         (dir (or (ear-agent--alist-get "directory" args)
                  (ear-agent--project-root)
                  default-directory)))
    (unless (not (string-empty-p cmd))
      (error "Missing command"))
    (let ((default-directory dir))
      `(("directory" . ,default-directory)
        ("command" . ,cmd)
        ("output" . ,(ear-agent--truncate (shell-command-to-string cmd)))))))

(defun ear-agent-skill-eval-elisp (args)
  (let* ((code (ear-agent--alist-get "code" args "")))
    (unless (not (string-empty-p code))
      (error "Missing code"))
    (let ((result (eval (read code) t)))
      `(("code" . ,code)
        ("result" . ,(prin1-to-string result))))))

(defun ear-agent-skill-check-elisp-syntax (args)
  (let* ((name (ear-agent--alist-get "buffer" args ""))
         (buf (ear-agent--resolve-buffer name)))
    (with-current-buffer buf
      (condition-case err
          (save-excursion
            (goto-char (point-min))
            (let ((count 0))
              (while (< (point) (point-max))
                (read (current-buffer))
                (setq count (1+ count)))
              `(("buffer" . ,(buffer-name buf))
                ("ok" . t)
                ("forms" . ,count))))
        (end-of-file `(("buffer" . ,(buffer-name buf))
                       ("ok" . t)
                       ("note" . "Reached end-of-file while reading forms.")))
        (error `(("buffer" . ,(buffer-name buf))
                 ("ok" . :json-false)
                 ("error" . ,(error-message-string err))))))))

(defun ear-agent-skill-byte-compile-file (args)
  (let* ((file (or (ear-agent--alist-get "file" args)
                   (with-current-buffer (ear-agent--origin-buffer) buffer-file-name))))
    (unless (and file (file-exists-p file))
      (error "No readable file to byte-compile"))
    (with-temp-buffer
      (let ((byte-compile-error-on-warn nil)
            (byte-compile-log-buffer (current-buffer)))
        (condition-case err
            (let ((ok (byte-compile-file file)))
              `(("file" . ,file)
                ("ok" . ,(ear-agent--json-bool ok))
                ("log" . ,(ear-agent--truncate (buffer-string)))))
          (error `(("file" . ,file)
                   ("ok" . :json-false)
                   ("error" . ,(error-message-string err))
                   ("log" . ,(ear-agent--truncate (buffer-string))))))))))

(defun ear-agent-skill-memory-append (args)
  (let* ((title (or (ear-agent--alist-get "title" args) "Note"))
         (text (ear-agent--alist-get "text" args "")))
    (ear-agent--append-file
     (ear-agent--memory-file)
     (format "\n* %s\n:PROPERTIES:\n:CREATED: %s\n:END:\n\n%s\n"
             title (format-time-string "%Y-%m-%d %H:%M:%S") text)
     t)
    `(("file" . ,(ear-agent--memory-file))
      ("written_chars" . ,(length text)))))

(defun ear-agent-skill-memory-search (args)
  (let* ((query (ear-agent--alist-get "query" args ""))
         (limit (ear-agent--as-int (ear-agent--alist-get "limit" args) 20))
         (file (ear-agent--memory-file)))
    (if (or (string-empty-p query) (not (file-exists-p file)))
        `(("query" . ,query) ("matches" . []))
      (with-temp-buffer
        (insert-file-contents file)
        (let (matches)
          (goto-char (point-min))
          (while (and (< (length matches) limit)
                      (search-forward query nil t))
            (let* ((line (line-number-at-pos))
                   (beg (save-excursion (beginning-of-line) (point)))
                   (end (save-excursion (end-of-line) (point))))
              (push `(("line" . ,line)
                      ("text" . ,(buffer-substring-no-properties beg end)))
                    matches)))
          `(("query" . ,query)
            ("file" . ,file)
            ("matches" . ,(vconcat (nreverse matches)))))))))

(defun ear-agent-skill-install-skill (args)
  "Install a generated skill by appending trusted Elisp to custom-skills.el.
This is intentionally gated by confirmation because it loads code."
  (let* ((name (ear-agent--alist-get "name" args ""))
         (elisp (ear-agent--alist-get "elisp" args "")))
    (unless (and (not (string-empty-p name))
                 (not (string-empty-p elisp)))
      (error "Missing name or elisp"))
    (unless (string-match-p "ear-agent-register-skill" elisp)
      (error "Skill code must call ear-agent-register-skill"))
    (ear-agent--append-file
     ear-agent-custom-skills-file
     (format "\n\n;;; Generated skill: %s at %s\n%s\n"
             name (format-time-string "%Y-%m-%d %H:%M:%S") elisp)
     t)
    (load-file ear-agent-custom-skills-file)
    `(("name" . ,name)
      ("file" . ,ear-agent-custom-skills-file)
      ("loaded" . t))))

(defun ear-agent-register-default-skills ()
  "Register built-in MVP skills. Safe to call multiple times."
  (interactive)
  (clrhash ear-agent--skills)
  (ear-agent-register-skill
   "observe"
   "Observe current Emacs state: origin buffer, file, mode, point, symbol at point, project root, and nearby text."
   '(("type" . "object") ("properties" . nil))
   t #'ear-agent-skill-observe)
  (ear-agent-register-skill
   "list_buffers"
   "List live Emacs buffers with mode, file, modified flag, and size."
   '(("type" . "object") ("properties" . nil))
   t #'ear-agent-skill-list-buffers)
  (ear-agent-register-skill
   "read_buffer"
   "Read text from a buffer. Args: buffer optional, start optional, end optional, max_chars optional."
   '(("buffer" . "string optional") ("start" . "integer optional")
     ("end" . "integer optional") ("max_chars" . "integer optional"))
   t #'ear-agent-skill-read-buffer)
  (ear-agent-register-skill
   "describe_symbol"
   "Reflect on an Emacs Lisp symbol using documentation, function/variable docs, type, and source file. Args: symbol."
   '(("symbol" . "string"))
   t #'ear-agent-skill-describe-symbol)
  (ear-agent-register-skill
   "apropos_symbols"
   "Search Emacs Lisp symbol names. Args: pattern regexp, limit optional."
   '(("pattern" . "string regexp") ("limit" . "integer optional"))
   t #'ear-agent-skill-apropos-symbols)
  (ear-agent-register-skill
   "xref_definitions"
   "Use xref to find definitions for an identifier in the origin buffer context. Args: identifier, limit optional."
   '(("identifier" . "string") ("limit" . "integer optional"))
   t #'ear-agent-skill-xref-definitions)
  (ear-agent-register-skill
   "project_grep"
   "Run recursive grep in project/root and return matching lines. Args: pattern, root optional, limit optional."
   '(("pattern" . "string") ("root" . "string optional") ("limit" . "integer optional"))
   t #'ear-agent-skill-project-grep)
  (ear-agent-register-skill
   "insert_text"
   "Insert text into a buffer at point or numeric position. Args: buffer optional, position point|integer, text. MUTATING."
   '(("buffer" . "string optional") ("position" . "point or integer") ("text" . "string"))
   nil #'ear-agent-skill-insert-text)
  (ear-agent-register-skill
   "replace_region"
   "Replace region [start,end] in a buffer with text. Args: buffer optional, start, end, text. MUTATING."
   '(("buffer" . "string optional") ("start" . "integer") ("end" . "integer") ("text" . "string"))
   nil #'ear-agent-skill-replace-region)
  (ear-agent-register-skill
   "write_file"
   "Write or append content to a file. Args: path, content, append boolean optional. MUTATING."
   '(("path" . "string") ("content" . "string") ("append" . "boolean optional"))
   nil #'ear-agent-skill-write-file)
  (ear-agent-register-skill
   "run_shell"
   "Run a shell command in directory/project. Args: command, directory optional. MUTATING/RISKY."
   '(("command" . "string") ("directory" . "string optional"))
   nil #'ear-agent-skill-run-shell)
  (ear-agent-register-skill
   "eval_elisp"
   "Evaluate one Elisp expression and return printed result. Args: code. MUTATING/RISKY."
   '(("code" . "string containing one Elisp expression"))
   nil #'ear-agent-skill-eval-elisp)
  (ear-agent-register-skill
   "check_elisp_syntax"
   "Read all forms in a buffer to check simple Elisp syntax without evaluating. Args: buffer optional."
   '(("buffer" . "string optional"))
   t #'ear-agent-skill-check-elisp-syntax)
  (ear-agent-register-skill
   "byte_compile_file"
   "Byte compile an Elisp file and return compiler log. Args: file optional. May create .elc."
   '(("file" . "string optional"))
   nil #'ear-agent-skill-byte-compile-file)
  (ear-agent-register-skill
   "memory_append"
   "Append an org-mode memory note. Args: title, text. MUTATING."
   '(("title" . "string") ("text" . "string"))
   nil #'ear-agent-skill-memory-append)
  (ear-agent-register-skill
   "memory_search"
   "Search textual memory.org. Args: query, limit optional."
   '(("query" . "string") ("limit" . "integer optional"))
   t #'ear-agent-skill-memory-search)
  (ear-agent-register-skill
   "install_skill"
   "Append trusted Elisp to custom-skills.el, load it, and thereby extend the skill registry. Args: name, elisp. MUTATING/RISKY."
   '(("name" . "string") ("elisp" . "string; must call ear-agent-register-skill"))
   nil #'ear-agent-skill-install-skill)
  ;; Re-load custom generated/manual skills after clearing the registry.
  (when (file-exists-p ear-agent-custom-skills-file)
    (condition-case err
        (load-file ear-agent-custom-skills-file)
      (error (message "EAR Agent: failed to load custom skills: %s"
                      (error-message-string err))))))

;;; LLM protocol

(defun ear-agent--system-prompt ()
  (format
   (concat
    "You are EAR-Agent, a minimal reflective LLM agent living inside Emacs.\n"
    "Treat Emacs as a partially observable environment. Maintain a compact belief about the user's task, current buffer, project state, and your own skill limits.\n"
    "You may inspect the environment and call skills. Do not invent tool results.\n"
    "Prefer read-only inspection before mutation. For code edits, inspect -> propose -> edit -> verify.\n"
    "Your output MUST be exactly one JSON object and no markdown.\n\n"
    "Allowed output forms:\n"
    "1) Call a skill:\n"
    "{\"action\":\"call\",\"tool\":\"skill_name\",\"args\":{...},\"note\":\"short public reason\"}\n"
    "2) Final answer:\n"
    "{\"action\":\"final\",\"answer\":\"answer to user\",\"memory\":\"optional durable memory note\"}\n\n"
    "Available skills:\n%s\n")
   (ear-agent--json-encode-pretty (ear-agent--skill-specs))))

(defun ear-agent--msg (role content)
  `(("role" . ,role) ("content" . ,content)))

(defun ear-agent--call-llm (messages)
  (unless (and (stringp ear-agent-model) (not (string-empty-p ear-agent-model)))
    (error "Set `ear-agent-model' or EAR_AGENT_MODEL first"))
  (unless (executable-find "curl")
    (error "curl not found"))
  (let* ((payload (json-encode
                   `(("model" . ,ear-agent-model)
                     ("messages" . ,(vconcat messages))
                     ("temperature" . ,ear-agent-temperature))))
         (payload-file (make-temp-file "ear-agent-payload" nil ".json"))
         (args (append
                (list "-sS" "-X" "POST" ear-agent-endpoint
                      "-H" "Content-Type: application/json")
                (unless (string-empty-p ear-agent-api-key)
                  (list "-H" (concat "Authorization: Bearer " ear-agent-api-key)))
                (list "--data-binary" (concat "@" payload-file)))))
    (unwind-protect
        (progn
          (with-temp-file payload-file (insert payload))
          (with-temp-buffer
            (let ((status (apply #'call-process "curl" nil t nil args)))
              (unless (and (integerp status) (= status 0))
                (error "curl failed with status %s: %s" status (buffer-string))))
            (let* ((raw (buffer-string))
                   (json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'string)
                   (obj (condition-case err
                            (json-read-from-string raw)
                          (error (error "Bad JSON response: %s\nRaw: %s"
                                        (error-message-string err) raw))))
                   (choices (cdr (assoc "choices" obj)))
                   (choice (car choices))
                   (message (cdr (assoc "message" choice)))
                   (content (cdr (assoc "content" message))))
              (unless (stringp content)
                (error "No choices[0].message.content in response: %s" raw))
              content)))
      (ignore-errors (delete-file payload-file)))))

(defun ear-agent--extract-json-object (s)
  (let* ((start (string-match "{" s))
         (end (cl-position ?} s :from-end t)))
    (unless (and start end (> end start))
      (error "Model did not return a JSON object: %s" s))
    (substring s start (1+ end))))

(defun ear-agent--parse-agent-json (s)
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string (ear-agent--extract-json-object s))))

(defun ear-agent--initial-user-message (task)
  (let ((obs (ear-agent-skill-observe nil)))
    (format
     (concat
      "User task:\n%s\n\n"
      "Initial observation:\n%s\n\n"
      "Decide whether to inspect more, act, verify, or answer. Remember: output exactly one JSON object.")
     task
     (ear-agent--json-encode-pretty obs))))

(defun ear-agent--loop (task)
  (ear-agent-register-default-skills)
  (ear-agent--ensure-dir)
  (let* ((messages (list (ear-agent--msg "system" (ear-agent--system-prompt))
                         (ear-agent--msg "user" (ear-agent--initial-user-message task))))
         (answer nil)
         (step 0))
    (setq ear-agent--last-messages messages)
    (ear-agent--log-event "user_task" `(("task" . ,task)))
    (while (and (< step ear-agent-max-steps) (not answer))
      (setq step (1+ step))
      (let* ((raw (ear-agent--call-llm messages))
             (_ (ear-agent--insert-chat (format "model step %d" step) raw))
             (obj (ear-agent--parse-agent-json raw))
             (action (ear-agent--alist-get "action" obj)))
        (setq messages (append messages (list (ear-agent--msg "assistant" raw))))
        (pcase action
          ("call"
           (let* ((tool (ear-agent--alist-get "tool" obj))
                  (args (or (ear-agent--alist-get "args" obj) nil))
                  (result (condition-case err
                              (ear-agent--execute-tool tool args)
                            (error (format "TOOL_ERROR: %s" (error-message-string err))))))
             (ear-agent--insert-chat (format "tool %s" tool) result)
             (setq messages
                   (append messages
                           (list (ear-agent--msg
                                  "user"
                                  (format "Tool result for `%s` with args %s:\n%s\n\nContinue. Output exactly one JSON object."
                                          tool (ear-agent--json-encode-pretty args) result)))))))
          ("final"
           (setq answer (or (ear-agent--alist-get "answer" obj) ""))
           (let ((mem (ear-agent--alist-get "memory" obj "")))
             (when (and (stringp mem) (not (string-empty-p mem)))
               (ear-agent--append-file
                (ear-agent--memory-file)
                (format "\n* Interaction memory\n:PROPERTIES:\n:CREATED: %s\n:END:\n\n%s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S") mem)
                t))))
          (_
           (setq messages
                 (append messages
                         (list (ear-agent--msg
                                "user"
                                (format "Invalid action `%s`. Use action=call or action=final only. Output exactly one JSON object."
                                        action))))))))
      (setq ear-agent--last-messages messages))
    (unless answer
      (setq answer (format "Stopped after %d steps without final answer. Check %s for transcript."
                           ear-agent-max-steps ear-agent-buffer-name)))
    (ear-agent--insert-chat "final" answer)
    (ear-agent--log-event "final" `(("answer" . ,answer)))
    answer))

;;; Interactive entry points

(defun ear-agent-ask (task)
  "Ask EAR Agent to act on TASK using the current buffer as origin."
  (interactive "sEAR task: ")
  (let ((ear-agent--origin-buffer (buffer-name (current-buffer))))
    (ear-agent-chat)
    (ear-agent--insert-chat "user" task)
    (message "%s" (ear-agent--loop task))))

(defun ear-agent-ask-region (start end task)
  "Ask EAR Agent about the active region plus TASK."
  (interactive "r\nsEAR task for region: ")
  (let* ((text (buffer-substring-no-properties start end))
         (combined (format "%s\n\nRegion:\n%s" task text)))
    (ear-agent-ask combined)))

(defun ear-agent-observe-now ()
  "Insert a raw observation into the agent chat buffer."
  (interactive)
  (let ((ear-agent--origin-buffer (buffer-name (current-buffer))))
    (ear-agent-chat)
    (ear-agent--insert-chat "observation"
                            (ear-agent--json-encode-pretty
                             (ear-agent-skill-observe nil)))))

(defun ear-agent-reset-memory ()
  "Delete EAR Agent memory/log files after confirmation."
  (interactive)
  (when (yes-or-no-p "Delete EAR Agent memory and event log? ")
    (dolist (f (list (ear-agent--memory-file) (ear-agent--event-log-file)))
      (when (file-exists-p f) (delete-file f)))
    (message "EAR Agent memory reset.")))

(defvar ear-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e a") #'ear-agent-ask)
    (define-key map (kbd "C-c e r") #'ear-agent-ask-region)
    (define-key map (kbd "C-c e o") #'ear-agent-observe-now)
    (define-key map (kbd "C-c e c") #'ear-agent-chat)
    map)
  "Keymap for `ear-agent-mode'.")

;;;###autoload
(define-minor-mode ear-agent-mode
  "Minor mode for the EAR reflective Emacs agent.

Keys:
  C-c e a  ask agent
  C-c e r  ask agent about region
  C-c e o  observe current state
  C-c e c  open chat buffer"
  :lighter " EAR"
  :keymap ear-agent-mode-map
  (ear-agent-register-default-skills)
  (ear-agent--ensure-dir))

(ear-agent-register-default-skills)

(provide 'sthenno/hermit-endpoint)
