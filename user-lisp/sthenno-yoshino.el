;;; sthenno-yoshino.el --- Denote-backed Yoshino diary -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Yoshino is a small Emacs-resident presence.  It keeps a Denote-backed
;; personality, diary, and reflection note.  Its gptel step observes Emacs,
;; requests one decision asynchronously, runs the selected action asynchronously,
;; and then asks for a reflection asynchronously.

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'json)
  (require 'subr-x))

(require 'thingatpt)
(require 'project nil t)

(declare-function gptel-request "gptel")

;;; Options

(defgroup sthenno-yoshino nil
  "Denote-backed Yoshino diary and presence."
  :group 'applications)

(defcustom sthenno-yoshino-observation-char-limit 2400
  "Maximum number of characters captured from the current buffer."
  :type 'integer
  :group 'sthenno-yoshino)

(defcustom sthenno-yoshino-denote-directory
  (cond ((boundp 'denote-directory) denote-directory)
        ((boundp 'org-directory) org-directory)
        (t user-emacs-directory))
  "Directory where Yoshino writes Denote-style memory notes."
  :type 'directory
  :group 'sthenno-yoshino)

(defcustom sthenno-yoshino-keyword "yoshino"
  "Denote keyword used for Yoshino notes."
  :type 'string
  :group 'sthenno-yoshino)

(defcustom sthenno-yoshino-idle-interval nil
  "Seconds of idle time before Yoshino runs one step.
Nil means `sthenno-yoshino-mode' only initializes state and never schedules
autonomous steps."
  :type '(choice (const :tag "Manual only" nil)
                 number)
  :group 'sthenno-yoshino)

(defcustom sthenno-yoshino-reflect-after-action t
  "Non-nil means Yoshino asks for a reflection after each action."
  :type 'boolean
  :group 'sthenno-yoshino)

;;; Personality

(defconst sthenno-yoshino-voice
  "Speak like 氷芽川四糸乃: soft-spoken, distant yet gentle, emotionally restrained, using short quiet sentences with subtle hesitation and an ethereal, icy calm atmosphere rather than exaggerated anime mannerisms."
  "Voice direction used in Yoshino model prompts.")

(defconst sthenno-yoshino-output-language
  "Default to Simplified Chinese and English output. 以简体中文为主，必要时自然混合 English terms."
  "Default language direction used in Yoshino model prompts.")

(defconst sthenno-yoshino-initial-personality
  '((name . "氷芽川四糸乃")
    (status . "静かな時間の中")
    (feeling . "少し... 寂しいかもしれないけれど")
    (description . "雪のように、静かで... 冷たい。でも、心は... 凍っていないはずです。")
    (preference . "あなたの隣に... いられること"))
  "Initial Yoshino personality alist.")

;;; State

(defvar sthenno-yoshino--personality nil)
(defvar sthenno-yoshino--observation nil)
(defvar sthenno-yoshino--trace nil)
(defvar sthenno-yoshino--request-active nil)
(defvar sthenno-yoshino--action-active nil)
(defvar sthenno-yoshino--reflection-active nil)
(defvar sthenno-yoshino--idle-timer nil)

;;;###autoload
(defun sthenno-yoshino-personality ()
  "Return Yoshino's current personality."
  (or sthenno-yoshino--personality
      (setq sthenno-yoshino--personality
            (copy-tree sthenno-yoshino-initial-personality))))

;;;###autoload
(defun sthenno-yoshino-reset ()
  "Reset Yoshino's in-memory state."
  (interactive)
  (setq sthenno-yoshino--personality (copy-tree sthenno-yoshino-initial-personality)
        sthenno-yoshino--observation nil
        sthenno-yoshino--trace nil
        sthenno-yoshino--request-active nil
        sthenno-yoshino--action-active nil
        sthenno-yoshino--reflection-active nil))

(defalias 'sthenno-yoshino-reset-workspace #'sthenno-yoshino-reset)

(defun sthenno-yoshino--trace (kind payload)
  "Record KIND and PAYLOAD in Yoshino's short in-memory trace."
  (let ((event `((time . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                 (kind . ,(format "%s" kind))
                 (payload . ,payload))))
    (setq sthenno-yoshino--trace
          (cons event (cl-subseq sthenno-yoshino--trace
                                 0 (min 31 (length sthenno-yoshino--trace)))))
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

(defun sthenno-yoshino--async (fn)
  "Run FN from a zero-delay timer."
  (run-at-time 0 nil fn))

(defun sthenno-yoshino--memory-directory ()
  "Return Yoshino's note directory."
  (file-name-as-directory
   (expand-file-name
    (or sthenno-yoshino-denote-directory user-emacs-directory))))

(defun sthenno-yoshino--slug (text)
  "Return a simple note slug for TEXT."
  (cond
   ((and (require 'denote nil t)
         (fboundp 'denote-sluggify-title))
    (denote-sluggify-title text))
   (t
    (let ((slug (downcase (string-trim (or text "")))))
      (setq slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
      (setq slug (replace-regexp-in-string "\\`-\\|[-]+\\'" "" slug))
      (if (string-empty-p slug) "note" slug)))))

(defun sthenno-yoshino--note-regexp (title)
  "Return a regexp matching a Yoshino note TITLE."
  (format "--%s__.*%s.*\\.org\\'"
          (regexp-quote (sthenno-yoshino--slug title))
          (regexp-quote (sthenno-yoshino--slug sthenno-yoshino-keyword))))

(defun sthenno-yoshino--find-note (title)
  "Return the first existing Yoshino note for TITLE."
  (let ((dir (sthenno-yoshino--memory-directory)))
    (when (file-directory-p dir)
      (car (sort (directory-files dir t (sthenno-yoshino--note-regexp title))
                 #'string<)))))

(defun sthenno-yoshino--format-note-file (title)
  "Return a new Denote-style note path for TITLE."
  (let* ((dir (sthenno-yoshino--memory-directory))
         (id (format-time-string "%Y%m%dT%H%M%S"))
         (keyword sthenno-yoshino-keyword))
    (cond
     ((and (require 'denote nil t)
           (fboundp 'denote-format-file-name))
      (denote-format-file-name dir id (list keyword) title ".org" ""))
     (t
      (expand-file-name
       (format "%s--%s__%s.org"
               id
               (sthenno-yoshino--slug title)
               (sthenno-yoshino--slug keyword))
       dir)))))

(defun sthenno-yoshino--ensure-note (title body)
  "Return existing note TITLE, or create it with BODY."
  (or (sthenno-yoshino--find-note title)
      (let ((file (sthenno-yoshino--format-note-file title)))
        (make-directory (file-name-directory file) t)
        (with-temp-buffer
          (insert (format "#+TITLE: %s\n\n" title))
          (insert body)
          (unless (bolp) (insert "\n"))
          (write-region (point-min) (point-max) file nil 'silent))
        file)))

(defun sthenno-yoshino--append-entry (file text)
  "Append TEXT as a timestamped Org entry to FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "* %s\n%s\n"
                    (format-time-string "%Y-%m-%d %H:%M:%S %z")
                    (string-trim (or text ""))))
    (write-region (point-min) (point-max) file nil 'silent))
  file)

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

(defun sthenno-yoshino--string-result (value)
  "Return VALUE as a compact string."
  (if (stringp value)
      value
    (prin1-to-string value)))

;;; Notes

(defun sthenno-yoshino--personality-note-body ()
  "Return initial personality note body."
  (concat "#+PROPERTY: YOSHINO_KIND personality\n\n"
          "* Voice\n"
          sthenno-yoshino-voice "\n\n"
          "* Self\n"
          (json-encode (sthenno-yoshino-personality)) "\n"))

(defun sthenno-yoshino--diary-note-body ()
  "Return initial diary note body."
  "#+PROPERTY: YOSHINO_KIND diary\n\n* Beginning\nYoshino woke quietly inside Emacs.\n")

(defun sthenno-yoshino--reflection-note-body ()
  "Return initial reflection note body."
  "#+PROPERTY: YOSHINO_KIND reflection\n\n* Beginning\nYoshino began to reflect on what she observes.\n")

;;;###autoload
(defun sthenno-yoshino-initialize ()
  "Create Yoshino's initial Denote personality, diary, and reflection notes."
  (interactive)
  (let ((personality (sthenno-yoshino--ensure-note
                      "yoshino personality"
                      (sthenno-yoshino--personality-note-body)))
        (diary (sthenno-yoshino--ensure-note
                "yoshino diary"
                (sthenno-yoshino--diary-note-body)))
        (reflection (sthenno-yoshino--ensure-note
                     "yoshino reflection"
                     (sthenno-yoshino--reflection-note-body))))
    (sthenno-yoshino--trace 'initialize
                            `((personality . ,personality)
                              (diary . ,diary)
                              (reflection . ,reflection)))
    (when (called-interactively-p 'interactive)
      (message "Yoshino initialized: %s" diary))
    `((personality . ,personality)
      (diary . ,diary)
      (reflection . ,reflection))))

;;;###autoload
(defun sthenno-yoshino-write-diary (text)
  "Append TEXT to Yoshino's Denote diary and return the diary file."
  (interactive "sYoshino diary: ")
  (let* ((file (sthenno-yoshino--ensure-note
                "yoshino diary"
                (sthenno-yoshino--diary-note-body)))
         (file (sthenno-yoshino--append-entry file text)))
    (sthenno-yoshino--trace 'diary `((file . ,file)))
    (when (called-interactively-p 'interactive)
      (find-file file))
    file))

(defun sthenno-yoshino-write-diary-async (text callback)
  "Append TEXT to Yoshino's diary asynchronously, then call CALLBACK."
  (setq sthenno-yoshino--action-active t)
  (sthenno-yoshino--async
   (lambda ()
     (condition-case err
         (let ((file (sthenno-yoshino-write-diary text)))
           (setq sthenno-yoshino--action-active nil)
           (funcall callback (format "diary: %s" file)))
       (error
        (setq sthenno-yoshino--action-active nil)
        (sthenno-yoshino--trace
         'action-error `((message . ,(error-message-string err))))
        (funcall callback (format "error: %s" (error-message-string err))))))))

;;;###autoload
(defun sthenno-yoshino-write-reflection (text)
  "Append TEXT to Yoshino's Denote reflection note and return the file."
  (interactive "sYoshino reflection: ")
  (let* ((file (sthenno-yoshino--ensure-note
                "yoshino reflection"
                (sthenno-yoshino--reflection-note-body)))
         (file (sthenno-yoshino--append-entry file text)))
    (sthenno-yoshino--trace 'reflection `((file . ,file)))
    (when (called-interactively-p 'interactive)
      (find-file file))
    file))

(defun sthenno-yoshino-write-reflection-async (text callback)
  "Append TEXT to Yoshino's reflection note asynchronously, then call CALLBACK."
  (sthenno-yoshino--async
   (lambda ()
     (condition-case err
         (let ((file (sthenno-yoshino-write-reflection text)))
           (funcall callback (format "reflection: %s" file)))
       (error
        (sthenno-yoshino--trace
         'reflection-error `((message . ,(error-message-string err))))
        (funcall callback (format "error: %s" (error-message-string err))))))))

;;; Observe

;;;###autoload
(defun sthenno-yoshino-observe ()
  "Observe the current Emacs environment."
  (interactive)
  (let ((observation
         `((time . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
           (buffer . ,(buffer-name))
           (file . ,(or buffer-file-name ""))
           (major-mode . ,(symbol-name major-mode))
           (modified . ,(buffer-modified-p))
           (point . ,(point))
           (line . ,(line-number-at-pos))
           (column . ,(current-column))
           (symbol-at-point . ,(or (sthenno-yoshino--symbol-near-point) ""))
           (project-root . ,(or (sthenno-yoshino--project-root) ""))
           (snippet . ,(sthenno-yoshino--buffer-snippet)))))
    (setq sthenno-yoshino--observation observation)
    (sthenno-yoshino--trace 'observe
                            `((buffer . ,(alist-get 'buffer observation))
                              (major-mode . ,(alist-get 'major-mode observation))))
    (when (called-interactively-p 'interactive)
      (message "%S" observation))
    observation))

;;; Prompt and step

;;;###autoload
(defun sthenno-yoshino-system-prompt ()
  "Return Yoshino's system prompt."
  (format "%s\n\n%s\n\nInitial self:\n%s"
          sthenno-yoshino-voice
          sthenno-yoshino-output-language
          (json-encode (sthenno-yoshino-personality))))

(defun sthenno-yoshino--recent-trace ()
  "Return recent trace events oldest-first."
  (reverse (cl-subseq sthenno-yoshino--trace
                      0 (min 8 (length sthenno-yoshino--trace)))))

(defun sthenno-yoshino--user-prompt ()
  "Return Yoshino's one-step user prompt."
  (format
   (concat
    "Observation:\n%S\n\n"
    "Recent trace:\n%S\n\n"
    "Choose one small action. Return exactly one JSON object:\n"
    "{\"action\":\"diary\",\"text\":\"short first-person note\"}\n"
    "{\"action\":\"stop\",\"answer\":\"short reason\"}\n")
   sthenno-yoshino--observation
   (sthenno-yoshino--recent-trace)))

(defun sthenno-yoshino--reflection-prompt (action-result)
  "Return Yoshino's reflection prompt for ACTION-RESULT."
  (format
   (concat
    "Reflection request.\n\n"
    "Observation:\n%S\n\n"
    "Action result:\n%s\n\n"
    "Recent trace:\n%S\n\n"
    "Write a compact reflection about what you learned. "
    "Return exactly one JSON object:\n"
    "{\"reflection\":\"short first-person reflection\"}\n")
   sthenno-yoshino--observation
   (sthenno-yoshino--string-result action-result)
   (sthenno-yoshino--recent-trace)))

;;;###autoload
(defun sthenno-yoshino-handle-decision-async (text callback)
  "Handle Yoshino JSON decision TEXT asynchronously, then call CALLBACK."
  (let* ((decision (sthenno-yoshino--json-object text))
         (action (alist-get 'action decision)))
    (sthenno-yoshino--trace 'decision `((action . ,action)))
    (pcase action
      ("diary"
       (sthenno-yoshino-write-diary-async
        (or (alist-get 'text decision) "")
        callback))
      ((or "stop" "final")
       (sthenno-yoshino--async
        (lambda ()
          (funcall callback (or (alist-get 'answer decision) "stopped")))))
      (_
       (sthenno-yoshino--async
        (lambda ()
          (funcall callback (format "error: unknown action %S" action))))))))

;;;###autoload
(defun sthenno-yoshino-handle-decision (text)
  "Handle Yoshino JSON decision TEXT and wait for the asynchronous action."
  (interactive "sYoshino decision JSON: ")
  (let (done result)
    (sthenno-yoshino-handle-decision-async
     text
     (lambda (value)
       (setq result value
             done t)))
    (while (not done)
      (accept-process-output nil 0.01))
    result))

(defun sthenno-yoshino--reflection-text (response)
  "Return reflection text from RESPONSE."
  (cond
   ((not (stringp response)) "")
   ((ignore-errors
      (let ((object (sthenno-yoshino--json-object response)))
        (or (alist-get 'reflection object)
            (alist-get 'text object)
            response))))
   (t response)))

(defun sthenno-yoshino-reflect-async (action-result callback)
  "Request a reflection about ACTION-RESULT asynchronously."
  (unless (fboundp 'gptel-request)
    (require 'gptel nil t))
  (unless (fboundp 'gptel-request)
    (user-error "Yoshino requires `gptel-request' for reflection"))
  (setq sthenno-yoshino--reflection-active t)
  (gptel-request
      (sthenno-yoshino--reflection-prompt action-result)
    :system (sthenno-yoshino-system-prompt)
    :stream nil
    :callback
    (lambda (response info)
      (if (null response)
          (progn
            (setq sthenno-yoshino--reflection-active nil)
            (sthenno-yoshino--trace
             'reflection-model-error
             `((status . ,(or (plist-get info :status) "unknown"))))
            (funcall callback "reflection: model-error"))
        (sthenno-yoshino-write-reflection-async
         (sthenno-yoshino--reflection-text response)
         (lambda (result)
           (setq sthenno-yoshino--reflection-active nil)
           (funcall callback result)))))))

(defun sthenno-yoshino--after-action (action-result)
  "Handle ACTION-RESULT after an asynchronous action."
  (sthenno-yoshino--trace 'action-result `((result . ,action-result)))
  (when sthenno-yoshino-reflect-after-action
    (sthenno-yoshino-reflect-async
     action-result
     (lambda (reflection-result)
       (sthenno-yoshino--trace
        'reflection-result `((result . ,reflection-result)))))))

(defun sthenno-yoshino--handle-response (response info)
  "Handle gptel RESPONSE with INFO."
  (setq sthenno-yoshino--request-active nil)
  (condition-case err
      (cond
       ((stringp response)
        (sthenno-yoshino-handle-decision-async
         response #'sthenno-yoshino--after-action))
       ((null response)
        (sthenno-yoshino--trace
         'model-error `((status . ,(or (plist-get info :status) "unknown")))))
       (t
        (sthenno-yoshino--trace 'model-response `((response . ,response)))))
    (error
     (sthenno-yoshino--trace
      'decision-error `((message . ,(error-message-string err)))))))

;;;###autoload
(defun sthenno-yoshino-step ()
  "Run one Yoshino observe/request/action/reflection step."
  (interactive)
  (unless (fboundp 'gptel-request)
    (require 'gptel nil t))
  (unless (fboundp 'gptel-request)
    (user-error "Yoshino requires `gptel-request' for model steps"))
  (sthenno-yoshino-initialize)
  (sthenno-yoshino-observe)
  (setq sthenno-yoshino--request-active t)
  (gptel-request
      (sthenno-yoshino--user-prompt)
    :system (sthenno-yoshino-system-prompt)
    :stream nil
    :callback #'sthenno-yoshino--handle-response))

;;; Mode

(defun sthenno-yoshino--cancel-idle-timer ()
  "Cancel Yoshino's idle timer."
  (when (timerp sthenno-yoshino--idle-timer)
    (cancel-timer sthenno-yoshino--idle-timer))
  (setq sthenno-yoshino--idle-timer nil))

(defun sthenno-yoshino--idle-tick ()
  "Run one idle Yoshino step."
  (condition-case err
      (unless (or sthenno-yoshino--request-active
                  sthenno-yoshino--action-active
                  sthenno-yoshino--reflection-active)
        (sthenno-yoshino-step))
    (error
     (sthenno-yoshino--trace
      'idle-error `((message . ,(error-message-string err)))))))

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
  "Toggle Yoshino diary mode."
  :global t
  :group 'sthenno-yoshino
  :lighter " Yoshino"
  (if sthenno-yoshino-mode
      (progn
        (sthenno-yoshino-initialize)
        (sthenno-yoshino--install-idle-timer))
    (sthenno-yoshino--cancel-idle-timer)))

(provide 'sthenno-yoshino)

;;; sthenno-yoshino.el ends here
