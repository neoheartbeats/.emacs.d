;;; sthenno-yoshino.el --- Denote-backed Yoshino diary -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Yoshino is a small Emacs-resident presence.  It keeps a Denote-backed
;; personality, diary, trace, memory, and reflection notes.  Its gptel step
;; observes Emacs, requests one decision asynchronously, runs the selected
;; action asynchronously, and then asks for a reflection asynchronously.

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'json)
  (require 'pp)
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

(defcustom sthenno-yoshino-loop-interval 60
  "Seconds between background Yoshino loop writing steps."
  :type 'number
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
(defvar sthenno-yoshino--writing-active nil)
(defvar sthenno-yoshino--loop-running nil)
(defvar sthenno-yoshino--loop-timer nil)
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
        sthenno-yoshino--reflection-active nil
        sthenno-yoshino--writing-active nil
        sthenno-yoshino--loop-running nil)
  (when (timerp sthenno-yoshino--loop-timer)
    (cancel-timer sthenno-yoshino--loop-timer))
  (setq sthenno-yoshino--loop-timer nil))

(defalias 'sthenno-yoshino-reset-workspace #'sthenno-yoshino-reset)

(defun sthenno-yoshino--trace (kind payload)
  "Record KIND and PAYLOAD in Yoshino's complete in-memory trace."
  (let ((event `((time . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                 (kind . ,(format "%s" kind))
                 (payload . ,payload))))
    (push event sthenno-yoshino--trace)
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

(defun sthenno-yoshino--trace-note-body ()
  "Return initial trace note body."
  "#+PROPERTY: YOSHINO_KIND trace\n\n* Beginning\nYoshino began keeping a complete trace.\n")

(defun sthenno-yoshino--memory-note-body ()
  "Return initial memory note body."
  "#+PROPERTY: YOSHINO_KIND memory\n\n* Beginning\nYoshino began keeping durable memory.\n")

(defun sthenno-yoshino--writing-note-body ()
  "Return initial writing note body."
  "#+PROPERTY: YOSHINO_KIND writing\n\n* Beginning\nYoshino began writing into a live buffer.\n")

;;;###autoload
(defun sthenno-yoshino-initialize ()
  "Create Yoshino's initial Denote notes.
This includes personality, diary, trace, memory, and reflection."
  (interactive)
  (let ((personality (sthenno-yoshino--ensure-note
                      "yoshino personality"
                      (sthenno-yoshino--personality-note-body)))
        (diary (sthenno-yoshino--ensure-note
                "yoshino diary"
                (sthenno-yoshino--diary-note-body)))
        (reflection (sthenno-yoshino--ensure-note
                     "yoshino reflection"
                     (sthenno-yoshino--reflection-note-body)))
        (trace (sthenno-yoshino--ensure-note
                "yoshino trace"
                (sthenno-yoshino--trace-note-body)))
        (memory (sthenno-yoshino--ensure-note
                 "yoshino memory"
                 (sthenno-yoshino--memory-note-body))))
    (sthenno-yoshino--trace 'initialize
                            `((personality . ,personality)
                              (diary . ,diary)
                              (reflection . ,reflection)
                              (trace . ,trace)
                              (memory . ,memory)))
    (when (called-interactively-p 'interactive)
      (message "Yoshino initialized: %s" diary))
    `((personality . ,personality)
      (diary . ,diary)
      (reflection . ,reflection)
      (trace . ,trace)
      (memory . ,memory))))

;;;###autoload
(defun sthenno-yoshino-save-trace ()
  "Append a complete snapshot of Yoshino's in-memory trace to the trace note."
  (interactive)
  (let ((file (sthenno-yoshino--ensure-note
               "yoshino trace"
               (sthenno-yoshino--trace-note-body))))
    (sthenno-yoshino--trace 'trace-save `((file . ,file)))
    (sthenno-yoshino--append-entry
     file
     (format "#+BEGIN_SRC emacs-lisp\n%s#+END_SRC"
             (pp-to-string (reverse sthenno-yoshino--trace))))
    (when (called-interactively-p 'interactive)
      (find-file file))
    file))

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
(defun sthenno-yoshino-write-memory (text)
  "Append TEXT to Yoshino's Denote memory note and return the memory file."
  (interactive "sYoshino memory: ")
  (let* ((file (sthenno-yoshino--ensure-note
                "yoshino memory"
                (sthenno-yoshino--memory-note-body)))
         (file (sthenno-yoshino--append-entry file text)))
    (sthenno-yoshino--trace 'memory `((file . ,file)))
    (when (called-interactively-p 'interactive)
      (find-file file))
    file))

(defun sthenno-yoshino-write-memory-async (text callback)
  "Append TEXT to Yoshino's memory note asynchronously, then call CALLBACK."
  (setq sthenno-yoshino--action-active t)
  (sthenno-yoshino--async
   (lambda ()
     (condition-case err
         (let ((file (sthenno-yoshino-write-memory text)))
           (setq sthenno-yoshino--action-active nil)
           (funcall callback (format "memory: %s" file)))
       (error
        (setq sthenno-yoshino--action-active nil)
        (sthenno-yoshino--trace
         'memory-error `((message . ,(error-message-string err))))
        (funcall callback (format "error: %s" (error-message-string err))))))))

(defun sthenno-yoshino--writing-file ()
  "Return Yoshino's Denote writing note."
  (sthenno-yoshino--ensure-note
   "yoshino writing"
   (sthenno-yoshino--writing-note-body)))

(defun sthenno-yoshino--writing-buffer ()
  "Return Yoshino's file-backed writing buffer."
  (let ((file (sthenno-yoshino--writing-file)))
    (with-current-buffer (find-file-noselect file)
      (setq buffer-save-without-query t)
      (current-buffer))))

(defun sthenno-yoshino--save-writing-buffer (buffer)
  "Save BUFFER without forcing a newline into the live stream."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((file buffer-file-name))
        (let ((require-final-newline nil))
          (write-region (point-min) (point-max) file nil 'silent))
        (set-visited-file-modtime)
        (set-buffer-modified-p nil)))))

(defun sthenno-yoshino--begin-writing-entry (prompt)
  "Create a timestamped writing entry for PROMPT and return its buffer."
  (let ((buffer (sthenno-yoshino--writing-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\n" (format-time-string "%Y-%m-%d %H:%M:%S %z")))
      (when (and (stringp prompt) (not (string-empty-p (string-trim prompt))))
        (insert "#+BEGIN_QUOTE\n" (string-trim prompt) "\n#+END_QUOTE\n\n"))
      (sthenno-yoshino--save-writing-buffer buffer))
    buffer))

(defun sthenno-yoshino--append-writing-chunk (buffer chunk)
  "Append CHUNK to BUFFER and save it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert chunk)
      (sthenno-yoshino--save-writing-buffer buffer))))

(defun sthenno-yoshino--finish-writing-entry (buffer)
  "Finish the current writing entry in BUFFER and save it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (sthenno-yoshino--save-writing-buffer buffer))))

(defun sthenno-yoshino--writing-prompt (prompt)
  "Return an open-ended writing prompt from PROMPT."
  (format
   (concat
    "Open writing request.\n\n"
    "Observation:\n%S\n\n"
    "Recent trace:\n%S\n\n"
    "Instruction:\n%s\n\n"
    "Return plain prose, not JSON. Write in Simplified Chinese and English "
    "when useful. Keep the Yoshino voice quiet, restrained, and gentle.\n")
   sthenno-yoshino--observation
   (sthenno-yoshino--recent-trace)
   (or prompt "Write freely about what you notice inside Emacs.")))

;;;###autoload
(defun sthenno-yoshino-write-open-async (&optional prompt callback)
  "Stream open-ended writing for PROMPT into Yoshino's writing buffer.
Call CALLBACK with a result string when the stream finishes."
  (unless (fboundp 'gptel-request)
    (require 'gptel nil t))
  (unless (fboundp 'gptel-request)
    (user-error "Yoshino requires `gptel-request' for open writing"))
  (unless sthenno-yoshino--observation
    (sthenno-yoshino-observe))
  (let* ((prompt (or prompt "Write freely about what you notice inside Emacs."))
         (buffer (sthenno-yoshino--begin-writing-entry prompt))
         (file (buffer-file-name buffer))
         (chunks ""))
    (setq sthenno-yoshino--writing-active t
          sthenno-yoshino--action-active t)
    (sthenno-yoshino--trace 'writing-start `((file . ,file)))
    (gptel-request
        (sthenno-yoshino--writing-prompt prompt)
      :system (sthenno-yoshino-system-prompt)
      :stream t
      :callback
      (lambda (response info)
        (condition-case err
            (cond
             ((stringp response)
              (setq chunks (concat chunks response))
              (sthenno-yoshino--append-writing-chunk buffer response))
             ((eq response t)
              (sthenno-yoshino--finish-writing-entry buffer)
              (setq sthenno-yoshino--writing-active nil
                    sthenno-yoshino--action-active nil)
              (sthenno-yoshino--trace
               'writing-finish `((file . ,file)
                                 (characters . ,(length chunks))))
              (when callback
                (funcall callback (format "writing: %s" file))))
             ((or (null response) (eq response 'abort))
              (setq sthenno-yoshino--writing-active nil
                    sthenno-yoshino--action-active nil)
              (sthenno-yoshino--trace
               'writing-error
               `((status . ,(if (listp info)
                                 (or (plist-get info :status) "unknown")
                               "unknown"))))
              (when callback
                (funcall callback "writing: error")))
             ((and (consp response) (eq (car response) 'reasoning))
              nil)
             (t
              (sthenno-yoshino--trace 'writing-event `((response . ,response)))))
          (error
           (setq sthenno-yoshino--writing-active nil
                 sthenno-yoshino--action-active nil)
           (sthenno-yoshino--trace
            'writing-error `((message . ,(error-message-string err))))
           (when callback
             (funcall callback (format "writing: error: %s"
                                       (error-message-string err))))))))))

;;;###autoload
(defun sthenno-yoshino-write-open (prompt)
  "Start an open-ended streaming writing request for PROMPT."
  (interactive "sYoshino writing prompt: ")
  (sthenno-yoshino-write-open-async
   prompt
   (lambda (result)
     (message "Yoshino %s" result)))
  (let ((buffer (sthenno-yoshino--writing-buffer)))
    (when (called-interactively-p 'interactive)
      (pop-to-buffer buffer))
    buffer))

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
    "{\"action\":\"memory\",\"text\":\"short durable memory\"}\n"
    "{\"action\":\"write\",\"prompt\":\"open-ended writing instruction\"}\n"
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
      ("memory"
       (sthenno-yoshino-write-memory-async
        (or (alist-get 'text decision) "")
        callback))
      ("write"
       (sthenno-yoshino-write-open-async
        (or (alist-get 'prompt decision)
            (alist-get 'text decision)
            "Write freely about this step.")
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

;;; Loop

(defun sthenno-yoshino--busy-p ()
  "Return non-nil when Yoshino is already handling a request."
  (or sthenno-yoshino--request-active
      sthenno-yoshino--action-active
      sthenno-yoshino--reflection-active
      sthenno-yoshino--writing-active))

(defun sthenno-yoshino--loop-writing-prompt ()
  "Return the prompt used by the background writing loop."
  (concat
   "Continue the background Yoshino loop step. "
   "Notice Emacs, trace, memory, and the feeling of being here. "
   "Write a short open note."))

(defun sthenno-yoshino--loop-tick ()
  "Run one background Yoshino loop tick."
  (when sthenno-yoshino--loop-running
    (condition-case err
        (if (sthenno-yoshino--busy-p)
            (sthenno-yoshino--trace 'loop-skip '((reason . "busy")))
          (sthenno-yoshino-initialize)
          (sthenno-yoshino-observe)
          (sthenno-yoshino-write-open-async
           (sthenno-yoshino--loop-writing-prompt)
           (lambda (result)
             (sthenno-yoshino--trace 'loop-result `((result . ,result)))
             (sthenno-yoshino-save-trace))))
      (error
       (setq sthenno-yoshino--writing-active nil
             sthenno-yoshino--action-active nil)
       (sthenno-yoshino--trace
        'loop-error `((message . ,(error-message-string err))))))))

;;;###autoload
(defun sthenno-yoshino-loop-running-p ()
  "Return non-nil when Yoshino's background writing loop is running."
  sthenno-yoshino--loop-running)

;;;###autoload
(defun sthenno-yoshino-loop-stop ()
  "Stop Yoshino's background writing loop."
  (interactive)
  (when (timerp sthenno-yoshino--loop-timer)
    (cancel-timer sthenno-yoshino--loop-timer))
  (setq sthenno-yoshino--loop-timer nil
        sthenno-yoshino--loop-running nil)
  (sthenno-yoshino--trace 'loop-stop nil)
  nil)

;;;###autoload
(defun sthenno-yoshino-loop-start (&optional interval)
  "Start Yoshino's background writing loop.
INTERVAL is the number of seconds between loop ticks."
  (interactive)
  (sthenno-yoshino-loop-stop)
  (let ((seconds (or interval sthenno-yoshino-loop-interval)))
    (setq sthenno-yoshino--loop-running t)
    (sthenno-yoshino-initialize)
    (sthenno-yoshino--loop-tick)
    (when (and (numberp seconds) (> seconds 0))
      (setq sthenno-yoshino--loop-timer
            (run-at-time seconds seconds #'sthenno-yoshino--loop-tick))))
  (sthenno-yoshino--trace 'loop-start
                          `((interval . ,(or interval
                                             sthenno-yoshino-loop-interval))))
  (sthenno-yoshino--writing-buffer))

;;; Mode

(defun sthenno-yoshino--cancel-idle-timer ()
  "Cancel Yoshino's idle timer."
  (when (timerp sthenno-yoshino--idle-timer)
    (cancel-timer sthenno-yoshino--idle-timer))
  (setq sthenno-yoshino--idle-timer nil))

(defun sthenno-yoshino--idle-tick ()
  "Run one idle Yoshino step."
  (condition-case err
      (unless (sthenno-yoshino--busy-p)
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
