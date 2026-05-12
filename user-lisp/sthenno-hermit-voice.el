;;; sthenno-hermit-voice.el --- Voice input for Hermit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; macOS-oriented voice input for `sthenno-hermit', backed by `gptel'.

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'gptel)
(require 'sthenno-hermit)

;;; Options

(defgroup sthenno/hermit-voice nil
  "Voice input for Hermit."
  :group 'sthenno/hermit)

(defcustom sthenno/hermit-record-command
  "ffmpeg -hide_banner -nostats -loglevel info -f avfoundation -i :0 -af \"highpass=f=80,lowpass=f=8000,afftdn=nf=-25,dynaudnorm=f=150:g=15,silencedetect=n=-35dB:d=1.0\" -ar 16000 -ac 1 -y %f 2>&1"
  "Shell command used to record audio.
The literal token %f is replaced by the shell-quoted output audio file.  The
default command also denoises and normalizes audio before saving it, and emits
ffmpeg silencedetect logs used by Hermit's automatic stop behavior."
  :type 'string
  :group 'sthenno/hermit-voice)

(defcustom sthenno/hermit-transcribe-command nil
  "Shell command used to transcribe recorded audio.
The literal token %f is replaced by the shell-quoted audio file.  The command
must print the transcription to stdout."
  :type '(choice (const :tag "Not configured" nil)
                 string)
  :group 'sthenno/hermit-voice)

(defcustom sthenno/hermit-stt-mode 'stream
  "Speech-to-text interaction mode used by `sthenno/hermit-listen'."
  :type '(choice (const :tag "Streaming microphone transcription" stream)
                 (const :tag "Record then transcribe" record))
  :group 'sthenno/hermit-voice)

(defcustom sthenno/hermit-stream-command nil
  "Shell command used for streaming microphone transcription.
The command must print partial or final transcription text to stdout."
  :type '(choice (const :tag "Not configured" nil)
                 string)
  :group 'sthenno/hermit-voice)

(defcustom sthenno/hermit-voice-history-size 6
  "Number of recent voice turns retained for Hermit conversation context."
  :type 'integer
  :group 'sthenno/hermit-voice)

(defcustom sthenno/hermit-voice-system-prompt
  "你是住在 Emacs 里的 Hermit 桌宠。回答要自然、简短、有帮助。"
  "System prompt used for Hermit's voice conversation."
  :type 'string
  :group 'sthenno/hermit-voice)

(defcustom sthenno/hermit-voice-auto-stop-after-silence t
  "Non-nil means stop recording after speech is followed by silence.
This relies on `sthenno/hermit-record-command' emitting ffmpeg silencedetect
messages.  Custom record commands can disable this behavior by omitting
silencedetect output."
  :type 'boolean
  :group 'sthenno/hermit-voice)

(defcustom sthenno/hermit-voice-min-recording-seconds 0.45
  "Minimum accepted recording duration in seconds."
  :type 'number
  :group 'sthenno/hermit-voice)

(defcustom sthenno/hermit-voice-ignored-transcripts
  '("[BLANK_AUDIO]" "[MUSIC]" "(music)" "you" "字幕由 Amara.org 社群提供")
  "Transcription outputs treated as silence or recognition noise."
  :type '(repeat string)
  :group 'sthenno/hermit-voice)

(defcustom sthenno/hermit-stream-auto-submit-after-seconds 0.80
  "Seconds to wait after the latest streaming transcript before auto-submitting.
Set to nil to require pressing `sthenno/hermit-listen' again to submit."
  :type '(choice (const :tag "Disable auto-submit" nil)
                 number)
  :group 'sthenno/hermit-voice)

;;; State

(defvar sthenno/hermit-voice--history nil)
(defvar sthenno/hermit-voice--record-process nil)
(defvar sthenno/hermit-voice--record-file nil)
(defvar sthenno/hermit-voice--record-start-time nil)
(defvar sthenno/hermit-voice--record-heard-speech nil)
(defvar sthenno/hermit-voice--record-auto-stopping nil)
(defvar sthenno/hermit-voice--record-log-tail "")
(defvar sthenno/hermit-voice--discard-recording nil)
(defvar sthenno/hermit-voice--transcribe-process nil)
(defvar sthenno/hermit-voice--transcribe-buffer nil)
(defvar sthenno/hermit-voice--request-buffer nil)
(defvar sthenno/hermit-voice--request-active nil)
(defvar sthenno/hermit-voice--response-text "")
(defvar sthenno/hermit-voice--stream-process nil)
(defvar sthenno/hermit-voice--stream-output "")
(defvar sthenno/hermit-voice--stream-transcript "")
(defvar sthenno/hermit-voice--stream-auto-submit-timer nil)

;;; Utility

(defun sthenno/hermit-voice--expand-command (command audio-file)
  "Return COMMAND with %f replaced by shell-quoted AUDIO-FILE."
  (unless (and (stringp command) (not (string-empty-p command)))
    (user-error "Hermit voice command is not configured"))
  (replace-regexp-in-string "%f" (shell-quote-argument audio-file)
                            command t t))

(defun sthenno/hermit-voice--plain (text)
  "Return TEXT as trimmed plain output."
  (when (stringp text)
    (string-trim (substring-no-properties text))))

(defun sthenno/hermit-voice--process-live-p (process)
  "Return non-nil when PROCESS is live."
  (and (processp process) (process-live-p process)))

(defun sthenno/hermit-voice--delete-file (file)
  "Delete FILE, ignoring errors."
  (when (and (stringp file) (file-exists-p file))
    (ignore-errors (delete-file file))))

(defun sthenno/hermit-voice--record-elapsed ()
  "Return elapsed seconds for the active or just-finished recording."
  (when sthenno/hermit-voice--record-start-time
    (- (float-time) sthenno/hermit-voice--record-start-time)))

(defun sthenno/hermit-voice--record-too-short-p (elapsed)
  "Return non-nil when ELAPSED seconds is too short to transcribe."
  (and (numberp elapsed)
       (< elapsed (max 0 (or sthenno/hermit-voice-min-recording-seconds 0)))))

(defun sthenno/hermit-voice--noise-transcript-p (text)
  "Return non-nil when TEXT looks like a no-speech transcription artifact."
  (let ((plain (downcase (or (sthenno/hermit-voice--plain text) ""))))
    (cl-some (lambda (ignored)
               (string= plain (downcase ignored)))
             sthenno/hermit-voice-ignored-transcripts)))

(defun sthenno/hermit-voice--strip-ansi (text)
  "Return TEXT without common ANSI terminal control sequences."
  (replace-regexp-in-string "\x1b\\[[0-9;?]*[[:alpha:]]" "" (or text "")))

(defun sthenno/hermit-voice--diagnostic-line-p (line)
  "Return non-nil when LINE is a whisper.cpp diagnostic."
  (string-match-p
   "\\`\\(load_backend:\\|ggml_\\|init:\\|whisper_\\|main:\\|system_info:\\|\\[Start speaking\\]\\)"
   line))

(defun sthenno/hermit-voice--clean-stream-output (output)
  "Return the latest meaningful transcript from streaming OUTPUT."
  (let* ((text (sthenno/hermit-voice--strip-ansi output))
         (text (replace-regexp-in-string "\r" "\n" text))
         (candidates nil))
    (dolist (line (split-string text "\n"))
      (setq line (string-trim line))
      (setq line (replace-regexp-in-string
                  "whisper_full_with_state:.*\\'" "" line))
      (setq line (replace-regexp-in-string "\\[BLANK_AUDIO\\]" "" line))
      (setq line (replace-regexp-in-string "[[:space:]]+" " " line))
      (setq line (string-trim line))
      (unless (or (string-empty-p line)
                  (sthenno/hermit-voice--diagnostic-line-p line)
                  (sthenno/hermit-voice--noise-transcript-p line))
        (push line candidates)))
    (car candidates)))

(defun sthenno/hermit-voice--clean-transcription-output (output)
  "Return cleaned transcription text from raw command OUTPUT."
  (let ((lines nil))
    (dolist (line (split-string (or output "") "\n"))
      (setq line (string-trim line))
      (setq line
            (replace-regexp-in-string
             "\\`\\[[0-9:.]+[[:space:]]+-->[[:space:]]+[0-9:.]+\\][[:space:]]*"
             "" line))
      (unless (or (string-empty-p line)
                  (string-match-p
                   "\\`\\(whisper_\\|system_info\\|main:\\|ggml_\\|\\[.*\\]$\\)"
                   line))
        (push line lines)))
    (string-trim (mapconcat #'identity (nreverse lines) " "))))

(defun sthenno/hermit-voice--request-buffer ()
  "Return the hidden gptel request buffer used by Hermit voice."
  (or (and (buffer-live-p sthenno/hermit-voice--request-buffer)
           sthenno/hermit-voice--request-buffer)
      (setq sthenno/hermit-voice--request-buffer
            (get-buffer-create " *sthenno/hermit-voice-gptel*"))))

(defun sthenno/hermit-voice--turn-limit ()
  "Return maximum number of history messages retained."
  (* 2 (max 0 (or sthenno/hermit-voice-history-size 0))))

(defun sthenno/hermit-voice--remember (role text)
  "Remember ROLE and TEXT in voice history."
  (let ((plain (sthenno/hermit-voice--plain text)))
    (unless (sthenno/hermit--blank-string-p plain)
      (setq sthenno/hermit-voice--history
            (append sthenno/hermit-voice--history
                    (list (cons role plain))))
      (let ((limit (sthenno/hermit-voice--turn-limit)))
        (when (and (> limit 0)
                   (> (length sthenno/hermit-voice--history) limit))
          (setq sthenno/hermit-voice--history
                (last sthenno/hermit-voice--history limit))))))
  sthenno/hermit-voice--history)

(defun sthenno/hermit-voice--format-history ()
  "Return voice history as a compact prompt string."
  (mapconcat
   (lambda (turn)
     (format "%s: %s"
             (pcase (car turn)
               ('user "User")
               ('assistant "Hermit")
               (_ "Message"))
             (cdr turn)))
   sthenno/hermit-voice--history
   "\n"))

;;; LLM interaction

(defun sthenno/hermit-voice--stop-request ()
  "Abort the active gptel request if possible."
  (when (and sthenno/hermit-voice--request-active
             (buffer-live-p (sthenno/hermit-voice--request-buffer))
             (fboundp 'gptel-abort))
    (ignore-errors (gptel-abort (sthenno/hermit-voice--request-buffer))))
  (setq sthenno/hermit-voice--request-active nil
        sthenno/hermit-voice--response-text ""))

(defun sthenno/hermit-voice--stop-transcription ()
  "Stop active transcription and clean its temporary state."
  (when (sthenno/hermit-voice--process-live-p
         sthenno/hermit-voice--transcribe-process)
    (delete-process sthenno/hermit-voice--transcribe-process))
  (when (buffer-live-p sthenno/hermit-voice--transcribe-buffer)
    (kill-buffer sthenno/hermit-voice--transcribe-buffer))
  (sthenno/hermit-voice--delete-file sthenno/hermit-voice--record-file)
  (setq sthenno/hermit-voice--transcribe-process nil
        sthenno/hermit-voice--transcribe-buffer nil
        sthenno/hermit-voice--record-file nil))

(defun sthenno/hermit-voice--discard-recording ()
  "Discard active recording and clean its temporary state."
  (when (sthenno/hermit-voice--process-live-p sthenno/hermit-voice--record-process)
    (delete-process sthenno/hermit-voice--record-process))
  (sthenno/hermit-voice--delete-file sthenno/hermit-voice--record-file)
  (setq sthenno/hermit-voice--record-process nil
        sthenno/hermit-voice--record-file nil
        sthenno/hermit-voice--record-start-time nil
        sthenno/hermit-voice--record-heard-speech nil
        sthenno/hermit-voice--record-auto-stopping nil
        sthenno/hermit-voice--record-log-tail ""
        sthenno/hermit-voice--discard-recording nil))

(defun sthenno/hermit-voice--cancel-stream-auto-submit ()
  "Cancel pending streaming auto-submit."
  (when (timerp sthenno/hermit-voice--stream-auto-submit-timer)
    (cancel-timer sthenno/hermit-voice--stream-auto-submit-timer))
  (setq sthenno/hermit-voice--stream-auto-submit-timer nil))

(defun sthenno/hermit-voice--stream-live-p ()
  "Return non-nil when streaming transcription is active."
  (sthenno/hermit-voice--process-live-p
   sthenno/hermit-voice--stream-process))

(defun sthenno/hermit-voice--stream-command ()
  "Return configured streaming transcription command."
  (unless (and (stringp sthenno/hermit-stream-command)
               (not (string-empty-p sthenno/hermit-stream-command)))
    (user-error "Set `sthenno/hermit-stream-command' before using streaming STT"))
  sthenno/hermit-stream-command)

(defun sthenno/hermit-voice--schedule-stream-auto-submit ()
  "Schedule streaming transcript auto-submit if enabled."
  (sthenno/hermit-voice--cancel-stream-auto-submit)
  (when (and (numberp sthenno/hermit-stream-auto-submit-after-seconds)
             (> sthenno/hermit-stream-auto-submit-after-seconds 0))
    (let ((process sthenno/hermit-voice--stream-process)
          (transcript sthenno/hermit-voice--stream-transcript))
      (setq sthenno/hermit-voice--stream-auto-submit-timer
            (run-at-time
             sthenno/hermit-stream-auto-submit-after-seconds nil
             (lambda ()
               (when (and (eq process sthenno/hermit-voice--stream-process)
                          (string= transcript
                                   sthenno/hermit-voice--stream-transcript))
                 (sthenno/hermit-voice--finish-stream))))))))

(defun sthenno/hermit-voice--reset-stream-state ()
  "Reset streaming transcription state."
  (sthenno/hermit-voice--cancel-stream-auto-submit)
  (setq sthenno/hermit-voice--stream-process nil
        sthenno/hermit-voice--stream-output ""
        sthenno/hermit-voice--stream-transcript ""))

(defun sthenno/hermit-voice--finish-stream (&optional discard)
  "Stop streaming transcription and optionally submit the transcript.
When DISCARD is non-nil, delete the in-progress transcript."
  (let ((process sthenno/hermit-voice--stream-process)
        (transcript (sthenno/hermit-voice--plain
                     sthenno/hermit-voice--stream-transcript)))
    (setq sthenno/hermit-voice--stream-process nil)
    (when (sthenno/hermit-voice--process-live-p process)
      (delete-process process))
    (sthenno/hermit-voice--cancel-stream-auto-submit)
    (setq sthenno/hermit-voice--stream-output ""
          sthenno/hermit-voice--stream-transcript "")
    (cond
     (discard
      (sthenno/hermit-clear-bubble))
     ((or (sthenno/hermit--blank-string-p transcript)
          (sthenno/hermit-voice--noise-transcript-p transcript))
      (sthenno/hermit-say "I could not hear clear speech."))
     (t
      (sthenno/hermit-say "Thinking...")
      (sthenno/hermit-voice--ask transcript)))))

(defun sthenno/hermit-voice--stream-filter (process chunk)
  "Handle streaming transcription PROCESS output CHUNK."
  (when (eq process sthenno/hermit-voice--stream-process)
    (setq sthenno/hermit-voice--stream-output
          (concat sthenno/hermit-voice--stream-output chunk))
    (when (> (length sthenno/hermit-voice--stream-output) 30000)
      (setq sthenno/hermit-voice--stream-output
            (substring sthenno/hermit-voice--stream-output -30000)))
    (let ((transcript (sthenno/hermit-voice--clean-stream-output
                       sthenno/hermit-voice--stream-output)))
      (when (and (not (sthenno/hermit--blank-string-p transcript))
                 (not (sthenno/hermit-voice--noise-transcript-p transcript))
                 (not (string= transcript
                               sthenno/hermit-voice--stream-transcript)))
        (setq sthenno/hermit-voice--stream-transcript transcript)
        (sthenno/hermit-say (format "Listening...\n%s" transcript))
        (sthenno/hermit-voice--schedule-stream-auto-submit)))))

(defun sthenno/hermit-voice--stream-sentinel (process _event)
  "Handle streaming transcription PROCESS exit."
  (when (eq process sthenno/hermit-voice--stream-process)
    (sthenno/hermit-voice--finish-stream)))

(defun sthenno/hermit-voice--start-stream ()
  "Start streaming microphone transcription."
  (when (sthenno/hermit-voice--stream-live-p)
    (user-error "Hermit is already listening"))
  (unless (eq system-type 'darwin)
    (user-error "Hermit voice streaming is currently macOS-only"))
  (sthenno/hermit-voice--stop-request)
  (sthenno/hermit-voice--stop-transcription)
  (sthenno/hermit-voice--discard-recording)
  (sthenno/hermit-voice--reset-stream-state)
  (setq sthenno/hermit-voice--stream-process
        (make-process
         :name "sthenno/hermit-stream"
         :buffer nil
         :command (list shell-file-name shell-command-switch
                        (sthenno/hermit-voice--stream-command))
         :connection-type 'pipe
         :noquery t
         :filter #'sthenno/hermit-voice--stream-filter
         :sentinel #'sthenno/hermit-voice--stream-sentinel))
  (sthenno/hermit-say "Listening live...")
  sthenno/hermit-voice--stream-process)

(defun sthenno/hermit-voice--prompt ()
  "Return the prompt sent to gptel for the current voice history."
  (format "Continue this conversation. Keep the next Hermit reply concise and natural.\n\n%s\nHermit:"
          (sthenno/hermit-voice--format-history)))

(defun sthenno/hermit-voice--gptel-callback (response info)
  "Handle gptel RESPONSE with INFO."
  (cond
   ((stringp response)
    (setq sthenno/hermit-voice--response-text
          (concat sthenno/hermit-voice--response-text response))
    (sthenno/hermit-say sthenno/hermit-voice--response-text))
   ((eq response t)
    (setq sthenno/hermit-voice--request-active nil)
    (unless (sthenno/hermit--blank-string-p sthenno/hermit-voice--response-text)
      (sthenno/hermit-voice--remember
       'assistant sthenno/hermit-voice--response-text)))
   ((eq response 'abort)
    (setq sthenno/hermit-voice--request-active nil))
   ((null response)
    (setq sthenno/hermit-voice--request-active nil)
    (sthenno/hermit-say
     (format "Hermit request failed: %s"
             (or (plist-get info :status) "unknown error"))))))

(defun sthenno/hermit-voice--ask (text)
  "Send transcribed TEXT to gptel."
  (let ((plain (sthenno/hermit-voice--plain text)))
    (when (sthenno/hermit--blank-string-p plain)
      (user-error "Hermit heard no speech"))
    (sthenno/hermit-voice--stop-request)
    (sthenno/hermit-voice--remember 'user plain)
    (setq sthenno/hermit-voice--response-text ""
          sthenno/hermit-voice--request-active t)
    (sthenno/hermit-say plain)
    (with-current-buffer (sthenno/hermit-voice--request-buffer)
      (erase-buffer)
      (gptel-request
          (sthenno/hermit-voice--prompt)
        :buffer (current-buffer)
        :system sthenno/hermit-voice-system-prompt
        :stream t
        :callback #'sthenno/hermit-voice--gptel-callback))))

;;; Recording and transcription

(defun sthenno/hermit-voice--record-file ()
  "Return a new temporary audio file path."
  (make-temp-file "sthenno-hermit-" nil ".wav"))

(defun sthenno/hermit-voice--start-recording ()
  "Start recording audio."
  (when (sthenno/hermit-voice--process-live-p sthenno/hermit-voice--record-process)
    (user-error "Hermit is already listening"))
  (unless (eq system-type 'darwin)
    (user-error "Hermit voice recording is currently macOS-only"))
  (unless (executable-find "ffmpeg")
    (user-error "Cannot find ffmpeg for Hermit voice recording"))
  (sthenno/hermit-voice--stop-request)
  (sthenno/hermit-voice--stop-transcription)
  (let* ((file (sthenno/hermit-voice--record-file))
         (command (sthenno/hermit-voice--expand-command
                   sthenno/hermit-record-command file)))
    (setq sthenno/hermit-voice--record-file file
          sthenno/hermit-voice--record-start-time (float-time)
          sthenno/hermit-voice--record-heard-speech nil
          sthenno/hermit-voice--record-auto-stopping nil
          sthenno/hermit-voice--record-log-tail ""
          sthenno/hermit-voice--discard-recording nil
          sthenno/hermit-voice--record-process
          (make-process
           :name "sthenno/hermit-record"
           :buffer nil
           :command (list shell-file-name shell-command-switch command)
           :connection-type 'pipe
           :noquery t
           :filter #'sthenno/hermit-voice--record-filter
           :sentinel #'sthenno/hermit-voice--record-sentinel))
    (sthenno/hermit-say
     (if sthenno/hermit-voice-auto-stop-after-silence
         "Listening... pause after speaking to finish."
       "Listening... press again to finish."))
    sthenno/hermit-voice--record-process))

(defun sthenno/hermit-voice--record-filter (process chunk)
  "Handle recording PROCESS output CHUNK for silence auto-stop."
  (when (eq process sthenno/hermit-voice--record-process)
    (setq sthenno/hermit-voice--record-log-tail
          (concat sthenno/hermit-voice--record-log-tail chunk))
    (when (> (length sthenno/hermit-voice--record-log-tail) 4000)
      (setq sthenno/hermit-voice--record-log-tail
            (substring sthenno/hermit-voice--record-log-tail -4000)))
    (when (string-match-p "silence_end:" chunk)
      (setq sthenno/hermit-voice--record-heard-speech t))
    (when (and sthenno/hermit-voice-auto-stop-after-silence
               sthenno/hermit-voice--record-heard-speech
               (not sthenno/hermit-voice--record-auto-stopping)
               (string-match-p "silence_start:" chunk)
               (not (sthenno/hermit-voice--record-too-short-p
                     (sthenno/hermit-voice--record-elapsed))))
      (setq sthenno/hermit-voice--record-auto-stopping t)
      (run-at-time
       0 nil
       (lambda (record-process)
         (when (and (eq record-process sthenno/hermit-voice--record-process)
                    (sthenno/hermit-voice--process-live-p record-process))
           (ignore-errors (sthenno/hermit-voice--stop-recording))))
       process))))

(defun sthenno/hermit-voice--stop-recording (&optional discard)
  "Stop the active recording.
When DISCARD is non-nil, delete the audio instead of transcribing it."
  (unless (sthenno/hermit-voice--process-live-p sthenno/hermit-voice--record-process)
    (user-error "Hermit is not listening"))
  (setq sthenno/hermit-voice--discard-recording discard)
  (process-send-string sthenno/hermit-voice--record-process "q")
  (run-at-time
   0.5 nil
   (lambda (process)
     (when (sthenno/hermit-voice--process-live-p process)
       (interrupt-process process)))
   sthenno/hermit-voice--record-process)
  (sthenno/hermit-say "Thinking..."))

(defun sthenno/hermit-voice--record-sentinel (process _event)
  "Handle recording PROCESS completion."
  (when (eq process sthenno/hermit-voice--record-process)
    (let ((file sthenno/hermit-voice--record-file)
          (discard sthenno/hermit-voice--discard-recording)
          (elapsed (sthenno/hermit-voice--record-elapsed)))
      (setq sthenno/hermit-voice--record-process nil
            sthenno/hermit-voice--record-file nil
            sthenno/hermit-voice--record-start-time nil
            sthenno/hermit-voice--record-heard-speech nil
            sthenno/hermit-voice--record-auto-stopping nil
            sthenno/hermit-voice--record-log-tail ""
            sthenno/hermit-voice--discard-recording nil)
      (cond
       (discard
        (sthenno/hermit-voice--delete-file file)
        (sthenno/hermit-clear-bubble))
       ((sthenno/hermit-voice--record-too-short-p elapsed)
        (sthenno/hermit-voice--delete-file file)
        (sthenno/hermit-say "That was too short. Try again."))
       ((and (stringp file)
             (file-readable-p file)
             (> (file-attribute-size (file-attributes file)) 0))
        (sthenno/hermit-voice--start-transcription file))
       (t
        (sthenno/hermit-voice--delete-file file)
        (sthenno/hermit-say "No audio captured."))))))

(defun sthenno/hermit-voice--start-transcription (file)
  "Start transcription for audio FILE."
  (unless sthenno/hermit-transcribe-command
    (sthenno/hermit-voice--delete-file file)
    (user-error "Set `sthenno/hermit-transcribe-command' before using Hermit voice"))
  (when (sthenno/hermit-voice--process-live-p sthenno/hermit-voice--transcribe-process)
    (delete-process sthenno/hermit-voice--transcribe-process))
  (let ((buffer (generate-new-buffer " *sthenno/hermit-transcribe*"))
        (command (sthenno/hermit-voice--expand-command
                  sthenno/hermit-transcribe-command file)))
    (setq sthenno/hermit-voice--transcribe-buffer buffer
          sthenno/hermit-voice--record-file file
          sthenno/hermit-voice--transcribe-process
          (make-process
           :name "sthenno/hermit-transcribe"
           :buffer buffer
           :command (list shell-file-name shell-command-switch command)
           :connection-type 'pipe
           :noquery t
           :sentinel #'sthenno/hermit-voice--transcribe-sentinel))
    (sthenno/hermit-say "Transcribing...")))

(defun sthenno/hermit-voice--transcribe-sentinel (process event)
  "Handle transcription PROCESS EVENT."
  (when (eq process sthenno/hermit-voice--transcribe-process)
    (let* ((buffer sthenno/hermit-voice--transcribe-buffer)
           (file sthenno/hermit-voice--record-file)
           (status (process-exit-status process))
           (output (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (buffer-string))))
           (transcript (sthenno/hermit-voice--clean-transcription-output
                        output)))
      (setq sthenno/hermit-voice--transcribe-process nil
            sthenno/hermit-voice--transcribe-buffer nil
            sthenno/hermit-voice--record-file nil)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (sthenno/hermit-voice--delete-file file)
      (if (and (zerop status)
               (not (sthenno/hermit--blank-string-p transcript))
               (not (sthenno/hermit-voice--noise-transcript-p transcript)))
          (sthenno/hermit-voice--ask transcript)
        (sthenno/hermit-say
         (if (zerop status)
             "I could not hear clear speech."
           (format "Transcription failed: %s"
                   (string-trim (or event "unknown error")))))))))

;;; Public commands

;;;###autoload
(defun sthenno/hermit-listen ()
  "Toggle Hermit voice recording.
In streaming mode, the first invocation starts live transcription and the second
submits the current transcript to gptel.  In record mode, the first invocation
starts recording, and the second stops recording and runs
`sthenno/hermit-transcribe-command'."
  (interactive)
  (cond
   ((sthenno/hermit-voice--stream-live-p)
    (sthenno/hermit-voice--finish-stream))
   ((sthenno/hermit-voice--process-live-p sthenno/hermit-voice--record-process)
    (sthenno/hermit-voice--stop-recording))
   ((eq sthenno/hermit-stt-mode 'stream)
    (sthenno/hermit-voice--start-stream))
   ((eq sthenno/hermit-stt-mode 'record)
    (sthenno/hermit-voice--start-recording))
   (t
    (user-error "Unknown Hermit STT mode: %S" sthenno/hermit-stt-mode))))

;;;###autoload
(defun sthenno/hermit-voice-stop ()
  "Stop Hermit voice recording, transcription and LLM requests."
  (interactive)
  (when (sthenno/hermit-voice--stream-live-p)
    (sthenno/hermit-voice--finish-stream t))
  (sthenno/hermit-voice--discard-recording)
  (sthenno/hermit-voice--stop-transcription)
  (sthenno/hermit-voice--stop-request)
  (sthenno/hermit-say "Stopped."))

;;;###autoload
(defun sthenno/hermit-voice-reset-history ()
  "Clear Hermit's voice conversation history."
  (interactive)
  (setq sthenno/hermit-voice--history nil)
  (sthenno/hermit-say "Voice history cleared."))

(provide 'sthenno-hermit-voice)

;;; sthenno-hermit-voice.el ends here
