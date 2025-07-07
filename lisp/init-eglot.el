;;; init-eglot.el --- LSP support by Eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Sthenno <sthenno@sthenno.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file complement the develop environment for specific languages.
;;

;;; Code:
;;

;;; Setup `treesit' for better performance for processing coding syntax

(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (setopt treesit-auto-install t)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

;; Append *-mode-hook to *-ts-mode-hook for modes in `major-mode-remap-list'
(mapc #'(lambda (major-mode-remap)
          (let ((major-mode-hook
                 (intern (concat (symbol-name (car major-mode-remap)) "-hook")))
                (major-ts-mode-hook
                 (intern (concat (symbol-name (cdr major-mode-remap)) "-hook"))))
            (add-hook major-ts-mode-hook `(lambda ()
                                            (run-hooks (quote ,major-mode-hook))))))
      major-mode-remap-alist)

(setq-default treesit-font-lock-level 4)

;;; Initialize `eglot'

(use-package eglot
  :ensure t
  :config
  (setq eglot-server-programs
        `(((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))))

  ;; Hooks
  (add-hook 'python-ts-mode-hook 'eglot-ensure)

  :bind (:map eglot-mode-map
              ("<f2>" . eglot-rename)))

;; Boost eglot using lsp-booster

(use-package eglot-booster
  :vc (eglot-booster
       :url "https://github.com/jdtsmith/eglot-booster"
       :branch "main")
  :ensure t
  :after eglot
  :init (add-to-list 'exec-path (locate-user-emacs-file "bin/"))
  :config (eglot-booster-mode 1))

;;; Python

(use-package python
  :init (setq-default python-indent-offset 4
                      python-indent-guess-indent-offset nil
                      python-indent-guess-indent-offset-verbose nil)
  :config

  ;; Python project management
  (defun sthenno/python-venv ()
    "Activate Python environment managed by uv based on current
project directory.

Looks for `.venv' directory in project root and activates the Python
interpreter."
    (interactive)
    (let* ((project-root (project-root (project-current t)))
           (venv-path (expand-file-name ".venv" project-root))
           (python-path (expand-file-name "bin/python" venv-path)))
      (if (file-exists-p python-path)
          (progn
            ;; Set Python interpreter path
            (setq python-shell-interpreter python-path)

            ;; Update `exec-path' to include the venv’s bin directory
            (let ((venv-bin-dir (file-name-directory python-path)))
              (setq exec-path (cons venv-bin-dir
                                    (remove venv-bin-dir exec-path))))

            ;; Update PATH environment
            (setenv "PATH" (concat (file-name-directory python-path)
                                   path-separator
                                   (getenv "PATH")))
            (setenv "VIRTUAL_ENV" venv-path)

            ;; Remove PYTHONHOME if it exists
            (setenv "PYTHONHOME" nil)

            (message "Activated uv Python environment at %s" venv-path))
        (message "No uv Python environment found in %s" project-root))))
  (add-hook 'python-ts-mode-hook 'sthenno/python-venv)

  :bind ((:map python-ts-mode-map
               ("s-<up>" . python-nav-beginning-of-block)
               ("s-<down>" . python-nav-end-of-block)
               ("C-x m" . python-nav-if-name-main))))

;;; Flymake

;; (use-package flymake
;;   :config
;;   ;; (setq flymake-no-changes-timeout nil)
;;   (setq flymake-start-on-save-buffer t)
;;   (setq flymake-mode-line-lighter "FM")
;;   (setq flymake-indicator-type 'margins)
;;   :hook (prog-mode . flymake-mode)
;;   :bind ((:map flymake-mode-map
;;                ("s-<down>" . flymake-goto-next-error)
;;                ("s-<up>"   . flymake-goto-prev-error))))

(use-package flymake-ruff
  :ensure t
  :config (add-hook 'python-ts-mode-hook 'flymake-ruff-load))

(use-package ruff-format
  :ensure t
  :config (add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode))

;;; AI

(use-package gptel
  :ensure t
  :diminish (gptel-mode)
  :config
  (setq gptel-default-mode #'org-mode)

  ;; Use OpenAI as the default backend
  ;;
  ;; (defun sthenno/gptel-setup-idealab ()
  ;;   (setq gptel-model 'claude_sonnet4
  ;;         gptel-backend (gptel-make-openai "idealab"
  ;;                         :protocol "https"
  ;;                         :host "idealab.alibaba-inc.com/api/openai"
  ;;                         :stream t
  ;;                         :key
  ;;                         (gptel-api-key-from-auth-source "idealab.alibaba-inc.com")
  ;;                         :models '(claude_opus4
  ;;                                   claude_sonnet4
  ;;                                   o3-0416-global
  ;;                                   o4-mini-0416-global))))

  (defun sthenno/gptel-setup-local ()
    (setq gptel-model 'sthenno
          gptel-backend (gptel-make-openai "local"
                          :protocol "http"
                          :host "localhost:30003"
                          :endpoint "/v1/chat/completions"
                          :stream t
                          :key "tmp"
                          :models '(sthenno))))
  (sthenno/gptel-setup-local)

  ;; System messages
  ;;
  ;; (setq gptel-directives
  ;;       `((default . ,(if (eq gptel-model 'sthenno)
  ;;                         (concat "You an AI assistant named Sthenno. "
  ;;                                 "The user is your architect, "
  ;;                                 "or more formally, your instructor.")
  ;;                       (concat "You are a large language model living in Emacs "
  ;;                               "and a helpful assistant. Respond concisely.")))))

  ;; Generation options
  (setq gptel-max-tokens 1024
        gptel-temperature 0.70)

  (add-hook 'gptel-mode-hook #'(lambda ()
                                 (setq-local gptel-max-tokens (* 4 1024)
                                             gptel-org-branching-context nil)

                                 ;; Scroll automatically as the response is inserted
                                 (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll
                                           90 t)
                                 ;; Move to the next prompt after the response is
                                 ;; inserted
                                 (add-hook 'gptel-post-response-hook
                                           #'(lambda (&optional _ _ arg)
                                               (goto-char (point-max)))
                                           90 t)))

  ;; FIXME: Tool use
  ;;

  ;; (setq gptel-tools
  ;;       `(,(gptel-make-tool :name "read_file"
  ;;                           :function (lambda (filepath)
  ;;                                       (unless (file-exists-p filepath)
  ;;                                         (error
  ;;                                          "error: file %s does not exist." filepath))
  ;;                                       (with-temp-buffer
  ;;                                         (insert-file-contents filepath)
  ;;                                         (buffer-string)))
  ;;                           :description "Read the contents of a file"
  ;;                           :args `((
  ;;                                    :name "filepath"
  ;;                                    :type string
  ;;                                    :description "The path to the file to be read"))
  ;;                           :category "filesystem")
  ;;         ,(gptel-make-tool :name "create_file"
  ;;                           :function
  ;;                           (lambda (path filename content)
  ;;                             (let ((full-path (expand-file-name filename path)))
  ;;                               (with-temp-buffer
  ;;                                 (insert content)
  ;;                                 (write-file full-path))
  ;;                               (format "Created file %s in %s" filename path)))
  ;;                           :description "Create a new file with the specified content"
  ;;                           :args `((
  ;;                                    :name "path"
  ;;                                    :type string
  ;;                                    :description "The directory where to create the file")
  ;;                                   (
  ;;                                    :name "filename"
  ;;                                    :type string
  ;;                                    :description "The name of the file to create")
  ;;                                   (
  ;;                                    :name "content"
  ;;                                    :type string
  ;;                                    :description "The content to write to the file"))
  ;;                           :category "filesystem")))

  ;; Use the mode-line to display status info
  (setq gptel-use-header-line nil)

  ;; UI
  ;;

  ;; Org mode specific options
  ;;
  ;; Use the lineage of the current heading as the context for gptel in Org buffers
  (setq gptel-org-branching-context t)

  ;; [TODO] Ignore the independent context
  ;;
  (let ((suffix "[*]≔\n")
        (prefix (if (bound-and-true-p org-modern-horizontal-rule)
                    '((in  . "-----\n*** In")
                      (out . "-----\n*** Out"))
                  '((in  . "*** In")
                    (out . "*** Out")))))
    (cl-macrolet ((set-prefix (alist type)
                    `(setf (alist-get 'org-mode ,alist)
                           (concat (alist-get ',type prefix) suffix))))
      (set-prefix gptel-prompt-prefix-alist in)
      (set-prefix gptel-response-prefix-alist out)))

  ;; Dynamically index `gptel' cells
  (defun sthenno/gptel-index-cells ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(In\\|Out\\)\\[[0-9]+\\]≔" nil t)
        (replace-match "\\1[*]≔"))
      (goto-char (point-min))
      (cl-loop for n from 1
               while (re-search-forward "In\\[\\*\\]≔" nil t)
               do (progn
                    (replace-match (format "In[%d]≔" n))
                    (when (re-search-forward "Out\\[\\*\\]≔" nil t)
                      (replace-match (format "Out[%d]≔" n)))))))

  (define-advice gptel--handle-wait
      (:override (fsm) sthenno/gptel--handle-wait)
    "Fire the request contained in state machine FSM's info."
    ;; Reset some flags in info.  This is necessary when reusing fsm's context for
    ;; a second network request: gptel tests for the presence of these flags to
    ;; handle state transitions.  (NOTE: Don't add :token to this.)
    (let ((info (gptel-fsm-info fsm)))
      (dolist (key '(:tool-success :tool-use :error :http-status :reasoning))
        (when (plist-get info key)
          (plist-put info key nil))))
    (funcall
     (if gptel-use-curl
         #'gptel-curl-get-response
       #'gptel--url-get-response)
     fsm)
    (run-hooks 'gptel-post-request-hook)
    (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
      (gptel--update-status " 􀕻 少女祈祷中…" 'org-formula)))

  (define-advice gptel--handle-pre-insert
      (:override (fsm) sthenno/gptel--handle-pre-insert)
    "Tasks before inserting the LLM response for state FSM.

Handle read-only buffers and run pre-response hooks (but only if
the request succeeded)."
    (let* ((info (gptel-fsm-info fsm))
           (start-marker (plist-get info :position)))
      (when (and
             (memq (plist-get info :callback)
                   '(gptel--insert-response gptel-curl--stream-insert-response))
             (with-current-buffer (plist-get info :buffer)
               (or buffer-read-only
                   (get-char-property start-marker 'read-only))))
        (message "Buffer is read only, displaying reply in buffer \"*LLM response*\"")
        (display-buffer
         (with-current-buffer (get-buffer-create "*LLM response*")
           (visual-line-mode 1)
           (goto-char (point-max))
           (move-marker start-marker (point) (current-buffer))
           (current-buffer))
         '((display-buffer-reuse-window
            display-buffer-pop-up-window)
           (reusable-frames . visible))))
      (with-current-buffer (marker-buffer start-marker)
        (when (plist-get info :stream)
          (gptel--update-status " 􂙎 少女输入中…" 'success))
        (save-excursion
          (goto-char start-marker)
          (when (and (member (plist-get info :http-status) '("200" "100"))
                     gptel-pre-response-hook)
            (run-hooks 'gptel-pre-response-hook))))))

  (define-advice gptel--handle-post-insert
      (:override (fsm) sthenno/gptel--handle-post-insert)
    "Tasks after successfully inserting the LLM response with state FSM.

Indicate gptel status, pulse the inserted text and run post-response hooks.

No state transition here since that's handled by the process sentinels."
    (let* ((info (gptel-fsm-info fsm))
           (start-marker (plist-get info :position))
           (tracking-marker (or (plist-get info :tracking-marker)
                                start-marker))

           ;; start-marker may have been moved if :buffer was read-only
           (gptel-buffer (marker-buffer start-marker)))
      (with-current-buffer gptel-buffer
        (if (not tracking-marker)       ; Empty response
            (when gptel-mode (gptel--update-status " Empty response" 'success))
          (pulse-momentary-highlight-region start-marker tracking-marker)
          (when gptel-mode
            (save-excursion (goto-char tracking-marker)
                            (insert gptel-response-separator
                                    (gptel-prompt-prefix-string)))
            (gptel--update-status  " 􂮢 预备" 'success))))

      ;; Run hook in visible window to set window-point, BUG #269
      (if-let* ((gptel-window (get-buffer-window gptel-buffer 'visible)))
          (with-selected-window gptel-window
            (run-hook-with-args
             'gptel-post-response-functions
             (marker-position start-marker) (marker-position tracking-marker)))
        (with-current-buffer gptel-buffer
          (run-hook-with-args
           'gptel-post-response-functions
           (marker-position start-marker) (marker-position tracking-marker))))))

  (define-advice gptel--handle-tool-use
      (:override (fsm) sthenno/gptel--handle-tool-use)
    "Run tool calls captured in FSM, and advance the state machine with the results."
    (when-let* ((info (gptel-fsm-info fsm))
                (backend (plist-get info :backend))
                ;; This function might run many times, so only act on the remaining tool calls.
                (tool-use (cl-remove-if (lambda (tc)
                                          (plist-get tc :result))
                                        (plist-get info :tool-use)))
                (ntools (length tool-use))
                (tool-idx 0))
      (with-current-buffer (plist-get info :buffer)
        (when gptel-mode
          (gptel--update-status
           (format " 􃂔 少女电话中…" ) 'mode-line-emphasis))

        (let ((result-alist) (pending-calls))
          (mapc                         ; Construct function calls
           (lambda (tool-call)
             (letrec ((args (plist-get tool-call :args))
                      (name (plist-get tool-call :name))
                      (arg-values nil)
                      (tool-spec
                       (cl-find-if
                        (lambda (ts)
                          (equal (gptel-tool-name ts) name))
                        (plist-get info :tools)))
                      (process-tool-result
                       (lambda (result)
                         (plist-put info :tool-success t)
                         (let ((result (gptel--to-string result)))
                           (plist-put tool-call :result result)
                           (push (list tool-spec args result) result-alist))
                         (cl-incf tool-idx)
                         (when (>= tool-idx ntools) ; All tools have run
                           (gptel--inject-prompt
                            backend (plist-get info :data)
                            (gptel--parse-tool-results
                             backend (plist-get info :tool-use)))
                           (funcall (plist-get info :callback)
                                    (cons 'tool-result result-alist) info)
                           (gptel--fsm-transition fsm)))))
               (if (null tool-spec)
                   (message "Unknown tool called by model: %s" name)
                 (setq arg-values
                       (mapcar
                        (lambda (arg)
                          (let ((key (intern (concat ":" (plist-get arg :name)))))
                            (plist-get args key)))
                        (gptel-tool-args tool-spec)))
                 ;; Check if tool requires confirmation
                 (if (and gptel-confirm-tool-calls (or (eq gptel-confirm-tool-calls t)
                                                       (gptel-tool-confirm tool-spec)))
                     (push (list tool-spec arg-values process-tool-result)
                           pending-calls)
                   ;; If not, run the tool
                   (if (gptel-tool-async tool-spec)
                       (apply (gptel-tool-function tool-spec)
                              process-tool-result arg-values)
                     (let ((result
                            (condition-case errdata
                                (apply (gptel-tool-function tool-spec) arg-values)
                              (error (mapconcat #'gptel--to-string errdata " ")))))
                       (funcall process-tool-result result)))))))
           tool-use)
          (when pending-calls
            (setq gptel--fsm-last fsm)
            (when gptel-mode (gptel--update-status
                              (format " Run tools?" ) 'mode-line-emphasis))
            (funcall (plist-get info :callback)
                     (cons 'tool-call pending-calls) info))))))

  (define-advice gptel--update-status
      (:override (&optional msg face) sthenno/gptel--update-status)
    "Update status MSG in FACE."
    (when gptel-mode
      (if gptel-use-header-line
          (and (consp header-line-format)
               (setf (nth 1 header-line-format)
                     (thread-first
                       msg
                       (buttonize (lambda (_)
                                    (gptel--inspect-fsm)))
                       (propertize 'face face 'mouse-face 'highlight))))
        (if (member msg '(" 􂙎 少女输入中…" " 􀕻 少女祈祷中…" " 􃂔 少女电话中…"))
            (setq mode-line-process (propertize msg 'face face))
          (setq mode-line-process
                '(:eval (concat " "
                                (buttonize (gptel--model-name gptel-model)
                                           (lambda (&rest _)
                                             (gptel-menu))))))
          (message (propertize msg 'face face))))
      (run-hooks 'sthenno/gptel--update-status-hook)
      (force-mode-line-update)))
  (add-hook 'sthenno/gptel--update-status-hook #'sthenno/gptel-index-cells)

  (defun sthenno/gptel-send (&optional arg)
    "Submit this prompt to the current LLM backend.

By default, the contents of the buffer up to the cursor position
are sent.  If the region is active, its contents are sent
instead.

The response from the LLM is inserted below the cursor position
at the time of sending.  To change this behavior or model
parameters, use prefix arg ARG activate a transient menu with
more options instead.

This command is asynchronous, you can continue to use Emacs while
waiting for the response."
    (interactive "P")
    (if (and arg (require 'gptel-transient nil t))
        (call-interactively #'gptel-menu)
      (when gptel-mode
        (goto-char (point-max)))
      (message "􀕻 正在联系 %s…" (gptel--model-name gptel-model))
      (gptel--sanitize-model)
      (gptel-request nil
        :stream gptel-stream
        :fsm (gptel-make-fsm :handlers gptel-send--handlers))
      (gptel--update-status " 􀕻 少女祈祷中…" 'org-formula)))

  ;; Functions of the `gptel' buffer
  (defun sthenno/gptel-to-buffer ()
    "Open the gptel buffer."
    (interactive)
    (let ((buff (concat "*" (gptel-backend-name gptel-backend) "*")))
      (gptel buff)
      (switch-to-buffer buff)))

  :bind ((:map global-map
               ("s-p" . sthenno/gptel-to-buffer)
               ("s-<return>" . sthenno/gptel-send))
         (:map gptel-mode-map
               ("s-<return>" . sthenno/gptel-send))))

(provide 'init-eglot)

;;; init-eglot.el ends here
