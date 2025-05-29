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
Looks for ‘.venv’ directory in project root and activates the Python interpreter."
    (interactive)
    (let* ((pr (project-root (project-current t)))
           (venv-path (expand-file-name ".venv" pr))
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

            (message "Activated UV Python environment at %s" venv-path))
        (message "No UV Python environment found in %s" pr))))

  (add-hook 'python-ts-mode-hook 'sthenno/python-venv)

  :bind ((:map python-ts-mode-map
               ;;  ([remap forward-paragraph] . python-nav-forward-statement)
               ;; ([remap backward-paragraph] . python-nav-backward-statement)
               ;; ([remap move-beginning-of-line] . python-nav-beginning-of-statement)
               ;; ([remap move-end-of-line] . python-nav-end-of-statement)
               ("s-<up>" . python-nav-beginning-of-block)
               ("s-<down>" . python-nav-end-of-block)
               ("C-x m" . python-nav-if-name-main)
               ;; ("<tab>" . python-indent-shift-right)
               ;; ("S-<tab>" . python-indent-shift-left)
               )))

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
  :defer t
  :config (add-hook 'python-ts-mode-hook 'flymake-ruff-load))

(use-package ruff-format
  :ensure t
  :defer t
  :config (add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode))

;;; AI

(use-package gptel
  :ensure t
  :diminish (gptel-mode)
  :config
  (setq gptel-default-mode #'org-mode)

  ;; System messages
  (setq gptel-directives
        '((default . "You are a helpful assistant living in Emacs. Respond concisely.")))

  ;; Generation options
  (setq gptel-max-tokens 8192
        gptel-temperature 0.70)

  ;; Use the mode-line to display status info
  (setq gptel-use-header-line nil)

  ;; Use OpenAI as the default backend
  ;;
  (setq gptel-model 'claude_opus4
        gptel-backend (gptel-make-openai "idealab"
                        :protocol "https"
                        :host "idealab.alibaba-inc.com/api/openai"
                        :stream t
                        :key (gptel-api-key-from-auth-source "idealab.alibaba-inc.com")
                        :models '(Qwen3-235B-A22B
                                  Qwen3-32B
                                  claude_opus4
                                  claude_sonnet4
                                  o3-0416-global
                                  o4-mini-0416-global
                                  gemini-2.5-pro-preview-05-06
                                  gemini-2.5-flash-preview-05-20)))

  ;; UI
  ;;
  ;; Scroll automatically as the response is inserted
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)

  ;; Move to the next prompt after the response is inserted
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)

  ;; Org mode specific options
  ;;
  ;; Use the lineage of the current heading as the context for gptel in Org buffers
  (setq gptel-org-branching-context t)

  ;; [TODO] Ignore the independent context
  ;;
  ;; (let ((prefix-in  (if (bound-and-true-p org-modern-horizontal-rule)
  ;;                       "-----\n=In[*]≔= "
  ;;                     "=In[*]≔= "))
  ;;       (prefix-out (if (bound-and-true-p org-modern-horizontal-rule)
  ;;                       "-----\n=Out[*]≔= "
  ;;                     "=Out[*]≔= ")))
  ;;   (setf (alist-get 'org-mode gptel-prompt-prefix-alist) prefix-in)
  ;;   (setf (alist-get 'org-mode gptel-response-prefix-alist) prefix-out))

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist)   "* In[*]≔\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "* Out[*]≔\n")

  ;; Dynamically index `gptel' cells
  (defun sthenno/gpel-index-cells-indexed-p ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(In\\|Out\\)\\[\\([0-9]+\\)\\]" nil t)
        (replace-match "\\1[*]" nil nil))
      (or (re-search-forward "In\\[\\*\\]≔" nil t)
          (re-search-forward "Out\\[\\*\\]≔" nil t))))

  (defun sthenno/gpel-index-cells ()
    (interactive)
    (when (sthenno/gpel-index-cells-indexed-p)
      (sthenno/gpel-index-cells-indexed-p))
    (save-excursion
      (goto-char (point-min))
      (let ((cnt 1))
        (while (re-search-forward "In\\[\\*\\]≔" nil t)
          (replace-match (format "In[%d]≔" cnt))
          (when (re-search-forward "Out\\[\\*\\]≔" nil t)
            (replace-match (format "Out[%d]≔" cnt)))
          (setq cnt (1+ cnt))))))

  (defun gptel--handle-wait (fsm)
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

  (defun gptel--handle-pre-insert (fsm)
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
            (sthenno/gpel-index-cells)
            (run-hooks 'gptel-pre-response-hook))))))

  (defun gptel--handle-post-insert (fsm)
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
        (if (not tracking-marker)       ;Empty response
            (when gptel-mode (gptel--update-status " Empty response" 'success))
          (pulse-momentary-highlight-region start-marker tracking-marker)
          (when gptel-mode
            (save-excursion (goto-char tracking-marker)
                            (insert gptel-response-separator
                                    (gptel-prompt-prefix-string)))
            (gptel--update-status  " 􂮢 Ready" 'success))))
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

  (defun gptel--update-status (&optional msg face)
    "Update status MSG in FACE."
    (when gptel-mode
      (if gptel-use-header-line
          (and (consp header-line-format)
               (setf (nth 1 header-line-format)
                     (thread-first
                       msg
                       (buttonize (lambda (_) (gptel--inspect-fsm)))
                       (propertize 'face face 'mouse-face 'highlight))))
        (if (member msg '(" 􂙎 少女输入中…" " 􀕻 少女祈祷中…"))
            (setq mode-line-process (propertize msg 'face face))
          (setq mode-line-process
                '(:eval (concat " "
                                (buttonize (gptel--model-name gptel-model)
                                           (lambda (&rest _) (gptel-menu))))))
          (message (propertize msg 'face face))))
      (sthenno/gpel-index-cells)
      (force-mode-line-update)))

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
      (turn-on-visual-line-mode)
      (diminish 'visual-line-mode)
      (switch-to-buffer buff)))

  :bind ((:map global-map
               ("s-p" . sthenno/gptel-to-buffer)
               ("s-<return>" . sthenno/gptel-send))
         (:map gptel-mode-map
               ("s-<return>" . sthenno/gptel-send)
               ("M-[" . gptel-beginning-of-response)
               ("M-]" . gptel-end-of-response))))

;;; Teminal support

;; (use-package vterm :ensure t)

;;; GitHub Copilot

;; (use-package copilot
;;   :vc (copilot
;;        :url "https://github.com/copilot-emacs/copilot.el"
;;        :branch "main")
;;   :ensure t
;;   :init (setq copilot-node-executable "/opt/homebrew/bin/node")
;;   :bind ((:map prog-mode-map
;;                ("C-x c" . copilot-mode))
;;          (:map copilot-completion-map
;;                ("TAB"      . copilot-accept-completion)
;;                ("<tab>"    . copilot-accept-completion)
;;                ("<right>" . copilot-accept-completion)
;;                ("<escape>" . copilot-clear-overlay))))

(provide 'init-eglot)

;;; init-eglot.el ends here
