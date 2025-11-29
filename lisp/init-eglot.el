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

  :bind ((:map eglot-mode-map
               ("<f2>" . eglot-rename))
         (:map python-base-mode-map
               ("C-c e" . eglot))))

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

;; The built-in `python' environment
(use-package python
  :init (setq-default python-indent-offset 4
                      python-indent-guess-indent-offset nil
                      python-indent-guess-indent-offset-verbose nil)
  :config

  ;; Python project management
  ;;
  ;; TODO: This function need to be updated.
  ;; (defun sthenno/python-venv ()
  ;;       "Activate Python environment managed by uv based on current
  ;; project directory.

  ;; Looks for `.venv' directory in project root and activates the Python
  ;; interpreter."
  ;;       (interactive)
  ;;       (let* ((project-root (project-root (project-current t)))
  ;;              (venv-path (expand-file-name ".venv" project-root))
  ;;              (python-path (expand-file-name "bin/python" venv-path)))
  ;;         (if (file-exists-p python-path)
  ;;             (progn

  ;;               ;; Set Python interpreter path
  ;;               (setq python-shell-interpreter python-path)

  ;;               ;; Update `exec-path' to include the venv’s bin directory
  ;;               (let ((venv-bin-dir (file-name-directory python-path)))
  ;;                 (setq exec-path (cons venv-bin-dir
  ;;                                       (remove venv-bin-dir exec-path))))

  ;;               ;; Update PATH environment
  ;;               (setenv "PATH" (concat (file-name-directory python-path)
  ;;                                      path-separator
  ;;                                      (getenv "PATH")))
  ;;               (setenv "VIRTUAL_ENV" venv-path)

  ;;               ;; Remove PYTHONHOME if it exists
  ;;               (setenv "PYTHONHOME" nil)

  ;;               (message "Activated uv Python environment at %s" venv-path))
  ;;           (message "No uv Python environment found in %s" project-root))))
  ;; (add-hook 'python-ts-mode-hook 'sthenno/python-venv)

  :bind (:map python-ts-mode-map
              ("s-<up>"   . python-nav-beginning-of-block)
              ("s-<down>" . python-nav-end-of-block)
              ("C-x m"    . python-nav-if-name-main)))

;;; Flymake

;; (use-package flymake
;;   :config
;;   (setq flymake-no-changes-timeout nil)
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

  ;; (defun sthenno/gptel-setup-local ()
  ;;   (setq gptel-model 'sthenno
  ;;         gptel-backend (gptel-make-openai "local"
  ;;                                          :protocol "http"
  ;;                                          :host "localhost:30003"
  ;;                                          :endpoint "/v1/chat/completions"
  ;;                                          :stream t
  ;;                                          :key "tmp"
  ;;                                          :models '(sthenno))))
  ;; (sthenno/gptel-setup-local)

  ;; (defun sthenno/gptel-setup-openai ()
  ;;   (setq gptel-model 'ft:gpt-4.1-2025-04-14:lingxin:exp-0723-002:BwRgAyL5:ckpt-step-194
  ;;         gptel-backend (gptel-make-openai "exp-default"
  ;;                         :models '(ft:gpt-4.1-2025-04-14:lingxin:exp-0723-002:BwRgAyL5:ckpt-step-194)
  ;;                         :stream t
  ;;                         :key (gptel-api-key-from-auth-source "api.openai.com")))
  ;;   (setq gptel-directives "You are Sthenno. 你是一位名叫 Sthenno 的虚拟助手，用户则是你的设计者。你的回复简短，时而刻薄。"))
  ;; (sthenno/gptel-setup-openai)

  (defun sthenno/gptel-setup-openai ()
    (setq gptel-model 'gpt-4.1
          gptel-backend (gptel-make-openai "gpt-4.1"
                          :models '(gpt-4.1)
                          :stream t
                          :key (gptel-api-key-from-auth-source "api.openai.com"))))
  (sthenno/gptel-setup-openai)

  ;; Generation options
  (setq gptel-max-tokens (* 8 1024)
        gptel-temperature 0.75)

  (add-hook 'gptel-mode-hook #'(lambda ()
                                 (setq-local gptel-max-tokens (* 8 1024)
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
  (setq gptel-tools
        `(,(gptel-make-tool :name "read_file"
                            :function (lambda (filepath)
                                        (unless (file-exists-p filepath)
                                          (error
                                           "error: file %s does not exist." filepath))
                                        (with-temp-buffer
                                          (insert-file-contents filepath)
                                          (buffer-string)))
                            :description "Read the contents of a file"
                            :args `((
                                     :name "filepath"
                                     :type string
                                     :description "The path to the file to be read"))
                            :category "filesystem")
          ,(gptel-make-tool :name "create_file"
                            :function
                            (lambda (path filename content)
                              (let ((full-path (expand-file-name filename path)))
                                (with-temp-buffer
                                  (insert content)
                                  (write-file full-path))
                                (format "Created file %s in %s" filename path)))
                            :description "Create a new file with the specified content"
                            :args `((
                                     :name "path"
                                     :type string
                                     :description "The directory where to create the file")
                                    (
                                     :name "filename"
                                     :type string
                                     :description "The name of the file to create")
                                    (
                                     :name "content"
                                     :type string
                                     :description "The content to write to the file"))
                            :category "filesystem")))

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

  (defun sthenno/gptel-send (&optional arg)
    (interactive "P")
    (if (and arg (require 'gptel-transient nil t))
        (call-interactively #'gptel-menu)
      (when gptel-mode
        (goto-char (point-max)))
      (message "􀕻 正在联系 %s…" ;; (gptel--model-name gptel-model)
               (gptel-backend-name gptel-backend))
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
