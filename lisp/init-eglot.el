;; init-eglot.el --- LSP support by Eglot -*- lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup `treesit'
;;
;; Command `treesit-auto-install-all' is required if the tree-sitter grammar
;; libs have not been configured already
(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode 1))

;; To enable the maximum fontifications
(setq treesit-font-lock-level 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initialize `eglot'
;; (use-package eglot
;;   :straight t
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
;;   (add-hook 'python-mode-hook #'eglot-ensure)
;;   (add-hook 'python-ts-mode-hook #'eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python
;;
;; TODO
(setq python-shell-interpreter "/Library/Frameworks/Python.framework/Versions/3.12/bin/python3"
      python-shell-interpreter-args "-i"
      python-shell--interpreter python-shell-interpreter
      python-shell--interpreter-args python-shell-interpreter-args
      python-shell-prompt-detect-failure-warning nil
      python-shell-completion-native-enable nil)
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; TODO
;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :config
;;   (add-hook 'prog-mode-hook #'(lambda ()
;;                                 (copilot-mode 1)))
;;   (define-key copilot-completion-map (kbd "M-.") #'copilot-accept-completion))

;; Reformat python buffers using the `black' formatter
;; (use-package blacken
;;   :straight t
;;   :config

;;   ;; Auto reformat the buffer after saving
;;   (add-hook 'python-ts-mode-hook #'(lambda ()
;;                                      (blacken-mode 1)))
;;   :hook
;;   (after-save . blacken-buffer)
;;   :bind
;;   (:map python-ts-mode-map
;;         ("s-i" . blacken-buffer)))

(provide 'init-eglot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-eglot.el ends here
