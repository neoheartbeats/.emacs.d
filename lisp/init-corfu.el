;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:

(setq tab-always-indent 'complete)

(use-package corfu
  :straight (:files (:defaults "extensions/*")
              :includes (corfu-history))
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  :config
  
  ;; Remember the latest choice
  (use-package corfu-history
		:config
    (corfu-history-mode 1))

  ;; Icon support
	;; (use-package kind-icon
	;; 	:custom
	;; 	(kind-icon-default-face 'corfu-default)
	;; 	:config
	;; 	(add-to-list 'corfu-margin-formatters
  ;;     #'kind-icon-margin-formatter))
  :bind
  (:map corfu-map
    ("<down>" . corfu-next)
    ("<escape>" . corfu-quit))
  :hook
	((prog-mode org-mode) . corfu-mode))


;; Add extensions
(use-package cape
  :init
  
  ;; Add `completion-at-point-functions', used by `completion-at-point'
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell))


(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-cycle-threshold 3))


(provide 'init-corfu)
;;; init-corfu.el ends here
