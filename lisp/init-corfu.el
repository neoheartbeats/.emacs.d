;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:

(setq tab-always-indent 'complete)

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match 'separator)
  (corfu-cycle t)
  :config
  ;; Add icon support
  (use-package kind-icon
    :after corfu
    :custom
    (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  ;; Remember the latest choice
  (use-package corfu-history
    :straight nil
    :config
    (corfu-history-mode 1))
  :bind
  (:map corfu-map
        ("<down>" . corfu-next)
        ("TAB" . corfu-quit)
        ("<escape>" . corfu-quit))
  :hook
  ((prog-mode org-mode) . corfu-mode))


;; Setup `cape-dict-file'
(setq cape-dict-file
      (expand-file-name "dict/words.txt" user-emacs-directory))


;; Add extensions
(use-package cape
  :init ;; Add `completion-at-point-functions', used by `completion-at-point'
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles . (partial-completion))))))


(provide 'init-corfu)
;;; init-corfu.el ends here
