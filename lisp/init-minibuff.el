;;; init-minibuff.el --- Config for minibuffer completion -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:

(use-package vertico
  :straight (:files (:defaults "extensions/*")
                    :includes (vertico-directory))
  :custom
  (vertico-count 6)
  (vertico-cycle t)
  :config
  (vertico-mode 1)
  (use-package vertico-directory
		:bind
		((:map vertico-map
				   ("RET" . vertico-directory-enter)
				   ("DEL" . vertico-directory-delete-char)
				   ("M-DEL" . vertico-directory-delete-word)))
		:hook ; Correct file path when changed
    (rfn-eshadow-update-overlay . vertico-directory-tidy))

  ;; Persist history over Emacs restarts.
  (use-package savehist
    :config
    (savehist-mode 1)))

(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion))
  (completion-category-defaults nil)
  (completion-cycle-threshold 3))

(use-package consult
  :config
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line)
  :bind
  (("C-s" . consult-line)
	 ("M-s" . consult-ripgrep)
	 ("s-b" . consult-buffer)))


(provide 'init-minibuff)
;;; init-minibuff.el ends here
