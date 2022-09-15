;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
