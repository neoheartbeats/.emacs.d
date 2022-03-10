;; init-lisp.el --- Lyrith: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Lisp mode setup.
;;
;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs-lisp mode
;;
;; Turning off ligatures makes lisp code more orderly
(add-hook 'emacs-lisp-mode-hook
          (lambda () (mac-auto-operator-composition-mode -1)))

(provide 'init-lisp)

;; init-lisp.el ends here
