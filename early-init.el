;;; early-init.el --- Emacs 27+ pre-initialisation config
;;; Commentary:

;; Code loaded before the package system and GUI is initialized.

;;; Code:

;; Prevent `package.el' loading packages prior to their init-file loading
;; This procedure is related to
;; https://github.com/radian-software/straight.el/
(setq package-enable-at-startup nil)


;;; Config related to GccEmacs
;; Prevent unwanted runtime compilation for GccEmacs
(setq native-comp-deferred-compilation nil)

;; Compile external packages for GccEmacs
(setq package-native-compile t)


(provide 'early-init)
;;; early-init.el ends here
