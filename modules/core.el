;; core.el --- Credits: loading first -*- lexical-binding: t -*-
;;
;; Copyright © 2022 Ilya.w
;;
;; Author: Ilya.w <ilya.w@icloud.com>
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; macOS bootstrap.
;;
;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default lexical binding
(setq-default lexical-binding t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compile external packages for GccEmacs
(setq package-native-compile t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; macOS key mapping
(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Font ligatures support
;; (setq mac-auto-operator-composition-characters "!\"#$%&'()*+,-./:<=>?@[\\]^_`{|}~")
;; (mac-auto-operator-composition-mode 1)

(provide 'core)
