;; init-custom.el --- Define customizations -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Ilya Wang
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;;
;; Definitions for custom-vars.
;;
;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup pes nil
  "Pes Emacs distribution."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/ilyaw39/.emacs.d"))


(defvar pes-logo "../resources/2A317673-5ACF-4C0D-94D9-0D27D3808800.png")

(defvar pes-home-path "/Users/ilyaw39/")
(defvar pes-dev-path "/Users/ilyaw39/Developer/")
(defvar pes-org-path "/Users/ilyaw39/Developer/LutwidgeTown/")

;; Homebrew specified path
(defvar pes-hb-bin-path "/opt/homebrew/bin/")
(defvar pes-hb-room-path "/opt/homebrew/Caskroom/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(provide 'init-custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-custom.el ends here
