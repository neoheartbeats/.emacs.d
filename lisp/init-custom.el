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

(defgroup centaur nil
  "Pes Emacs distribution."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/ilyaw39/.emacs.d"))

(defcustom pes-completion-style 'childframe
  "Completion display style."
  :group 'pes
  :type
  '(choice
    (const :tag "Minibuffer" minibuffer) (const :tag "Child Frame" childframe)))

(defcustom pes-prog-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-" . ?←)
    ("->" . ?→)
    ("->>" . ?↠)
    ("=>" . ?⇒)
    ("map" . ?↦)
    ("/=" . ?≠)
    ("!=" . ?≠)
    ("==" . ?≡)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("=<<" . (?= (Br . Bl) ?≪))
    (">>=" . (?≫ (Br . Bl) ?=))
    ("<=<" . ?↢)
    (">=>" . ?↣)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("not" . ?¬))
  "A list of symbol prettifications.
Nil to use font supports ligatures."
  :group 'pes
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom pes-org-prettify-symbols-alist
  '(("[ ]" . ?)
    ("[-]" . ?)
    ("[X]" . ?)

    (":PROPERTIES:" . ?)
    (":ID:" . ?🪪)
    (":END:" . ?🔚)

    ("#+ARCHIVE:" . ?📦)
    ("#+AUTHOR:" . ?👤)
    ("#+CREATOR:" . ?💁)
    ("#+DATE:" . ?📆)
    ("#+DESCRIPTION:" . ?⸙)
    ("#+EMAIL:" . ?📧)
    ("#+HEADERS" . ?☰)
    ("#+OPTIONS:" . ?⚙)
    ("#+SETUPFILE:" . ?⚒)
    ("#+TAGS:" . ?🏷)
    ("#+TITLE:" . ?📓)

    ("#+BEGIN_SRC" . ?✎)
    ("#+END_SRC" . ?□)
    ("#+BEGIN_QUOTE" . ?«)
    ("#+END_QUOTE" . ?»)
    ("#+RESULTS:" . ?💻))
  "A list of symbol prettifications for `org-mode'."
  :group 'pes
  :type '(alist :key-type string :value-type (choice character sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-custom.el ends here
