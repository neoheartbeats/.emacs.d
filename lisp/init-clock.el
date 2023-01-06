;;; init-clock.el --- Magics in the time -*- lexical-binding: t -*-
;;; Commentary:

;; This file configures Org Agenda, so it should be loaded after loading
;; the configuration of Org mode.

;;; Code:

(require 'org)


;; Org Agenda
(define-key global-map (kbd "C-c a") #'org-agenda)

(setq org-agenda-files '("~/Developer/TH18-03/dates/"
                         "~/Developer/TH18-03/notes/"))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-edit-timestamp-down-means-later t)
(setq org-export-coding-system 'utf-8)
(setq org-export-kill-product-buffer-when-displayed t)
(setq org-html-validation-link nil)
(setq org-fast-tag-selection-single-key 'expert)

;; Appearances
(setq org-auto-align-tags nil)
(setq org-catch-invisible-edits 'show-and-error)
(setq org-agenda-block-separator ?─)
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ───── " "───────────────"))
(setq org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────")
(setq org-agenda-block-separator nil)
(setq org-tags-colum 0)


(provide 'init-clock)
;;; init-clock.el ends here
