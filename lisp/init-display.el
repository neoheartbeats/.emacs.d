;;; init-display.el --- Emacs Dashboard setup -*- lexical-binding: t -*-
;;; Commentary:

;; An extensible Emacs startup screen showing you what’s most important.
;; This is where the game starts.

;;; Code:

(use-package projectile)

(use-package dashboard
  :custom
  (dashboard-banner-logo-title "欢迎来到童话库!")
  (dashboard-startup-banner (concat (expand-file-name "resources/" user-emacs-directory)
                                    "intro.png"))
  (dashboard-center-content t)
  (dashboard-set-navigator t)
  (dashboard-show-shortcuts nil)
  (dashboard-set-footer nil)
  (dashboard-items '((recents  . 3)
                     (bookmarks . 3)
                     (projects . 3)))
  (dashboard-item-names '(("Recent Files:" . "􀐫 Recents")
                          ("Bookmarks:" . "􀊴 Bookmarks")
                          ("Registers:" . "􀉩 Registers")
                          ("Projects:" . "􀤞 Projects")
                          ("Agenda for today:" . "􀉉 Today")
                          ("Agenda for the coming week:" . "􀧞 Schedule")))
  :config
  (dashboard-setup-startup-hook)

  ;; Buttons
  (setq dashboard-navigator-buttons
        `((("􀉊" "Today" "Go to Today" (lambda (&rest _)
                                        (interactive)
                                        (org-roam-dailies-goto-today)
                                        (goto-char (point-max)))
            `warning "[" "]")
           ("􀈟" "Mission" "Visit my missions" (lambda (&rest _)
                                                (interactive)
                                                (org-agenda-list))
            `warning "[" "]")))))


(provide 'init-display)
;;; init-display.el ends here