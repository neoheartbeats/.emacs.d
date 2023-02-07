;;; init-utils.el --- Elisp helper functions -*- lexical-binding: t -*-
;;; Commentary:

;; This file is inspired by https://github.com/purcell/emacs.d/.

;;; Code:


;; String utilities
(defun pes-string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR',
returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;; Delete the current file
(defun pes-delete-this-file ()
  "Delete the current file, then kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p
         (format "Confirm delete '%s'?"
                 (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Rename the current file
(defun pes-rename-this-file-and-buffer (new-name)
  "Rename both current buffer and file to NEW-NAME."
  (interactive "File new name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


;; ERC & GNUS
(setq user-mail-address "ilyaw3939@gmail.com")
(setq user-full-name "ilyaw3939")

(setq message-send-mail-function 'smtpmail-send-it)
(setq gnus-secondary-select-methods '((nntp "news.gwene.org")))
(setq gnus-select-method
      '(nnimap
        "gmail"
        (nnimap-address "imap.gmail.com")
        (nnimap-server-port 993)
        (nnimap-stream ssl)))

(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)

(setq erc-server-coding-system '(utf-8 . utf-8))
(setq erc-interpret-mirc-color t)
(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-kill-server-buffer-on-quit t)


(provide 'init-utils)
;;; init-utils.el ends here
