;;; init-packages.el --- Settings for `package.el' -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)


;; Install into separate package dirs for each Emacs version
;; to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s"
                                emacs-major-version
                                emacs-minor-version)
                        user-emacs-directory))


;; Standard package repositories
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(push '("org" . "https://orgmode.org/elpa/") package-archives)


;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (when (stringp min-version)
    (setq min-version (version-to-list min-version)))
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car
                    (sort known
                          (lambda (lo hi)
                            (version-list-<= (package-desc-version hi)
                                             (package-desc-version lo)))))))
        (if (and best (version-list-<= min-version
                                       (package-desc-version best)))
            (package-install best)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t)))
        (package-installed-p package min-version))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))


;; Fire up `package.el'
(setq package-enable-at-startup nil)
(package-initialize)


;; `package.el' updates the saved version of `package-selected-packages'
;; correctly only after custom-file has been loaded, which is a bug.
;; We work around this by adding the required packages
;; to `package-selected-packages' after startup is complete.

(defvar my/required-packages nil)

(defun my/note-selected-package (oldfun package &rest args)
  "If OLDFUN reports PACKAGE was successfully installed, note that fact.
The package name is noted by adding it to
`my/required-packages'. This function is used as an
advice for `require-package', to which ARGS are passed."
  (let ((available (apply oldfun package args)))
    (prog1
        available
      (when available
        (add-to-list 'my/required-packages package)))))

(advice-add 'require-package :around #'my/note-selected-package)

(when (fboundp 'package--save-selected-packages)
  (require-package 'seq)
  (add-hook 'after-init-hook
            #'(lambda ()
                (package--save-selected-packages
                 (seq-uniq (append my/required-packages
                                   package-selected-packages))))))


(require-package 'fullframe)
(fullframe list-packages quit-window)


(let ((package-check-signature nil))
  (require-package 'gnu-elpa-keyring-update))


(defun my/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun my/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (my/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max
                                       (mapcar 'length
                                               (mapcar 'car
                                                       package-archives)))))
      (my/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook #'my/maybe-widen-package-menu-columns)


(provide 'init-packages)
;;; init-packages.el ends here
