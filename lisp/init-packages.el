;;; init-packages.el --- Settings for `package.el' -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'package)

(setq package-name-column-width 40)
(setq package-version-column-width 14)
(setq package-status-column-width 12)
(setq package-archive-column-width 8)

(add-hook 'package-menu-mode-hook #'hl-line-mode)


;; Install into separate package dirs for each Emacs version
;; to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s"
                                emacs-major-version
                                emacs-minor-version)
                        user-emacs-directory))


;; Standard package repositories
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("org" . "https://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("elpa" . 2)
        ("nongnu" . 1)))

;; I want to use my own packages from specific repositories.  All
;; others will rely on `package-archive-priorities'.  I do this to
;; test that the packaged version works as intended.
;;
;; Note that the `modus-themes' are built into Emacs 28 and are synced
;; to GNU ELPA from emacs.git.  As I already run Emacs from source, I am
;; using MELPA for the `modus-themes' here: it is for testing purposes.
(setq package-pinned-packages
      '((agitate . "elpa-devel")
        (altcaps . "elpa-devel")
        (cursory . "elpa-devel")
        (denote . "elpa-devel")
        (ef-themes . "elpa-devel")
        (fontaine . "elpa-devel")
        (lin . "elpa-devel")
        (logos . "elpa-devel")
        (modus-themes . "melpa")
        (notmuch-indicator . "elpa-devel")
        (pulsar . "elpa-devel")
        (standard-themes . "elpa-devel")
        (substitute . "elpa-devel")
        (sxhkdrc-mode . "elpa-devel")
        (tmr . "elpa-devel")))

(setq custom-safe-themes t)


(defmacro prot-emacs-builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

Ignore PACKAGE if it is a member of `prot-emacs-omit-packages'."
  (declare (indent 1))
  `(progn
     (unless (and (not (memq ,package prot-emacs-omit-packages))
                  (require ,package nil 'noerror))
       (display-warning 'prot-emacs
                        (format "Loading `%s' failed" ,package)
                        :warning))
     ,@body))

(defmacro prot-emacs-elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

Try to install the PACKAGE if it is missing.

Ignore PACKAGE, including the step of installing it, if it is a
member of `prot-emacs-omit-packages'."
  (declare (indent 1))
  `(unless (memq ,package prot-emacs-omit-packages)
     (progn
       (when (not (package-installed-p ,package))
         (unless package-archive-contents
           (package-refresh-contents))
         (package-install ,package))
       (if (require ,package nil 'noerror)
           (progn ,@body)
         (display-warning 'prot-emacs
                          (format "Loading `%s' failed" ,package)
                          :warning)))))

(defmacro prot-emacs-vc-package (package remote &rest body)
  "Set up PACKAGE from its REMOTE source.
REMOTE is a plist that specifies:

- :url     A string pointing to the URL of the PACKAGE source.
           This is required.

- :branch  The branch to build from.  This is optional.  It
            defaults to the REMOTE's main branch.

BODY is the configuration associated with PACKAGE."
  (declare (indent 1))
  `(unless (memq ,package prot-emacs-omit-packages)
     (progn
       (when (not (package-installed-p ,package))
         (package-vc-install
          (cons ,package (list :url ,(plist-get remote :url)
                               ,@(when-let ((b (plist-get remote :branch)))
                                   (list :branch b))))))
       (if (require ,package nil 'noerror)
           (progn ,@body)
         (display-warning 'prot-emacs
                          (format "Loading `%s' failed" ,package)
                          :warning)))))

(defvar prot-emacs-package-form-regexp
  "^(\\(prot-emacs-.*-package\\|require\\) +'\\([0-9a-zA-Z-]+\\)"
  "Regexp to add packages to `lisp-imenu-generic-expression'.")

(eval-after-load 'lisp-mode
  `(add-to-list 'lisp-imenu-generic-expression
                (list "Packages" ,prot-emacs-package-form-regexp 2)))


;; The purpose of this file is for the user to define their
;; preferences BEFORE loading any of the modules.  For example, the
;; user option `prot-emacs-omit-packages' lets the user specify which
;; packages not to load.  Search for all `defcustom' forms in this
;; file for other obvious customisations.
(when-let* ((file (locate-user-emacs-file "prot-emacs-pre-custom.el"))
            ((file-exists-p file)))
  (load-file file))


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

(defvar pes-required-packages nil)

(defun pes-note-selected-package (oldfun package &rest args)
  "If OLDFUN reports PACKAGE was successfully installed, note that fact.
The package name is noted by adding it to
`pes-required-packages'. This function is used as an
advice for `require-package', to which ARGS are passed."
  (let ((available (apply oldfun package args)))
    (prog1
        available
      (when available
        (add-to-list 'pes-required-packages package)))))

(advice-add 'require-package :around #'pes-note-selected-package)

(when (fboundp 'package--save-selected-packages)
  (require-package 'seq)
  (add-hook 'after-init-hook
            #'(lambda ()
                (package--save-selected-packages
                 (seq-uniq (append pes-required-packages
                                   package-selected-packages))))))


(require-package 'fullframe)
(fullframe list-packages quit-window)


(let ((package-check-signature nil))
  (require-package 'gnu-elpa-keyring-update))


(defun pes-set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun pes-maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (pes-set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max
                                       (mapcar 'length
                                               (mapcar 'car
                                                       package-archives)))))
      (pes-set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook #'pes-maybe-widen-package-menu-columns)


(provide 'init-packages)
;;; init-packages.el ends here
