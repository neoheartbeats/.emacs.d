;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Package management
;;
;; Determine package loading sources
(require 'package)
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap "use-package"
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Default to ensure installing packages
(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/modules/")
(require 'core)
(require 'defaults)
(require 'ui)
(require 'init-org)
(require 'enhance)

(setq custom-file "~/.emacs.d/modules/custom.el")
(load custom-file 'no-error 'no-message)

(setq py-python-command "python3"
      python-shell-interpreter "python3"
      org-babel-python-command "python3"
      python-indent-guess-indent-offset-verbose nil)

(use-package vterm-toggle
  :init
  (use-package vterm)
  :custom
  (vterm-toggle-fullscreen-p nil)
  :config
  (add-to-list 'display-buffer-alist
	       '((lambda (b _)
		   (with-current-buffer b
		     (equal major-mode
			    'vterm-mode)))
		 (display-buffer-reuse-window
		  display-buffer-at-bottom)
		 (reusable-frames . visible)
		 (window-height . 0.3)))
  :bind
  ("M-`" . vterm-toggle-cd))

(use-package emms
  :init
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  :bind
  ("s-p s" . (lambda ()
	       (interactive)
	       (emms-add-directory "~/org/Emms/")
	       (emms-shuffle)
	       (emms-start)))
  ("s-p n" . emms-next)
  ("s-p p" . emms-previous)
  ("s-p t" . emms-stop))
