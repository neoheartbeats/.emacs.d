(setq inhibit-startup-message t)
(setq visible-bell t)
(setq initial-scratch-message nil)
(setq use-dialog-box nil)

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)

(setq-default cursor-type '(bar . 1))
(setq frame-title-format '("GccEmacs@28.0.91 _Credits"))

(fset 'yes-or-no-p 'y-or-n-p)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(global-visual-line-mode 1)
(delete-selection-mode 1)

(mac-auto-operator-composition-mode 1)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'no-error 'no-message)

(defun display-startup-echo-area-message ()
  (message "Funding for this program is made possible by viewers like you."))

(setq-default mode-line-format nil)

(setq default-frame-alist
      '((top . 0)
	(width . 100)
	(height . 50)))

(set-face-attribute 'default nil
		    :font "FIRA CODE"
		    :height 150)

(set-face-attribute 'variable-pitch nil
		    :font "Noto Serif CJK SC"
		    :height 150)

(set-fontset-font t 'han "Noto Serif CJK SC")

(global-set-key (kbd "s-a")
		'mark-whole-buffer)
(global-set-key (kbd "s-c")
		'kill-ring-save)
(global-set-key (kbd "s-s")
		'save-buffer)
(global-set-key (kbd "s-v")
		'yank)
(global-set-key (kbd "s-z")
		'undo)
(global-set-key (kbd "s-x")
		'kill-region)
(global-set-key (kbd "s-w")
		(lambda ()
		  (interactive)
		  (kill-buffer (current-buffer))))
(global-set-key (kbd "<s-right>")
		'next-buffer)
(global-set-key (kbd "<s-left>")
		'previous-buffer)
(global-set-key (kbd "s-e")
		'delete-window)
(global-set-key (kbd "s-q")
		'save-buffers-kill-emacs)
(global-set-key (kbd "s-n")
		'find-file)
(global-set-key (kbd "<escape>")
		'keyboard-escape-quit)
(global-set-key (kbd "C-c p")
		(lambda ()
		  (interactive)
		  (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x p")
		(lambda ()
		  (interactive)
		  (org-latex-preview)
		  (org-display-inline-images)))
(global-set-key (kbd "s-i")
		'indent-region)
(global-set-key (kbd "s-m")
		'toggle-frame-fullscreen)

(require 'org)

(setq org-directory "~/org/"
      org-ellipsis " ▼"
      org-insert-heading-respect-content nil
      org-image-actual-width nil
      org-confirm-elisp-link-function nil
      org-confirm-babel-evaluate nil
      org-insert-heading-respect-content t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts nil
      org-src-preserve-indentation t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-startup-indented t
      org-startup-with-inline-images t
      org-startup-with-latex-preview t
      org-preview-latex-default-process 'imagemagick)

(add-hook 'org-mode-hook 'org-hide-drawer-all)

(setq org-latex-preview-ltxpng-directory "~/.emacs.d/ltximg/")

(setq org-preview-latex-process-alist
      '((imagemagick
	 :programs
	 ("latex" "convert")
	 :description "pdf > png"
	 :image-input-type "pdf"
	 :image-output-type "png"
	 :image-size-adjust
	 (1.0 . 1.0)
	 :latex-compiler
	 ("xelatex -interaction nonstopmode -output-directory %o %f")
	 :image-converter
	 ("convert -density %D -trim -antialias %f -quality 100 %O"))))

(plist-put org-format-latex-options :scale 1.0)

(setq org-latex-packages-alist
      '(("" "physics" t)
	("" "mhchem" t)
        ("" "mathtools" t)
	("" "gensymb" t)
	("" "notomath" t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (octave . t)
   (scheme . t)
   (emacs-lisp . t)
   (python . t)))

(setq py-python-command "python3"
      python-shell-interpreter "python3"
      org-babel-python-command "python3"
      python-indent-guess-indent-offset-verbose nil)

(defun prog-icons ()
  (setq prettify-symbols-alist
	'(("lambda" . "λ")))
  (prettify-symbols-mode))
(add-hook 'prog-mode-hook 'prog-icons)

(defun org-icons ()
  (setq prettify-symbols-alist
	'(("lambda" . "λ")
	  (":PROPERTIES:" . "≡")
	  (":ID:" . "i")
	  (":END:" . "-")
	  ("#+TITLE:" . "T")
          ("#+title:" . "T")
	  ("#+begin_src" . "»")
	  ("#+end_src" . "»")
          ("#+RESULTS:" . ":")
          ("#+attr_org:" . "⌘")
	  (":ROAM_ALIASES:" . "@")))
  (prettify-symbols-mode))

(add-hook 'org-mode-hook 'org-icons)

(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 ()
	 (compose-region
	  (match-beginning 1)
	  (match-end 1) "•"))))))

(add-hook 'prog-mode-hook 'linum-mode)

(require 'package)
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package company
  :init
  (global-company-mode 1)
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  :bind
  (:map company-active-map
	("<escape>" . company-abort)
	("C-p" . company-select-previous)
	("C-n" . company-select-next))
  (:map company-search-map
	("<escape>" . company-search-abort)
	("C-p" . company-select-previous)
	("C-n" . company-select-next)))

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  (org-mode . smartparens-mode)
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0))

(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless)))

(use-package consult
  :bind
  (("C-s" . consult-line)
   ("M-s i" . consult-imenu)
   ("M-s g" . consult-grep)))

(use-package magit)

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/")
  (org-roam-dailies-directory "dLs/")
  (org-roam-completion-everywhere t)
  :bind
  ("C-c n n" . org-id-get-create)
  ("C-c n a" . org-roam-alias-add)
  ("C-c n f" . org-roam-node-find)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n j" . org-roam-dailies-goto-today)
  ("<s-up>" . org-roam-dailies-goto-previous-note)
  ("<s-down>" . org-roam-dailies-goto-next-note)
  :config
  (org-roam-setup)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
		 (display-buffer-in-side-window)
		 (side . right)
		 (slot . 0)
		 (window-width . 0.42)
		 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  (defun display-backlink-latex (node)
    (org--latex-preview-region (point-min) (point-max)))
  (eval-after-load 'org-roam
    (advice-add 'org-roam-reflinks-section
		:after #'display-backlink-latex))
  :hook
  (after-init . org-roam-dailies-goto-today)
  (org-roam-backlinks-mode . visual-line-mode))

(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("▼")))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("🔴" "🟡" "🟢")))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :custom
  (yas-triggers-in-field t))

(use-package powerthesaurus
  :bind
  ("M-p" . powerthesaurus-lookup-synonyms-dwim))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package modus-themes
  :init
  (modus-themes-load-themes)
  (modus-themes-load-vivendi)
  :custom
  (modus-themes-variable-pitch-ui t)
  (modus-themes-mode-line '(borderless))
  (modus-themes-syntax '(yellow-comments green-strings))
  (modus-themes-completions 'opinionated)
  (modus-themes-hl-line '(underline))
  (modus-themes-paren-match '(bold intense))
  (modus-themes-links '(neutral-underline))
  (modus-themes-box-buttons '(variable-pitch 0.9))
  (modus-themes-prompts '(intense background)))

(use-package dirvish
  :init
  (use-package dired
    :custom
    (dired-recursive-deletes 'always)
    (delete-by-moving-to-trash t)
    (dired-dwim-target t)))

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
