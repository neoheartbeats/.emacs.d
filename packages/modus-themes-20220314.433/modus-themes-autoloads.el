;;; modus-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "modus-themes" "modus-themes.el" (0 0 0 0))
;;; Generated autoloads from modus-themes.el

(autoload 'modus-themes-contrast "modus-themes" "\
Measure WCAG contrast ratio between C1 and C2.
C1 and C2 are color values written in hexadecimal RGB.

\(fn C1 C2)" nil nil)

(autoload 'modus-themes-color "modus-themes" "\
Return color value for COLOR from current palette.
COLOR is a key in `modus-themes-operandi-colors' or
`modus-themes-vivendi-colors'.

\(fn COLOR)" nil nil)

(autoload 'modus-themes-color-alts "modus-themes" "\
Return color value from current palette.
When Modus Operandi is enabled, return color value for color
LIGHT-COLOR.  When Modus Vivendi is enabled, return color value
for DARK-COLOR.  LIGHT-COLOR and DARK-COLOR are keys in
`modus-themes-operandi-colors' or `modus-themes-vivendi-colors'.

\(fn LIGHT-COLOR DARK-COLOR)" nil nil)

(autoload 'modus-themes-load-themes "modus-themes" "\
Ensure that the Modus themes are in `custom-enabled-themes'.

This function is intended for use in package declarations such as
those defined with the help of `use-package'.  The idea is to add
this function to the `:init' stage of the package's loading, so
that subsequent calls that assume the presence of a loaded theme,
like `modus-themes-toggle' or `modus-themes-load-operandi', will
continue to work as intended even if they are lazy-loaded (such
as when they are declared in the `:config' phase)." nil nil)

(autoload 'modus-themes-load-operandi "modus-themes" "\
Load `modus-operandi' and disable `modus-vivendi'.
Also run `modus-themes-after-load-theme-hook'." t nil)

(autoload 'modus-themes-load-vivendi "modus-themes" "\
Load `modus-vivendi' and disable `modus-operandi'.
Also run `modus-themes-after-load-theme-hook'." t nil)

(autoload 'modus-themes-toggle "modus-themes" "\
Toggle between `modus-operandi' and `modus-vivendi' themes.
Also runs `modus-themes-after-load-theme-hook' at its last stage
by virtue of calling either of `modus-themes-load-operandi' and
`modus-themes-load-vivendi' functions." t nil)

(when load-file-name (let ((dir (file-name-directory load-file-name))) (unless (equal dir (expand-file-name "themes/" data-directory)) (add-to-list 'custom-theme-load-path dir))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "modus-themes" '("modus-themes-")))

;;;***

;;;### (autoloads nil nil ("modus-operandi-theme.el" "modus-themes-pkg.el"
;;;;;;  "modus-vivendi-theme.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; modus-themes-autoloads.el ends here
