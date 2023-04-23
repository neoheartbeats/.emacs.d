;;; org-latex-preview.el --- LaTeX previews for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Keywords: tex, extensions, tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  LaTeX previews for Org

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-compat)
(require 'ox-latex)

(declare-function org-persist-read "org-persist")
(declare-function org-persist-register "org-persist")
(declare-function org-persist-unregister "org-persist")

(defvar org-src-mode-hook nil)
(defvar org-src--beg-marker nil)

(defgroup org-latex-preview nil
  "Options for generation of LaTeX previews in Org mode."
  :tag "Org LaTeX Preview"
  :group 'org)

;;;###autoload
(defcustom org-latex-preview-options
  '(:foreground auto :background "Transparent" :scale 1.0
    :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
    :zoom 1.0)
  "Options for creating images from LaTeX fragments.
This is a property list with the following properties:
:foreground  the foreground color for images embedded in Emacs, e.g. \"Black\".
             `default' means use the foreground of the default face.
             `auto' means use the foreground from the text face.
:background  the background color, or \"Transparent\".
             `default' means use the background of the default face.
             `auto' means use the background from the text face.
:scale       a scaling factor for the size of the images, to get more pixels
:matchers    a list indicating which matchers should be used to
             find LaTeX fragments.  Valid members of this list are:
             \"begin\" find environments
             \"$1\"    find single characters surrounded by $.$
             \"$\"     find math expressions surrounded by $...$
             \"$$\"    find math expressions surrounded by $$....$$
             \"\\(\"    find math expressions surrounded by \\(...\\)
             \"\\=\\[\"    find math expressions surrounded by \\=\\[...\\]
:zoom        when the image has associated font-relative height information,
             the display size is scaled by this factor."
  :group 'org-latex-preview
  :package-version '(Org . "9.7")
  :type 'plist)

(defcustom org-latex-preview-default-process
  (if (executable-find "dvisvgm") 'dvisvgm 'dvipng)
  "The default process to convert LaTeX fragments to image files.
All available processes and theirs documents can be found in
`org-latex-preview-process-alist', which see."
  :group 'org-latex-preview
  :package-version '(Org . "9.7")
  :type 'symbol)

;;;###autoload
(defcustom org-latex-preview-process-alist
  '((dvipng
     :programs ("latex" "dvipng")
     :description "dvi > png"
     :message "you need to install the programs: latex and dvipng."
     :image-input-type "dvi"
     :image-output-type "png"
     :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
     :latex-precompiler ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
     :image-converter ("dvipng --follow -D %D -T tight --depth --height -o %B-%%09d.png %f")
     :transparent-image-converter
     ("dvipng --follow -D %D -T tight -bg Transparent --depth --height -o %B-%%09d.png %f"))
    (dvisvgm
     :programs ("latex" "dvisvgm")
     :description "dvi > svg"
     :message "you need to install the programs: latex and dvisvgm."
     :image-input-type "dvi"
     :image-output-type "svg"
     :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
     :latex-precompiler ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
     ;; With dvisvgm the --bbox=preview flag is needed to emit the preview.sty-provided
     ;; height+width+depth information. The --optimise, --clipjoin, and --relative flags
     ;; cause dvisvgm do do some extra work to tidy up the SVG output, but barely add to
     ;; the overall dvisvgm runtime (<1% increace, from testing).
     :image-converter ("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -o %B-%%9p.svg %f"))
    (imagemagick
     :programs ("pdflatex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: latex and imagemagick."
     :image-input-type "pdf"
     :image-output-type "png"
     :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
     :latex-precompiler ("pdftex -output-directory %o -ini -jobname=%b \"&pdflatex\" mylatexformat.ltx %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %B-%%09d.png")))
  "Definitions of external processes for LaTeX previewing.
Org mode can use some external commands to generate TeX snippet's images for
previewing or inserting into HTML files, e.g., \"dvipng\".  This variable tells
`org-latex-preview-create-image' how to call them.

The value is an alist with the pattern (NAME . PROPERTIES).  NAME is a symbol.
PROPERTIES accepts the following attributes:

  :programs           list of strings, required programs.
  :description        string, describe the process.
  :message            string, message it when required programs cannot be found.
  :image-input-type   string, input file type of image converter (e.g., \"dvi\").
  :image-output-type  string, output file type of image converter (e.g., \"png\").
  :post-clean         list of strings, files matched are to be cleaned up once
                      the image is generated.  When nil, the files with \".dvi\",
                      \".xdv\", \".pdf\", \".tex\", \".aux\", \".log\", \".svg\",
                      \".png\", \".jpg\", \".jpeg\" or \".out\" extension will
                      be cleaned up.
  :latex-header       list of strings, the LaTeX header of the snippet file.
                      When nil, the fallback value is used instead, which is
                      controlled by `org-latex-preview-preamble',
                      `org-latex-default-packages-alist' and
                      `org-latex-packages-alist', which see.
  :latex-compiler     list of LaTeX commands, as strings.  Each of them is given
                      to the shell.  Place-holders \"%t\", \"%b\" and \"%o\" are
                      replaced with values defined below.
  :image-converter    list of image converter commands strings.  Each of them is
                      given to the shell and supports any of the following
                      place-holders defined below.

If set, :transparent-image-converter is used instead of :image-converter to
convert an image when the background color is nil or \"Transparent\".

Place-holders used by `:image-converter', `:latex-precompiler',
and `:latex-compiler':

  %f    input file name
  %b    base name of input file
  %o    base directory of input file
  %O    absolute output file name

Place-holders only used by `:latex-precompiler' and `:latex-compiler':

  %l   LaTeX compiler command string
  %L   LaTeX compiler command name

Place-holders only used by `:image-converter':

  %D    dpi, which is used to adjust image size by some processing commands."
  :group 'org-latex-preview
  :package-version '(Org . "9.7")
  :type '(alist :tag "LaTeX to image backends"
          :value-type (plist)))

(defcustom org-latex-preview-compiler-command-map
  '(("pdflatex" . "latex")
    ("xelatex" . "xelatex -no-pdf")
    ("lualatex" . "dvilualatex"))
  "An alist mapping from each of `org-latex-compilers' to command strings.
Each key is a LaTeX compiler name, for each compiler in
`org-latex-compilers', and the value the command that should be used
when producing a preview (optionally including flags).

This should only ever be changed in the event that PDF, not DVI output
is required."
  :group 'org-latex-preview
  :package-version '(Org . "9.7")
  :type '(alist :tag "Compiler"
          :value-type (string :type "command")))

(defcustom org-latex-preview-persist t
  "Persist produced LaTeX previews across Emacs sessions.

When non-nil, org-persist is used to cache the fragments and
data.  Otherwise, a temporary directory is used for images and
the data is stored in `org-latex-preview--table' for the duration
of the Emacs session."
  :group 'org-latex
  :package-version '(Org . "9.7")
  :type 'boolean)

(defcustom org-latex-preview-persist-expiry 7
  "A homologue of `org-persist-default-expiry' for preview data.
This is only relevant when `org-latex-preview-persist' is non-nil."
  :group 'org-latex
  :package-version '(Org . "9.7")
  :type '(choice (const :tag "Never" never)
          (const :tag "Always" nil)
          (number :tag "Keep N days")
          (function :tag "Function")))

(defcustom org-latex-preview-numbered nil
  "Whether to calculate and apply correct equation numbering.
When nil, equation numbering is disabled and a diamond symbol is
shown in place of the equation number.

When non-nil, equation numbering is tracked across the document.

Alternatively, when set to the symbol \"preview\" numbering will
simply be left as the automatic LaTeX numbering generated when
previewing the batch of fragments.  This may be mostly-correct,
or mostly-incorrect depending on the situation."
  :group 'org-latex
  :package-version '(Org . "9.7")
  :type '(choice (const :tag "No" nil)
          (const :tag "Preview " preview)
          (const :tag "Yes" t)))

(defcustom org-latex-preview-processing-indicator 'fringe
  "The style of visual indicator for LaTeX currently being processed.
This sets the method used to indicated that a LaTeX fragment is
currently being processed for display.

There are three recognised value symbols:
- nil, do not indicate fragment processing.
- face, apply a special face to fragments that are being processed.
  You can customize the face `org-latex-preview-processing-face' to
  change how it appears.
- fringe, apply a fringe marker to lines where fragments are being
  processed."
  :group 'org-latex
  :package-version '(Org . "9.7")
  :type '(choice
          (const :tag "No indicator" nil)
          (const :tag "Fringe marker" fringe)
          (const :tag "Processing face" face)))

(defcustom org-latex-preview-auto-blacklist nil
  "List of movement commands that will not cause previews to be
revealed when using `org-latex-preview-auto-mode'."
  :type '(repeat symbol)
  :package-version '(Org . "9.7")
  :group 'org-latex-preview)

(defcustom org-latex-preview-process-finish-functions nil
  "Abnormal hook run after preview generation.

Each function in this hook is called with three arguments:

- The exit-code of the preview generation process. More
  specifically, this is the exit-code of the image-converter, the
  final process in the chain of processes that generates a latex
  preview image.

- The process buffer.

- The info plist. ;;TODO: Improve this."
  :group 'org-latex-preview
  :type 'hook)

(defcustom org-latex-preview-update-overlay-functions nil
  "Abnormal hook run after a preview-overlay is updated.

Each function in this hook is called with one argument, the
overlay that was updated."
  :group 'org-latex-preview
  :type 'hook)

(defcustom org-latex-preview-close-hook nil
  "Hook run after placing a LaTeX preview image.

This hook typically runs when the cursor is moved out of a LaTeX
fragment or environment with `org-latex-preview-auto-mode'
active, causing the display of text contents to be replaced by
the corresponding preview image."
  :group 'org-latex-preview
  :type 'hook)
(defcustom org-latex-preview-open-hook nil
  "Hook run after hiding a LaTeX preview image.

This hook typically runs when the cursor is moved into a LaTeX
fragment or environment with `org-latex-preview-auto-mode'
active, causing the display of the preview image to be replaced
by the corresponding LaTeX fragment text."
  :group 'org-latex-preview
  :type 'hook)

(defface org-latex-preview-processing-face '((t :inherit shadow))
  "Face applied to LaTeX fragments for which a preview is being generated.

See `org-latex-preview-processing-indicator'."
  :group 'org-faces)

(defconst org-latex-preview--image-log "*Org Preview Convert Output*"
  "Buffer name for Preview image conversion output.")
(defconst org-latex-preview--latex-log "*Org Preview LaTeX Output*"
  "Buffer name for Preview LaTeX output.")
(defconst org-latex-preview--precompile-log "*Org Preview Preamble Precompilation*"
  "Buffer name for Preview LaTeX output.")

(defcustom org-latex-preview-preamble "\\documentclass{article}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\usepackage{xcolor}"
  "The document header used for processing LaTeX fragments.
It is imperative that this header make sure that no page number
appears on the page.  The package defined in the variables
`org-latex-default-packages-alist' and `org-latex-packages-alist'
will either replace the placeholder \"[PACKAGES]\" in this
header, or they will be appended."
  :group 'org-latex-preview
  :type 'string)

(defcustom org-latex-preview-width 0.6
  "The text width when compiling LaTeX fragments.
This can take a few forms, namely:
- A string giving a LaTeX dimension (e.g. \"12cm\").
- A floating point value between 0.0 and 1.0,
  this sets the text width to this ratio of the page width.
- nil, in which case the default text width is unmodified."
  :group 'org-latex-preview
  :package-version '(Org . "9.7")
  :type '(choice
          (string :tag "LaTeX width")
          (float :tag "Proportional width")
          (const :tag "Unset" nil)))

(defcustom org-latex-preview-use-precompilation t
  "Use LaTeX header precompilation when previewing fragments.
This causes a slight delay the first time `org-latex-pdf-process'
is called in a buffer, but subsequent calls will be faster.

This requires the LaTeX package \"mylatexformat\" to be installed."
  :group 'org-latex-preview
  :package-version '(Org . "9.7")
  :type 'boolean)

(defcustom org-latex-preview-auto-generate t
  "Whether `org-latex-preview-auto-mode' should apply to new fragments.

When non-nil, newly inserted/typed LaTeX fragments and
environments will be automatically previewed.  Otherwise, only
existing LaTeX previews will be automatically hidden/shown on
cursor movement and regenerated after edits."
  :group 'org-latex
  :package-version '(Org . "9.7")
  :type 'boolean)

(defconst org-latex-preview--tentative-math-re
  "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}"
  "Regexp whith will match all instances of LaTeX math.
Note that this will also produce false postives, and
`org-element-context' should be used to verify that matches are
indeed LaTeX fragments/environments.")

(defconst org-latex-preview--ignored-faces
  '(org-indent)
  "Faces that should not affect the color of preview overlays.")

(defconst org-latex-preview--svg-fg-standin "#000001"
  "Hex color that is used as a stand-in for the current color.
The entire purpose of this is to be replaced by \"currentColor\"
in `org-latex-preview--svg-make-fg-currentColor', and so it
should be a color that is extremely likely not otherwise found in
the image.")

(defconst org-latex-preview--overlay-priority -80
  "The priority used with preview overlays.")

(defun org-latex-preview--ensure-overlay (beg end)
  "Build an overlay between BEG and END."
  (let (ov)
    (dolist (o (overlays-in beg end))
      (when (eq (overlay-get o 'org-overlay-type)
                'org-latex-overlay)
        (if (or ov (not (and (= beg (overlay-start o))
                             (= end (overlay-end o)))))
            (delete-overlay o)
          (setq ov o)
          ;; Reset all potentially modified properties.
          (overlay-put ov 'face nil)
          (overlay-put ov 'help-echo nil)     ;tooltip error display
          (overlay-put ov 'before-string nil) ;error fringe marker
          ;; (overlay-put ov 'hidden-face nil)   ;(re)store svg face
          ;; We do not set the display property of preview image
          ;; overlays to nil when ensuring that an overlay exists.
          ;; This causes flicker during regeneration as the the
          ;; underlying text is shown and then replaced with the new
          ;; image.
          ;;
          ;; We also do not reset the image spec stored in the
          ;; `preview-image' property, or the state of the preview
          ;; stored in the `view-text' property, as persisting the
          ;; state of an already existing overlay is required for live
          ;; previews.
          (overlay-put ov 'preview-state nil) ;is fragment modified?
          )))
    (unless ov
      (setq ov (make-overlay beg end nil 'front-advance))
      (overlay-put ov 'org-overlay-type 'org-latex-overlay)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'priority org-latex-preview--overlay-priority)
      (overlay-put ov 'modification-hooks
                   (list #'org-latex-preview-auto--mark-overlay-modified))
      (overlay-put ov 'insert-in-front-hooks
                   (list #'org-latex-preview-auto--insert-front-handler))
      (overlay-put ov 'insert-behind-hooks
                   (list #'org-latex-preview-auto--insert-behind-handler)))
    ov))

(defun org-latex-preview--indicate-processing (ov &optional on)
  "Modify OV to provide visual indication of LaTeX fragment preview generation.

When `org-latex-preview-processing-indicator' is set to fringe, a
triangle in the left fringe will be shown or hidden depending on ON.

When `org-latex-preview-processing-indicator' is set to face, the
overlay face is set to `org-latex-preview-processing-face'."
  (pcase org-latex-preview-processing-indicator
    ('fringe
     (overlay-put
      ov 'before-string
      (and on (propertize "!" 'display
                          `(left-fringe right-triangle
                            fringe)))))
    ('face
     (overlay-put ov 'face (and on 'org-latex-preview-processing-face)))))

(defun org-latex-preview-auto--mark-overlay-modified (ov after-p _beg _end &optional _l)
  "When AFTER-P mark OV as modified and display nothing."
  (when after-p
    (unless (eq (overlay-get ov 'preview-state) 'modified)
      (overlay-put ov 'preview-state 'modified)
      (overlay-put ov 'face nil)
      (overlay-put ov 'display nil))))

(defun org-latex-preview--update-overlay (ov path-info)
  "Update the overlay OV to show the image specified by PATH-INFO."
  (let* ((zoom (or (plist-get org-latex-preview-options :zoom) 1.0))
         (height (plist-get (cdr path-info) :height))
         (depth (plist-get (cdr path-info) :depth))
         (errors (plist-get (cdr path-info) :errors))
         (image-type (plist-get (cdr path-info) :image-type))
         (image-display
          (and (car path-info)
               (list 'image
                     :type image-type
                     :file (car path-info)
                     :height (and height (cons (* height zoom) 'em))
                     :ascent (if (and depth height)
                                 ;; The baseline seems to tend to sit slightly
                                 ;; lower than it should be, and a very mild
                                 ;; bias seems to improve the visual result.
                                 ;; From testing with a collecting of LaTeX
                                 ;; maths fonts (cm, cmbright, arev, pxfonts,
                                 ;; notomath, nextxsf, eulervm) decreacing the
                                 ;; depth measurement by 0.02pt in the baseline
                                 ;; calculation seems to work well.
                                 ;; I have yet to come across any situation
                                 ;; where this results in a negative depth,
                                 ;; however we may as well ensure that never
                                 ;; occurs.
                                 (round (* 100 (- 1 (/ (max 0.0 (- depth 0.02))
                                                       height))))
                               'center)))))
    (overlay-put ov 'preview-image image-display)
    (cond
       ((eq image-type 'svg)
        (overlay-put
         ov 'hidden-face
         (or (and errors 'error)
             (org-latex-preview--face-around
              (overlay-start ov) (overlay-end ov)))))
       (errors
        (overlay-put
         ov 'before-string
         (propertize "!" 'display
                     `(left-fringe exclamation-mark error)))))
    (unless (overlay-get ov 'view-text) ;Live previewing this element, update in background
      (when org-latex-preview-processing-indicator
        (org-latex-preview--indicate-processing ov))
      ;; This is a temporary measure until a more sophisticated
      ;; interface for errors is available in Org.
      (when (and errors tooltip-mode)
        (overlay-put ov 'help-echo errors))
      (when image-display (overlay-put ov 'display image-display))
      (overlay-put ov 'face (overlay-get ov 'hidden-face)))
    (run-hook-with-args 'org-latex-preview-update-overlay-functions ov)))

(defun org-latex-preview--face-around (start end)
  "Return the relevant face symbol around the region START to END.
A relevant face symbol before START is prefered, with END
examined if none could be found, and finally the default face
used as the final fallback.
Faces in `org-latex-preview--ignored-faces' are ignored."
  (or (and (> start (point-min))
           (not (eq (char-before start) ?\n))
           (let ((face (get-text-property (1- start) 'face)))
             (cond
              ((consp face)
               (cl-set-difference face org-latex-preview--ignored-faces))
              ((not (memq face org-latex-preview--ignored-faces))
               face))))
      (and (> (point-max) end)
           (not (eq (char-after end) ?\n))
           (let ((face (get-text-property (1+ end) 'face)))
             (cond
              ((consp face)
               (cl-set-difference face org-latex-preview--ignored-faces))
              ((not (memq face org-latex-preview--ignored-faces))
               face))))
      'default))

;; Code for `org-latex-preview-auto-mode':
;;
;; The boundaries of latex preview image overlays are automatically
;; extended to track changes in the underlying text by the functions
;; `org-latex-preview-auto--insert-front-handler' and
;; `org-latex-preview-auto--insert-behind-handler'.  These are placed in
;; the `insert-in-front-hooks' and `insert-behind-hooks' properties of
;; the iamge overlays. See (info "(elisp) Overlay Properties").
;; Additionally, when an overlay's text is modified,
;; `org-latex-preview-auto--mark-overlay-modified', placed in the overlay's
;; modification hook, notes this in the overlay's `preview-state'
;; property.
;;
;; This code examines the previous and current cursor
;; positions after each command.  It uses the variables
;; `org-latex-preview-auto--from-overlay' and `org-latex-preview-auto--marker' to track
;; this.
;;
;; If the cursor has moved out of or into a latex preview overlay,
;; the overlay is changed to display or hide its image respectively.
;; The functions `org-latex-preview-auto--handle-pre-cursor' and
;; `org-latex-preview-auto--handle-post-cursor' do this.  These are palced in
;; `pre-command-hook' and `post-command-hook' respectively.
;;
;; When the cursor positions pre- and post-command are inside an
;; overlay, it uses the overlay property `view-text' to check if the
;; source and destination overlays are distinct.  If they are it shows
;; and hides images as appropriate.
;;
;; If the latex fragment text for an existing overlay is modified, a
;; new preview image will be generated automatically.  The
;; modification state of the overlay is stored in the overlay property
;; `preview-state', and the function
;; `org-latex-preview-auto--close-previous-overlay' handles the recompilation.
;;
;; When the user option `org-latex-preview-auto-generate' is
;; non-nil, previews are auto-generated for latex fragments as they
;; are inserted into the buffer.  This work is handled by
;; `org-latex-preview-auto--detect-fragments-in-change', which is added to
;; `after-change-functions'.  It does this by placing dummy overlays
;; that don't display images, but are marked as having been modified.

(defvar-local org-latex-preview-auto--from-overlay nil
  "Whether the cursor if starting from within a preview overlay.")
(defvar-local org-latex-preview-auto--marker nil
  "Marker to keep track of the previous cursor position.
This helps with tracking cursor movement into and out of preview overlays.")
(defvar-local org-latex-preview-auto--inhibit nil
  "Delay the state machine that decides to auto-generate preview fragments.")

(defsubst org-latex-preview-auto--move-into (ov)
  "Adjust column when moving into the overlay OV from below."
  (when (> (marker-position org-latex-preview-auto--marker)
           (line-end-position))
    (goto-char (overlay-end ov))
    (goto-char (max (line-beginning-position)
                    (overlay-start ov)))))

(defun org-latex-preview-auto--handle-pre-cursor ()
  "Record the previous state of the cursor position.

This keeps track of the cursor relative to the positions of
Org latex preview overlays.

This is intended to be placed in `pre-command-hook'."
  (if org-latex-preview-auto--inhibit
      (setq org-latex-preview-auto--inhibit nil)
    (setq org-latex-preview-auto--from-overlay
          (eq (get-char-property (point) 'org-overlay-type)
              'org-latex-overlay))
    (set-marker org-latex-preview-auto--marker (point))))

(defun org-latex-preview-auto--handle-post-cursor ()
  "Toggle or generate LaTeX previews based on cursor movement.

If the cursor is moving into a preview overlay, \"open\" it to
display the underlying latex fragment.  If the cursor is moving
out of a preview overlay, show the image again or generate a new
one as appropriate.

See `org-latex-preview-auto-generate' to customize this behavior.

This is intended to be placed in `post-command-hook'."
  (let ((into-overlay-p (eq (get-char-property (point) 'org-overlay-type)
                            'org-latex-overlay)))
    (cond
     ((and into-overlay-p org-latex-preview-auto--from-overlay)
      (unless (get-char-property (point) 'view-text)
        ;; Jumped from overlay to overlay
        (org-latex-preview-auto--close-previous-overlay)
        (org-latex-preview-auto--open-this-overlay)))
     ((and into-overlay-p (not org-latex-preview-auto--from-overlay))
      ;; Moved into overlay
      (org-latex-preview-auto--open-this-overlay))
     (org-latex-preview-auto--from-overlay
      ;; Moved out of overlay
      (org-latex-preview-auto--close-previous-overlay)))
    (set-marker org-latex-preview-auto--marker (point))))

(defun org-latex-preview-auto--detect-fragments-in-change (beg end _)
  "Examine the content between BEG and END, and preview LaTeX fragments found."
  (when org-latex-preview-auto-generate
    (let ((initial-point (point))
          fragments)
      (save-excursion
        ;; Find every location in the changed region where a backslash
        ;; is succeeded by a parenthesis or square bracket, and check
        ;; for a LaTeX fragment.
        (goto-char beg)
        (unless (eobp)
          (while (search-forward "\\" end t)
            (and (memq (char-after) '(?\( ?\) ?\[ ?\]))
                 (push (org-latex-preview-auto--maybe-track-element-here
                        'latex-fragment initial-point)
                       fragments))))
        ;; Find every location in the changed region where a parenthesis
        ;; or square bracket is preceeded by a backslash, and check for
         ;; a LaTeX fragment.
        (goto-char beg)
        (unless (bobp)
          (while (re-search-forward "[][()]" end t)
            (and (eq (char-before (1- (point))) ?\\)
                 (push (org-latex-preview-auto--maybe-track-element-here
                        'latex-fragment initial-point)
                       fragments))))
        ;; Check for LaTeX environments on lines affected by the change.
        ;; Start by finding all affected lines with at least four
        ;; characters of content. Then we can check if the line starts
        ;; with "\beg" or "\end", and if so check for a LaTeX environment.
        (goto-char beg)
        (beginning-of-line)
        (skip-chars-forward " \t")
        (when (< (point) end)
          (let ((line-start-positions
                 (and (> (point-max) (+ 4 (point)))
                      (list (point)))))
            (while (and (< (point) end)
                        (search-forward "\n" end t))
              (skip-chars-forward " \t")
              (when (> (point-max) (+ 4 (point)))
                (push (point) line-start-positions)))
            (dolist (line-start line-start-positions)
              (goto-char line-start)
              (and (eq (char-after) ?\\)
                   (member (buffer-substring (point) (+ (point) 4))
                           '("\\beg" "\\end"))
                   (push (org-latex-preview-auto--maybe-track-element-here
                          'latex-environment initial-point)
                         fragments))))))
      (when (setq fragments (delq nil fragments))
        (when (and org-latex-preview-numbered
                   (cl-find 'latex-environment fragments
                          :key #'org-element-type :test #'eq))
          (setq fragments
                (append fragments
                        (org-latex-preview--get-numbered-environments
                         end nil))))
        (org-latex-preview--place-from-elements
         org-latex-preview-default-process
         fragments)))))

(defun org-latex-preview-auto--maybe-track-element-here (type pos)
  "Check for an org element of TYPE at `point' and ensure an overlay exists.
If POS lies within the element, nil is returned.  Otherwise the
element is returned to be used to generate a preview.

If an org-latex-overlay is already present, nothing is done."
  (and (not (eq (get-char-property (point) 'org-overlay-type)
                'org-latex-overlay))
       (when-let* ((element (org-element-context))
                   ((eq (org-element-type element) type))
                   (elem-beg (or (org-element-property :post-affiliated element)
                                 (org-element-property :begin element)))
                   (elem-end (- (org-element-property :end element)
                                (or (org-element-property :post-blank element) 0)
                                (if (eq (char-before (org-element-property :end element))
                                        ?\n)
                                    1 0)))
                   (ov (org-latex-preview--ensure-overlay elem-beg elem-end)))
         (overlay-put ov 'preview-state 'modified)
         (if (<= elem-beg pos elem-end)
             (progn
               (overlay-put ov 'view-text t)
               ;; Record a position safely inside the created overlay
               (set-marker org-latex-preview-auto--marker
                           (min pos (1- elem-end)))
               (setq org-latex-preview-auto--from-overlay t)
               nil)
           (setq org-latex-preview-auto--inhibit t)
           element))))

(defun org-latex-preview-auto--open-this-overlay ()
  "Open Org latex preview image overlays.

If there is a latex preview image overlay at point, hide the
image and display its text."
  (dolist (ov (overlays-at (point)))
    (when (and (eq (overlay-get ov 'org-overlay-type)
                   'org-latex-overlay)
               (not (memq this-command
                          org-latex-preview-auto-blacklist)))
      (overlay-put ov 'display nil)
      (overlay-put ov 'view-text t)
      (when-let ((f (overlay-get ov 'face)))
        (overlay-put ov 'hidden-face f)
        (overlay-put ov 'face nil))
      (org-latex-preview-auto--move-into ov)
      (setq org-latex-preview-auto--from-overlay nil)
      (run-hooks 'org-latex-preview-open-hook))))

(defun org-latex-preview-auto--close-previous-overlay ()
  "Close Org latex preview image overlays.

If there is a latex preview image overlay at the previously
recorded cursor position, hide its text and display the
image.  The preview image is regenerated if necessary."
  (dolist (ov (overlays-at (marker-position org-latex-preview-auto--marker)))
    (when (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
      (overlay-put ov 'view-text nil)
      (if (eq (overlay-get ov 'preview-state) 'modified)
          ;; It may seem odd to use an timer for this action, but by
          ;; introducing a brief window for Emacs to deal with input
          ;; events triggered during prior processing the perceptible
          ;; delay is reduced.  Setting an 0.05s timer isn't
          ;; necesarily the optimal duration, but from a little
          ;; testing it appears to be fairly reasonable.
          (run-at-time 0.05 nil #'org-latex-preview-auto--regenerate-overlay ov)
        (when-let (f (overlay-get ov 'hidden-face))
          (unless (eq f 'org-latex-preview-processing-face)
            (overlay-put ov 'face f))
          (overlay-put ov 'hidden-face nil))
        (overlay-put ov 'display (overlay-get ov 'preview-image)))
      (run-hooks 'org-latex-preview-close-hook))))

(defun org-latex-preview-auto--regenerate-overlay (ov)
  "Regenerate the LaTeX fragment under overlay OV."
  (with-current-buffer (overlay-buffer ov)
    (let* ((fragment (save-excursion
                       (goto-char (overlay-start ov))
                       (org-element-context)))
           (others (and org-latex-preview-numbered
                        (eq (org-element-type fragment) 'latex-environment)
                        (org-latex-preview--get-numbered-environments
                         (overlay-end ov) nil))))
      (if (memq (org-element-type fragment)
                '(latex-fragment latex-environment))
          (org-latex-preview--place-from-elements
           org-latex-preview-default-process
           (cons fragment others))
        (delete-overlay ov)
        (when others
          (org-latex-preview--place-from-elements
           org-latex-preview-default-process
           others))))))

(defun org-latex-preview-auto--insert-front-handler
    (ov after-p _beg end &optional _length)
  "Extend Org LaTeX preview text boundaries when editing previews.

OV is the overlay displaying the preview.  For the meaning of
AFTER-P, END and the other arguments, see the
`modification-hooks' property of overlays in the Elisp
manual: (elisp) Overlay Properties."
  (when after-p
    (unless undo-in-progress
      (if (eq (overlay-get ov 'preview-state) 'active)
          (move-overlay ov end (overlay-end ov))))))

(defun org-latex-preview-auto--insert-behind-handler
    (ov after-p beg _end &optional _length)
  "Extend Org LaTeX preview text boundaries when editing previews.

OV is the overlay displaying the preview.  For the meaning of
AFTER-P, BEG and the other arguments, see the
`modification-hooks' property of overlays in the Elisp
manual: (elisp) Overlay Properties."
  (when after-p
    (unless undo-in-progress
      (if (eq (overlay-get ov 'preview-state) 'active)
          (move-overlay ov (overlay-end ov) beg)))))

;;;###autoload
(define-minor-mode org-latex-preview-auto-mode
  "Minor mode to automatically preview LaTeX fragments.

When LaTeX preview auto mode is on, LaTeX fragments in Org buffers are
automatically previewed after being inserted, and hidden when the
cursor moves into them.  This allows one to seamlessly edit and
preview LaTeX in Org buffers.

To enable auto-toggling of the preview images without auto-generating
them or vice-versa, customize the variable `org-latex-preview-auto-generate'."
  :global nil
  (if org-latex-preview-auto-mode
      (progn
        (setq org-latex-preview-auto--marker (make-marker))
        (add-hook 'pre-command-hook #'org-latex-preview-auto--handle-pre-cursor nil 'local)
        (add-hook 'post-command-hook #'org-latex-preview-auto--handle-post-cursor nil 'local)
        (add-hook 'after-change-functions #'org-latex-preview-auto--detect-fragments-in-change nil 'local)
        ;; Live previews
        (when (eq org-latex-preview-auto-generate 'live)
          (setq org-latex-preview-live--docstring " ")
          (setq-local org-latex-preview-live--generator
                      (thread-first #'org-latex-preview-live--regenerate
                                    (org-latex-preview-live--throttle
                                     org-latex-preview-throttle)
                                    (org-latex-preview-live--debounce
                                     org-latex-preview-debounce)))
          (when (eq org-latex-preview-live-display-type 'eldoc)
            (add-hook 'eldoc-documentation-functions #'org-latex-preview-live--display-in-eldoc nil t))
          (add-hook 'org-src-mode-hook #'org-latex-preview-live--src-buffer-setup)
          (add-hook 'org-latex-preview-close-hook #'org-latex-preview-live--clearout nil 'local)
          (add-hook 'org-latex-preview-open-hook #'org-latex-preview-live--ensure-overlay nil 'local)
          (add-hook 'after-change-functions org-latex-preview-live--generator 90 'local)
          (add-hook 'org-latex-preview-update-overlay-functions #'org-latex-preview-live--update-overlay nil 'local)
          ;; (add-hook 'org-latex-preview-process-finish-functions #'org-latex-preview-live--update-overlay-run nil 'local)
          ))
    (remove-hook 'pre-command-hook #'org-latex-preview-auto--handle-pre-cursor 'local)
    (remove-hook 'post-command-hook #'org-latex-preview-auto--handle-post-cursor 'local)
    (remove-hook 'after-change-functions #'org-latex-preview-auto--detect-fragments-in-change 'local)
    ;; Live previews
    (org-latex-preview-live--clearout)
    (setq-local org-latex-preview-live--generator nil)
    (when (eq org-latex-preview-live-display-type 'eldoc)
      (remove-hook 'eldoc-documentation-functions #'org-latex-preview-live--display-in-eldoc t))
    (remove-hook 'org-latex-preview-close-hook #'org-latex-preview-live--clearout 'local)
    (remove-hook 'org-latex-preview-open-hook #'org-latex-preview-live--ensure-overlay 'local)
    (remove-hook 'after-change-functions org-latex-preview-live--generator 'local)
    (remove-hook 'org-latex-preview-update-overlay-functions #'org-latex-preview-live--update-overlay 'local)
    ;; (remove-hook 'org-latex-preview-process-finish-functions #'org-latex-preview-live--update-overlay-run 'local)
    ))

;; Code for "live" preview generation
;;
;; When `org-latex-preview-auto-mode' is turned on and
;; `org-latex-preview-auto-generate' is set to the symbol `live',
;; previews are generated in the background with each change to the
;; LaTeX fragment being edited.  This continuously updated preview is
;; shown to the right of the LaTeX fragment, or under the LaTeX
;; environment being edited.  (Alternatively, it can be shown using
;; Eldoc.)
;;
;; The code works as follows (simplified description):

;; - When the cursor enters a fragment and
;; `org-latex-preview-auto-mode' is active, it is "opened" up and the
;; preview image is hidden.  At this time, a new overlay (stored in
;; the buffer-local `org-latex-preview-live--overlay') is created next
;; to (or under) the fragment.  The `after-string' property of this
;; overlay is updated to show the existing preview image.
;;
;; - A handler is added to `after-change-functions' to regenerate the
;;   preview for the fragment.
;;
;; When the preview is regenerated, the `after-string' property of
;;   `org-latex-preview-live--overlay' is updated to show the new
;;   image.  This regeneration is modulated with a debounce
;;   `org-latex-preview-live--debounce' and a throttle
;;   `org-latex-preview-live--throttle'.
;;
;; - When the cursor exits the boundaries of the fragment, the overlay
;;   in `org-latex-preview-live--overlay' is deleted.

(defvar-local org-latex-preview-live--overlay nil)
(defvar-local org-latex-preview-live--docstring " ")
(defvar-local org-latex-preview-live--element-type nil)
(defvar-local org-latex-preview-live--generator nil)
(defcustom org-latex-preview-live-preview-fragments t
  "Whether LaTeX fragments should be live-previewed along with
LaTeX environments."
  :group 'org-latex-preview
  :type 'boolean)
(defcustom org-latex-preview-live-display-type 'buffer
  ";;TODO: "
  :group 'org-latex-preview
  :type  '(choice
           (const :tag "Display next to fragment" 'buffer)
           (const :tag "Display in Eldoc" 'eldoc)))

(defcustom org-latex-preview-debounce 1.0
  "Idle time before regenerating LaTeX previews.  When
`org-latex-preview-auto-generate' is set to `live' and
`org-latex-preview-auto-mode' is active, live previews are
updated when there have been no changes to the LaTeX fragment or
environment for at least this much time."
  :group 'org-latex-preview
  :package-version '(Org . "9.7")
  :type 'number)

(defcustom org-latex-preview-throttle 1.5
  "Throttle time for live LaTeX previews.  When
`org-latex-preview-auto-generate' is set to `live' and
`org-latex-preview-auto-mode' is active, live previews are
updated no more than once in this interval of time."
  :group 'org-latex-preview
  :package-version '(Org . "9.7")
  :type 'number)

(defun org-latex-preview-live--debounce (func duration)
  "Return debounced func with duration applied."
  (let ((debounce-timer))
    (lambda (&rest args)
      (when (timerp debounce-timer)
        ;; (message "Debounced!")
        (cancel-timer debounce-timer)
        (setq debounce-timer nil))
      (setq debounce-timer
            (run-at-time
             duration nil
             (lambda ()
               (setq debounce-timer nil)
               ;; (message "I ran!")
               (apply func args)))))))

(defun org-latex-preview-live--throttle (func timeout)
  "Return throttled function with timeout applied."
  (let ((waiting))
    (lambda (&rest args)
      (unless waiting
        (apply func args)
        (setq waiting t)
        (run-at-time timeout nil
                     (lambda () (setq waiting nil)))))))

(defun org-latex-preview-live--clearout ()
  ";TODO: "
  (setq org-latex-preview-live--element-type nil)
  (and org-latex-preview-live--overlay
       (overlayp org-latex-preview-live--overlay)
       (delete-overlay org-latex-preview-live--overlay)))

(defun org-latex-preview-live--regenerate (beg end _)
  ";TODO: "
  (dolist (ov (overlays-at (point)))
    (when (and (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
               (<= (overlay-start ov) beg)
               (>= (overlay-end ov) end))
      ;; The order is important here
      (unless (and (overlayp org-latex-preview-live--overlay)
                   (overlay-buffer org-latex-preview-live--overlay))
        (org-latex-preview-live--ensure-overlay ov))
      ;; Regenerate preview -- updating the live preview happens in an
      ;; org-async callback
      (org-latex-preview-auto--regenerate-overlay ov)
      (if (overlay-buffer ov)
          ;; (org-latex-preview-live--update-props
          ;;  (overlay-get ov 'preview-image))
          t
        (org-latex-preview-live--clearout)))))

(defun org-latex-preview-live--update-props (image-spec &optional box-face)
  ""
  ;; (let ((box-face
  ;;        (or box-face
  ;;            (get-text-property
  ;;             0 'face
  ;;             (overlay-get org-latex-preview-live--overlay 'after-string)))))
  ;;   (overlay-put org-latex-preview-live--overlay 'after-string
  ;;                (propertize " " 'display image-spec 'face box-face)))

  (put-text-property
   0 1 'display image-spec
   org-latex-preview-live--docstring)
  (when box-face
      (put-text-property
       0 1 'face box-face
       org-latex-preview-live--docstring)))

(defun org-latex-preview-live--display-in-eldoc (callback)
  ";;TODO: "
  (when (and org-latex-preview-live--docstring
             (get-char-property (point) 'org-overlay-type))
    (funcall callback org-latex-preview-live--docstring)))

(defun org-latex-preview-live--ensure-overlay (&optional ov)
  (when-let*
      ((ov (or ov (cl-find-if
                   (lambda (o) (eq (overlay-get o 'org-overlay-type)
                              'org-latex-overlay))
                   (overlays-at (point)))))
       (beg (overlay-start ov))
       (end (overlay-end ov)))
    (let ((latex-env-p (progn
                         (unless org-latex-preview-live--element-type
                           (let* ((elm (org-element-context))
                                  (elm-type (org-element-type elm)))
                             (setq org-latex-preview-live--element-type
                                   elm-type)))
                         (eq org-latex-preview-live--element-type
                             'latex-environment))))
      (when (or latex-env-p org-latex-preview-live-preview-fragments)
        ;; Create live preview overlay if necessary
        (when (eq org-latex-preview-live-display-type 'buffer)
          (unless (and (overlayp org-latex-preview-live--overlay)
                       (overlay-buffer org-latex-preview-live--overlay))
            (setq org-latex-preview-live--overlay
                  (make-overlay end end nil 'front-advance))
            (overlay-put org-latex-preview-live--overlay
                         'priority org-latex-preview--overlay-priority))
          ;; Update the live preview overlay
          ;; (overlay-put ov 'view-text t)
          (overlay-put org-latex-preview-live--overlay 'after-string
                       org-latex-preview-live--docstring))
        (if (not latex-env-p)
            (org-latex-preview-live--update-props
             (overlay-get ov 'preview-image) '(:box t))
          (overlay-put org-latex-preview-live--overlay 'before-string "\n")
          (org-latex-preview-live--update-props
           (overlay-get ov 'preview-image) '(:overline t :underline t)))))))

;; TODO: Add this to the live preview mode setup.
;;
;; NOTE: This should only run when the fragment being live-previewed
;; is included in the image-conversion run.  This can fail to happen
;; when two conditions are simultaneously met:
;;
;; i) The current fragment is changed in a way that does not require
;; processing, for example if the delimiters are deleted or if the
;; changed fragment corresponds to a cached image, and
;; 
;; ii) Other fragments are re-processed, for example if the numbering
;; changes.
(defun org-latex-preview-live--update-overlay-run (exit-code _buf extended-info)
  (when-let (((= exit-code 0))
             ((overlayp org-latex-preview-live--overlay))
             ((overlay-buffer org-latex-preview-live--overlay))
             (ov
              (thread-first
                (plist-get extended-info :fragments)
                (car)
                (plist-get :overlay))))
    (when (memq ov (overlays-at (point)))
        (org-latex-preview-live--update-props
         (overlay-get ov 'preview-image)))))

;; TODO: This should not run after each overlay is updated, only the
;; one being previewed.
(defun org-latex-preview-live--update-overlay (ov)
  (and (overlayp org-latex-preview-live--overlay)
         (org-latex-preview-live--update-props
          (overlay-get ov 'preview-image))))

;; Code for previews in org-src buffers
(defun org-latex-preview-live--src-buffer-setup ()
  (when (and (equal major-mode (org-src-get-lang-mode "latex"))
             (buffer-local-value 'org-latex-preview-auto-mode
                                 (marker-buffer org-src--beg-marker)))
    (let* ((org-buf (marker-buffer org-src--beg-marker))
           (src-buf (current-buffer))
           (org-buf-visible-p (window-live-p (get-buffer-window org-buf)))
           (preamble (org-latex-preview--get-preamble org-buf))
           (element (org-element-context))
           ;; Do not use (org-element-property :begin element) to
           ;; find the bounds -- this is fragile under typos.
           (beg (save-excursion (goto-char (point-min))
                                (skip-chars-forward "\n \t\r")
                                (point)))
           (end (save-excursion (goto-char (point-max))
                                (skip-chars-backward "\n \t\r")
                                (point)))
           (numbering-offsets) (ov) (orig-ov))
      (setq org-latex-preview-auto--marker (point-marker))
      (with-current-buffer org-buf
        (org-latex-preview-live--clearout)
        (when (setq orig-ov
                    (cl-some
                     (lambda (o) (and (eq (overlay-get o 'org-overlay-type)
                                     'org-latex-overlay)
                                 o))
                     (overlays-at (point))))
          (setq ov (copy-overlay orig-ov))
          (overlay-put ov 'view-text t)
          (move-overlay ov beg end src-buf))
        (org-latex-preview-auto--close-previous-overlay))

      (or ov (setq ov (org-latex-preview--ensure-overlay beg end)))
      ;; Adjust numbering
      (when (and org-latex-preview-numbered
                 (eq (org-element-type element) 'latex-environment))
        (with-current-buffer org-buf
          (when-let ((numbering-table (org-latex-preview--environment-numbering-table)))
            (setq numbering-offsets (list (gethash element numbering-table))))))

      (when (eq (buffer-local-value 'org-latex-preview-auto-generate org-buf) 'live)
        (if org-buf-visible-p
            ;; Display live previews in original org buffer
            (progn
              (setq-local org-latex-preview-live--generator
                          (thread-first
                            (lambda (&rest _)
                              (let* ((content (string-trim (buffer-string))))
                                (with-current-buffer org-buf
                                  (org-latex-preview-place
                                   org-latex-preview-default-process
                                   (list (list (overlay-start orig-ov)
                                               (overlay-end orig-ov)
                                               content))
                                   numbering-offsets))))
                            (org-latex-preview-live--throttle
                             org-latex-preview-throttle)
                            (org-latex-preview-live--debounce
                             org-latex-preview-debounce)))
              (add-hook 'after-change-functions org-latex-preview-live--generator 90 'local))

          ;; Or display live previews in org-src buffer
          (org-latex-preview-live--ensure-overlay ov)
          (add-hook 'org-latex-preview-update-overlay-functions
                    #'org-latex-preview-live--update-overlay
                    nil 'local)
          (setq-local org-latex-preview-live--generator
                      (thread-first
                        (lambda (&rest _)
                          (org-latex-preview-place
                           org-latex-preview-default-process
                           (list (list (point-min) (point-max) (buffer-string)))
                           numbering-offsets preamble))
                        (org-latex-preview-live--throttle
                         org-latex-preview-throttle)
                        (org-latex-preview-live--debounce
                         org-latex-preview-debounce)))
          (add-hook 'org-latex-preview-open-hook #'org-latex-preview-live--ensure-overlay nil 'local)
          (add-hook 'org-latex-preview-close-hook #'org-latex-preview-live--clearout nil 'local)
          (add-hook 'after-change-functions org-latex-preview-live--generator 90 'local)))
      ;; auto-mode in the src buffer
      (add-hook 'pre-command-hook #'org-latex-preview-auto--handle-pre-cursor nil 'local)
      (add-hook 'post-command-hook #'org-latex-preview-auto--handle-post-cursor nil 'local))))

(defun org-latex-preview-clear-overlays (&optional beg end)
  "Remove all overlays with LaTeX fragment images in current buffer.
When optional arguments BEG and END are non-nil, remove all
overlays between them instead.  Return a non-nil value when some
overlays were removed, nil otherwise."
  (let ((overlays
         (cl-remove-if-not
          (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
          (overlays-in (or beg (point-min)) (or end (point-max))))))
    (mapc #'delete-overlay overlays)
    overlays))

(defun org-latex-preview--preview-region (beg end)
  "Preview LaTeX fragments between BEG and END.
BEG and END are buffer positions."
  (org-latex-preview-fragments
   org-latex-preview-default-process
   beg end))

;;;###autoload
(defun org-latex-preview (&optional arg)
  "Toggle preview of the LaTeX fragment at point.

If the cursor is on a LaTeX fragment, create the image and
overlay it over the source code, if there is none.  Remove it
otherwise.  If there is no fragment at point, display images for
all fragments in the current section.  With an active region,
display images for all fragments in the region.

With a `\\[universal-argument]' prefix argument ARG, clear images \
for all fragments
in the current section.

With a `\\[universal-argument] \\[universal-argument]' prefix \
argument ARG, display image for all
fragments in the buffer.

With a `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix argument ARG, clear image for all
fragments in the buffer."
  (interactive "P")
  (cond
   ((not (display-graphic-p)) nil)
   ;; Clear whole buffer.
   ((equal arg '(64))
    (org-latex-preview-clear-overlays (point-min) (point-max))
    (message "LaTeX previews removed from buffer"))
   ;; Preview whole buffer.
   ((equal arg '(16))
    (org-latex-preview--preview-region (point-min) (point-max)))
   ;; Clear current section.
   ((equal arg '(4))
    (org-latex-preview-clear-overlays
     (if (use-region-p)
         (region-beginning)
       (if (org-before-first-heading-p) (point-min)
         (save-excursion
           (org-with-limited-levels (org-back-to-heading t) (point)))))
     (if (use-region-p)
         (region-end)
       (org-with-limited-levels (org-entry-end-position)))))
   ((use-region-p)
    (org-latex-preview--preview-region (region-beginning) (region-end)))
   ;; Toggle preview on LaTeX code at point.
   ((let ((datum (org-element-context)))
      (and (memq (org-element-type datum) '(latex-environment latex-fragment))
           (org-latex-preview--auto-aware-toggle datum))))
   ;; Preview current section.
   (t
    (let ((beg (if (org-before-first-heading-p) (point-min)
                 (save-excursion
                   (org-with-limited-levels (org-back-to-heading t) (point)))))
          (end (org-with-limited-levels (org-entry-end-position))))
      (org-latex-preview--preview-region beg end)))))

(defun org-latex-preview--auto-aware-toggle (datum)
  "Toggle the preview of the LaTeX fragment/environment DATUM.
This is done with care to work nicely with `org-latex-preview-auto-mode',
should it be enabled."
  (let* ((beg (org-element-property :begin datum))
         (end (org-element-property :end datum))
         (ov (cl-some
              (lambda (o)
                (and (eq (overlay-get o 'org-overlay-type)
                         'org-latex-overlay)
                     o))
              (overlays-at beg))))
    ;; If using auto-mode, an overlay will already exist but
    ;; not be showing an image.  We can detect this
    ;; situtation via the preview-state overlay property, and
    ;; in such cases the most reasonable action is to just
    ;; (re)generate the preview image.
    (cond
     ;; When not using auto-mode.
     ((not org-latex-preview-auto-mode)
      (if (org-latex-preview-clear-overlays beg end)
          (message "LaTeX preview removed")
        (org-latex-preview--place-from-elements
         org-latex-preview-default-process (list datum))))
     ;; When using auto-mode, but no current preview.
     ((not ov)
      (org-latex-preview--place-from-elements
       org-latex-preview-default-process (list datum))
      (message "Creating LaTeX preview"))
     ;; When on a just written/edited fragment that should be previewed.
     ((eq (overlay-get ov 'preview-state) 'modified)
      (org-latex-preview-auto--regenerate-overlay ov)
      (overlay-put ov 'view-text t))
     ;; When on an unmodified fragment that is currently showing an image,
     ;; clear the image.
     ((overlay-get ov 'display)
      (org-latex-preview-clear-overlays beg end)
      (message "LaTeX preview removed"))
     ;; Since we're on an unmodified fragment but not showing an image,
     ;; let's try to show the image if possible.
     (ov
      (overlay-put ov 'view-text t)
      (overlay-put ov 'face (overlay-get ov 'hidden-face))
      (overlay-put ov 'display (overlay-get ov 'preview-image))))))

(defun org-latex-preview-collect-fragments (&optional beg end)
  "Collect all LaTeX maths fragments/environments between BEG and END."
  (let (fragments)
    (save-excursion
      (goto-char (or beg (point-min)))
      (while (re-search-forward org-latex-preview--tentative-math-re end t)
        (let ((obj (org-element-context)))
          (when (and (memq (org-element-type obj)
                           '(latex-fragment latex-environment))
                     ;; Avoid duplicating nested latex environments
                     (not (and fragments
                               (= (org-element-property :begin obj)
                                  (org-element-property :begin (car fragments))))))
            (push obj fragments)))))
    (nreverse fragments)))

(defun org-latex-preview-fragments (processing-type &optional beg end)
  "Produce image overlays of LaTeX math fragments between BEG and END.

The LaTeX fragments are processed using PROCESSING-TYPE, a key of
`org-latex-preview-process-alist'.

If `point' is currently on an LaTeX overlay, then no overlays
will be generated.  Since in practice `org-latex-preview-clear-overlays'
should have been called immediately prior to this function, this
situation should not occur in practice and mainly acts as
protection against placing doubled up overlays."
  (when (fboundp 'clear-image-cache)
    (clear-image-cache))
  ;; Optimize overlay creation: (info "(elisp) Managing Overlays").
  (when (memq processing-type '(dvipng dvisvgm imagemagick))
    (overlay-recenter (or end (point-max))))
  (unless (eq (get-char-property (point) 'org-overlay-type)
              'org-latex-overlay)
    (let ((ws (window-start)))
      (if (assq processing-type org-latex-preview-process-alist)
          (org-latex-preview--place-from-elements
           processing-type
           (nconc (org-latex-preview-collect-fragments (max ws beg) end)
                  (when (< beg ws)
                    (org-latex-preview-collect-fragments beg (1- ws)))))
        (error "Unknown conversion process %s for previewing LaTeX fragments"
               processing-type)))))

(defun org-latex-preview--construct-entries
    (elements &optional construct-numbering-p parse-tree)
  "Constuct a well formatted list of entries and (optinally) numbering offsets.
This operates by processing ELEMENTS.  When CONSTRUCT-NUMBERING-P is non-nil,
the number offsets will also be calculated, using PARSE-TREE if given."
  (let* ((numbering-table (and construct-numbering-p
                               (cl-find 'latex-environment elements
                                        :key #'org-element-type :test #'eq)
                               (org-latex-preview--environment-numbering-table
                                parse-tree)))
         (numbering-offsets
          (and numbering-table
               (mapcar
                (lambda (element)
                  (and numbering-table
                       (eq (org-element-type element) 'latex-environment)
                       (gethash element numbering-table)))
                elements)))
         (entries
          (mapcar
           (lambda (element)
             (list (or (org-element-property :post-affiliated element)
                       (org-element-property :begin element))
                   (- (org-element-property :end element)
                      (or (org-element-property :post-blank element) 0)
                      (if (eq (char-before (org-element-property :end element))
                              ?\n)
                          1 0))
                   (org-element-property :value element)))
           elements)))
    (list entries numbering-offsets)))

(defun org-latex-preview--place-from-elements (processing-type elements)
  "Preview LaTeX math fragments ELEMENTS using PROCESSING-TYPE."
  (apply #'org-latex-preview-place processing-type
         (org-latex-preview--construct-entries
          elements org-latex-preview-numbered)))

;;;###autoload
(defun org-latex-preview-place (processing-type entries &optional numbering-offsets latex-preamble)
  "Preview LaTeX math fragments ENTRIES using PROCESSING-TYPE.
Each entry of ENTRIES should be a list of 2-3 items, either
  (BEG END), or
  (BEG END VALUE)
Where BEG and END are the positions in the buffer, and the LaTeX previewed
is either the substring between BEG and END or (when provided) VALUE."
  (unless latex-preamble
    (setq latex-preamble
          (or org-latex-preview--preamble-content
              (setq org-latex-preview--preamble-content
                    (org-latex-preview--get-preamble)))))
  (let* ((processing-info
          (cdr (assq processing-type org-latex-preview-process-alist)))
         (imagetype (or (plist-get processing-info :image-output-type) "png"))
         (numbering-offsets (cons nil numbering-offsets))
         fragment-info prev-fg prev-bg)
    (save-excursion
      (dolist (entry entries)
        (pcase-let* ((`(,beg ,end ,provided-value) entry)
                     (value (or provided-value
                                (buffer-substring-no-properties beg end)))
                     (`(,fg ,bg) (org-latex-preview--colors-around beg end))
                     (number (car (setq numbering-offsets (cdr numbering-offsets))))
                     (hash (org-latex-preview--hash
                            processing-type latex-preamble value imagetype fg bg number))
                     (options (org-combine-plists
                               org-latex-preview-options
                               (list :foreground fg
                                     :background bg
                                     :number number
                                     :continue-color
                                     (and (equal prev-bg bg)
                                          (equal prev-fg fg)))
                               (and (eq processing-type 'dvisvgm)
                                    (list :foreground
                                          org-latex-preview--svg-fg-standin)))))
          (if-let ((path-info (org-latex-preview--get-cached hash)))
              (org-latex-preview--update-overlay
               (org-latex-preview--ensure-overlay beg end)
               path-info)
            (push (list :string (org-latex-preview--tex-styled
                                 processing-type value options)
                        :overlay (org-latex-preview--ensure-overlay beg end)
                        :key hash)
                  fragment-info))
          (setq prev-fg fg prev-bg bg))))
    (when fragment-info
      (org-latex-preview--create-image-async
       processing-type
       (nreverse fragment-info)
       :latex-preamble latex-preamble
       :place-preview-p t))))

(defun org-latex-preview--colors-around (start end)
  "Find colors for LaTeX previews occuping the region START to END."
  (let* ((face (org-latex-preview--face-around start end))
         (fg (pcase (plist-get org-latex-preview-options :foreground)
               ('auto
                (org-latex-preview--resolved-faces-attr face :foreground))
               ('default (face-attribute 'default :foreground nil))
               (color color)))
         (bg (pcase (plist-get org-latex-preview-options :background)
               ('auto
                (org-latex-preview--resolved-faces-attr face :background))
               ('default (face-attribute 'default :background nil))
               (color color))))
    (list fg bg)))

(defun org-latex-preview--resolved-faces-attr (face attr)
  "Find ATTR of the FACE text property.
This is surprisingly complicated given the various forms of output
\\=(get-text-property pos \\='face) can produce.

Faces in `org-latex-preview--ignored-faces' are ignored."
  (when (consp face)
    (setq face (cl-set-difference face org-latex-preview--ignored-faces))
    (when (= (length face) 1)
      (setq face (car face))))
  (cond
   ((not face)
    (face-attribute 'default attr))
   ((not (consp face)) ; Spec like org-level-1.
    (face-attribute face attr nil 'default))
   ((keywordp (car face)) ; Spec like (:inherit org-block :extend t).
    (or (plist-get face attr)
        (face-attribute 'default attr)))
   ((consp (car face)) ; Spec like ((:inherit default :extend t) org-block).
    (or (plist-get (car face) attr)
        (face-attribute (cadr face) attr nil
                        (append (cddr face) '(default)))))
   ((symbolp (car face)) ; Spec like (org-level-1 default).
    (face-attribute (car face) attr nil (append (cdr face) '(default))))))

(defun org-latex-preview--hash (processing-type preamble string imagetype fg bg &optional number)
  "Return a SHA1 hash for referencing LaTeX fragments when previewing them.

PROCESSING-TYPE is the type of process used to create the
preview, see `org-latex-preview-default-process'.

STRING is the string to be hashed, typically the contents of a
LaTeX fragment.

IMAGETYPE is the type of image to be created, see
`org-latex-preview-process-alist'.

FG and BG are the foreground and background colors for the
image.

NUMBER is the equation number that should be used, if applicable."
  (sha1 (prin1-to-string
         (list processing-type
               preamble
               org-latex-preview-options
               string
               (if (equal imagetype "svg")
                   'svg fg)
               bg
               number))))

(defconst org-latex-preview--numbered-environments
  '("equation" "eqnarray" "math" "displaymath" ; latex.ltx
    "align" "gather" "multiline" "flalign" "alignat" ; amsmath.sty
    "xalignat" "xxalignat" "subequations" ; amsmath.sty
    "dmath" "dseries" "dgroup" "darray" ; breqn.sty
    "empheq") ; empheq.sty
  "List of LaTeX environments which produce numbered equations.")

(defun org-latex-preview--environment-numbering-table (&optional parse-tree)
  "Creat a hash table from numbered equations to their initial index.
If the org-element cache is active or PARSE-TREE is provided, the
hash table will use `eq' equality, otherwise `equal' will be
used.  When PARSE-TREE is provided, it is passed onto
`org-latex-preview--get-numbered-environments'."
  (let ((table (make-hash-table :test (if (or parse-tree (org-element--cache-active-p))
                                          #'eq #'equal)))
        (counter 1))
    (save-match-data
      (dolist (element (org-latex-preview--get-numbered-environments
                        nil nil parse-tree))
        (let ((content (org-element-property :value element)))
          (puthash element counter table)
          (if (string-match-p "\\`\\\\begin{[^}]*align" content)
              (let ((beg (org-element-property :begin element))
                    (end (org-element-property :end element)))
                (cl-incf counter (1+ (how-many "\\\\$" beg end)))
                (cl-decf counter (how-many "\\nonumber" beg end))
                (cl-decf counter (how-many "\\tag{" beg end)))
            (unless (or (string-match-p "\\nonumber" content)
                        (string-match-p "\\tag{" content))
              (cl-incf counter))))))
    table))

(defun org-latex-preview--get-numbered-environments (&optional beg end parse-tree)
  "Find all numbered environments between BEG and END.
If PARSE-TREE is provided, it will be used insead of
`org-element-cache-map' or `org-element-parse-buffer'."
  (cond
   (parse-tree
    (org-element-map
        parse-tree
        '(latex-environment)
      (lambda (datum)
        (and (<= (or beg (point-min)) (org-element-property :begin datum)
                 (org-element-property :end datum) (or end (point-max)))
             (let* ((content (org-element-property :value datum))
                    (env (and (string-match "\\`\\\\begin{\\([^}]+\\)}" content)
                              (match-string 1 content))))
               (and (member env org-latex-preview--numbered-environments)
                    datum))))))
   ((org-element--cache-active-p)
    (org-element-cache-map
       (lambda (datum)
         (and (<= (or beg (point-min)) (org-element-property :begin datum)
                  (org-element-property :end datum) (or end (point-max)))
              (let* ((content (org-element-property :value datum))
                     (env (and (string-match "\\`\\\\begin{\\([^}]+\\)}" content)
                               (match-string 1 content))))
                (and (member env org-latex-preview--numbered-environments)
                     datum))))
       :granularity 'element
       :restrict-elements '(latex-environment)
       :from-pos beg
       :to-pos (or end (point-max-marker))))
   (t
    (org-element-map
        (org-element-parse-buffer 'element)
        '(latex-environment)
      (lambda (datum)
        (and (<= (or beg (point-min)) (org-element-property :begin datum)
                 (org-element-property :end datum) (or end (point-max)))
             (let* ((content (org-element-property :value datum))
                    (env (and (string-match "\\`\\\\begin{\\([^}]+\\)}" content)
                              (match-string 1 content))))
               (and (member env org-latex-preview--numbered-environments)
                    (save-excursion
                      (goto-char (org-element-property :begin datum))
                      (org-element-context))))))))))

(cl-defun org-latex-preview--create-image-async (processing-type fragments-info &key latex-processor latex-preamble place-preview-p)
  "Preview PREVIEW-STRINGS asynchronously with method PROCESSING-TYPE.

FRAGMENTS-INFO is a list of plists, each of which provides
information on an individual fragment and should have the
following structure:
  (:string fragment-string :overlay fragment-overlay :key fragment-hash)
where
- fragment-string is the literal content of the fragment
- fragment-overlay is the overlay placed for the fragment
- fragment-hash is a string that uniquely identifies the fragment

It is worth noting the FRAGMENTS-INFO plists will be modified
during processing to hold more information on the fragments.

When PLACE-PREVIEW-P is true, it will be set in the extended info
plist passed to filters, and is expected to result in the newly
generated fragment image being placed in the buffer.

LATEX-PROCESSOR is a member of `org-latex-compilers' which is guessed if unset.

When provided, LATEX-PREAMBLE overrides the default LaTeX preamble.

Returns a list of async tasks started."
  (let* ((processing-type
          (or processing-type org-latex-preview-default-process))
         (latex-processor
          (or latex-processor
              (and (derived-mode-p 'org-mode)
                   (cdr (assoc "LATEX_COMPILER"
                               (org-collect-keywords
                                '("LATEX_COMPILER") '("LATEX_COMPILER")))))
              org-latex-compiler))
         (processing-info
          (nconc (list :latex-processor latex-processor
                       :latex-header latex-preamble)
                 (alist-get processing-type org-latex-preview-process-alist)))
         (programs (plist-get processing-info :programs))
         (error-message (or (plist-get processing-info :message) "")))
    ;; xelatex produces .xdv (eXtended dvi) files, not .dvi, so as a special
    ;; case we check for xelatex + dvi and if so switch the file extension to xdv.
    (when (and (equal latex-processor "xelatex")
               (equal (plist-get processing-info :image-input-type) "dvi"))
      (setq processing-info
            (plist-put (copy-sequence processing-info) :image-input-type "xdv")))
    (dolist (program programs)
      (org-check-external-command program error-message))
    (when org-latex-preview-processing-indicator
      (dolist (fragment fragments-info)
        (org-latex-preview--indicate-processing
         (plist-get fragment :overlay) 'on)))
    ;; At this point we will basically construct a tree of async calls:
    ;;
    ;; dvisvgm case:
    ;; └─ Compile tex file ⟶ stdout to `org-latex-preview--latex-preview-filter'
    ;;    └─ (success or failure)
    ;;       └─Extact images ⟶ stdout to `org-latex-preview--dvisvgm-filter'
    ;;         ├─ (success)
    ;;         │  ├─ Call `org-latex-preview--check-all-fragments-produced',
    ;;         │  │  which can rerun the async tree if needed.
    ;;         │  └─ Delete tempfiles (`org-latex-preview--cleanup-callback').
    ;;         └─ (failure)
    ;;            ├─ Run `org-latex-preview--failure-callback' (remove overlays).
    ;;            └─ Message "creating latex previews failed. please see %s for details".
    ;;
    ;; dvipng case:
    ;; ├─ Compile tex file ⟶ stdout to `org-latex-preview--latex-preview-filter'
    ;; └─ Extract images ("--follow" tex ouput) ⟶ stdout to `org-latex-preview--dvipng-filter'
    ;;    ├─ (success)
    ;;    │  ├─ Call `org-latex-preview--check-all-fragments-produced',
    ;;    │  │  which can rerun the async tree if needed.
    ;;    │  └─ Delete tempfiles (`org-latex-preview--cleanup-callback')
    ;;    └─ (failure)
    ;;       ├─ Run `org-latex-preview--failure-callback' (remove overlays).
    ;;       └─ Message "creating latex previews failed. please see %s for details".
    ;;
    ;; generic case:
    ;; └─ Compile tex file ⟶ stdout to `org-latex-preview--latex-preview-filter'
    ;;    └─ (success or failure)
    ;;       └─Extact images
    ;;         ├─ (success)
    ;;         │  ├─ Call `org-latex-preview--generic-callback'.
    ;;         │  ├─ Delete tempfiles (`org-latex-preview--cleanup-callback')
    ;;         │  └─ Call `org-latex-preview--check-all-fragments-produced',
    ;;         │     which can rerun the async tree if needed.
    ;;         └─ (failure)
    ;;            ├─ Run `org-latex-preview--failure-callback' (remove overlays).
    ;;            └─ Message "creating latex previews failed. please see %s for details".
    ;;
    ;; With continuous, synchronous processing:
    ;;
    ;; ⟶ stdout to `org-latex-preview--latex-preview-filter'
    ;;   ├─ read preview fontsize
    ;;   └─ capture compilation errors
    ;;
    ;; ⟶ stdout to `org-latex-preview--dvisvgm-filter'
    ;;   ├─ read preview image metadata
    ;;   ├─ edit svgs and adjust colors
    ;;   ├─ cache svgs with org-persist or in /tmp
    ;;   └─ update overlays in buffer with svg images and metadata
    ;;
    ;; ⟶ stdout to `org-latex-preview--dvipng-filter'
    ;;   ├─ read preview image metadata
    ;;   ├─ cache pngs with org-persist or in /tmp
    ;;   └─ update overlays in buffer with png images and metadata
    ;;
    (let* ((extended-info
            (append processing-info
                    (list :processor processing-type
                          :fragments fragments-info
                          :org-buffer (current-buffer)
                          :texfile (org-latex-preview--create-tex-file
                                    processing-info fragments-info)
                          :place-preview-p place-preview-p)))
           (tex-compile-async
            (org-latex-preview--tex-compile-async extended-info))
           (img-extract-async
            (org-latex-preview--image-extract-async extended-info)))
      (plist-put (cddr img-extract-async) :success
                 (list ; The order is important here.
                  #'org-latex-preview--check-all-fragments-produced
                  #'org-latex-preview--cleanup-callback))
      (plist-put (cddr img-extract-async) :failure
                 (list
                  #'org-latex-preview--failure-callback
                  (format "Creating LaTeX preview images failed (exit code %%d). Please see %s for details"
                          (propertize org-latex-preview--image-log 'face 'warning))
                  #'org-latex-preview--cleanup-callback))
      (when org-latex-preview-process-finish-functions
        ;; Extra callbacks to run after image generation
        (push #'org-latex-preview--run-finish-functions
              (plist-get (cddr img-extract-async) :success))
        (push #'org-latex-preview--run-finish-functions
              (plist-get (cddr img-extract-async) :failure)))
      (pcase processing-type
        ('dvipng
         (plist-put (cddr img-extract-async) :filter
                    #'org-latex-preview--dvipng-filter))
        ('dvisvgm
         (plist-put (cddr img-extract-async) :filter
                    #'org-latex-preview--dvisvgm-filter))
        (_
         (plist-put (cddr img-extract-async) :success
                    (list ; The order is important here.
                     #'org-latex-preview--generic-callback
                     #'org-latex-preview--cleanup-callback
                     #'org-latex-preview--check-all-fragments-produced))))
      (if (and (eq processing-type 'dvipng)
               (member "--follow" (cadr img-extract-async)))
        (list (org-async-call tex-compile-async)
              (org-async-call img-extract-async))
        (plist-put (cddr tex-compile-async) :success img-extract-async)
        (plist-put (cddr tex-compile-async) :failure img-extract-async)
        (list (org-async-call tex-compile-async))))))

(defun org-latex-preview--run-finish-functions (&rest args)
  "Run hooks after preview image generation."
  (apply #'run-hook-with-args
         'org-latex-preview-process-finish-functions
         args))

(defun org-latex-preview--failure-callback (_exit _buf extended-info)
  "Clear overlays corresponding to previews that failed to generate.

EXTENDED-INFO contains the information needed to identify such
previews."
  (cl-loop for fragment in (plist-get extended-info :fragments)
           for path = (plist-get fragment :path)
           when (not path)
           for ov = (plist-get fragment :overlay)
           when ov do
           ;; ;TODO: Other options here include:
           ;; ;Fringe marker
           ;; (overlay-put ov 'before-string
           ;;              (propertize "!" 'display
           ;;                          `(left-fringe exclamation-mark
           ;;                            warning)))
           ;; ;Special face
           ;; (unless (overlay-get ov 'face)
           ;;   (overlay-put ov 'face 'org-latex-preview-processing-face))
           ;;
           ;; ;Note: ov has buffer extended-info, no need to set current-buffer
           (delete-overlay ov)))

(defvar-local org-latex-preview--preamble-content nil
  "Cache of the LaTeX preamble for snippet preview.")

(defun org-latex-preview--clear-preamble-cache ()
  "Set `org-latex-preview--preamble-content' to nil."
  (setq org-latex-preview--preamble-content nil))

(add-hook 'org-mode-hook #'org-latex-preview--clear-preamble-cache)

(defconst org-latex-preview--single-eqn-format
  "\n\\makeatletter
\\renewcommand{\\theequation}{\\(\\diamond\\)\\ifnum\\value{equation}>1%
\\,+\\,\\@arabic{\\numexpr\\value{equation}-1\\relax}\\fi}
\\makeatother"
  "A LaTeX preamble snippet that sets \"◇\"-based equation numbers.")

(defun org-latex-preview--get-preamble (&optional buf)
  "Obtain the LaTeX preview for snippet preview in BUF."
  (with-current-buffer (or buf (current-buffer))
    (org-fold-core-ignore-modifications
      (let ((org-inhibit-startup t)
            (info (org-combine-plists
                   (org-export--get-export-attributes
                    (org-export-get-backend 'latex))
                   (org-export--get-buffer-attributes)
                   '(:time-stamp-file nil)))
            org-export-use-babel
            org-latex-precompile
            ;; (org-latex-conditional-features
            ;;  (cl-remove-if
            ;;   (lambda (feat)
            ;;     (plist-get (alist-get (cdr feat)
            ;;                           org-latex-feature-implementations)
            ;;                :not-preview))
            ;;   org-latex-conditional-features))
            )
        (org-export-with-buffer-copy
         :drop-narrowing t
         (font-lock-mode -1)
         (setq info
               (org-export--annotate-info (org-export-get-backend 'latex) info))
         (concat
          (org-latex-make-preamble
           (org-combine-plists
            (org-export-get-environment
             (org-export-get-backend 'latex))
            '(:time-stamp-file nil))
           org-latex-preview-preamble 'snippet)
          (and (not org-latex-preview-numbered)
               org-latex-preview--single-eqn-format)))))))

(defun org-latex-preview--create-tex-file (processing-info fragments)
  "Create a LaTeX file based on PROCESSING-INFO and FRAGMENTS.

More specifically, a preamble will be generated based on
PROCESSING-INFO.  Then, if `org-latex-preview-use-precompilation' is
non-nil, a precompiled format file will be generated if needed
and used.  Otherwise the preamble is used normally.

Within the body of the created LaTeX file, each of
FRAGMENTS will be placed in order, wrapped within a
\"preview\" environment.

The path of the created LaTeX file is returned."
  (let* ((header
          (concat
           (plist-get processing-info :latex-header)
           (let ((w org-latex-preview-width))
             (cond
              ((stringp w)
               (format "\n\\setlength{\\textwidth}{%s}\n" w))
              ((and (floatp w) (<= 0.0 w 1.0))
               (format "\n\\setlength{\\textwidth}{%s\\paperwidth}\n" w))))
           "\n\\usepackage[active,tightpage,auctex]{preview}\n"))
         (relative-file-p
          (string-match-p "\\(?:\\\\input{\\|\\\\include{\\)[^/]" header))
         (remote-file-p (file-remote-p default-directory))
         (tex-temp-name
          (expand-file-name
           (concat (make-temp-name "org-tex-") ".tex")
           (and remote-file-p temporary-file-directory)))
         (write-region-inhibit-fsync t)
         (coding-system-for-write buffer-file-coding-system))
    (when (and relative-file-p remote-file-p)
      (error "Org LaTeX Preview does not currently support \\input/\\include in remote files"))
    (when org-latex-preview-use-precompilation
      (if-let ((format-file (org-latex-preview-precompile processing-info header)))
          ;; Replace header with .fmt file path.
          (setq header (concat "%& " (file-name-sans-extension format-file)))
        (display-warning
         '(org latex-preview disable-local-precompile)
         (concat "Precompile failed, disabling LaTeX preview precompile in this buffer."
                 "\n  To renable, run `(setq-local org-latex-preview-use-precompilation t)' or reopen this buffer."
                 (pcase (plist-get processing-info :latex-processor)
                   ("lualatex"
                    "\n  LuaLaTeX is known to be problematic, if you might be able to help please get in touch with emacs-orgmode@gnu.org.")
                   ("xelatex"
                    ;; Note: <https://tex.stackexchange.com/questions/395965/precompile-with-xelatex-and-fontspec> might be helpful.
                    "\n  The current XeTeX approach does not support fontspec, if you might be able to help please get in touch with emacs-orgmode@gnu.org."))
                 "\n "))
        (setq-local org-latex-preview-use-precompilation nil)))
    (with-temp-file tex-temp-name
      (insert header)
      ;; The \abovedisplayskip length must be set after \begin{document} because
      ;; it is usually set during the font size intialisation that occurs at
      ;; \begin{document}.  We can either modify the \normalsize command to set
      ;; the \abovedisplayskip length, or just set it after \begin{document}.
      (insert "\n\\begin{document}\n\n"
              "\\setlength\\abovedisplayskip{0pt}"
              " % Remove padding before equation environments.\n\n")
      (dolist (fragment-info fragments)
        (insert
         "\n\\begin{preview}\n"
         (plist-get fragment-info :string)
         "\n\\end{preview}\n"))
      (insert "\n\\end{document}\n"))
    tex-temp-name))

(defun org-latex-preview--tex-compile-async (extended-info)
  "Create an `org-async-call' spec to compile the texfile in EXTENDED-INFO."
  (let* ((tex-process-buffer
          (with-current-buffer
              (get-buffer-create org-latex-preview--latex-log)
            (erase-buffer)
            (current-buffer)))
         (tex-compile-command-fmt
          (pcase (plist-get extended-info :latex-compiler)
            ((and (pred stringp) cmd) cmd)
            ((and (pred consp) cmds)
             (when (> (length cmds) 1)
               (warn "Preview :latex-compiler must now be a single command.  %S will be ignored."
                     (cdr cmds)))
             (car cmds))))
         (texfile (plist-get extended-info :texfile))
         (org-tex-compiler
          (cdr (assoc (plist-get extended-info :latex-processor)
                      org-latex-preview-compiler-command-map)))
         (tex-command-spec
          `((?o . ,(shell-quote-argument temporary-file-directory))
            (?b . ,(shell-quote-argument (file-name-base texfile)))
            (?f . ,(shell-quote-argument texfile))
            (?l . ,org-tex-compiler)
            (?L . ,(car (split-string org-tex-compiler)))))
         (tex-formatted-command
          (split-string-shell-command
           (format-spec tex-compile-command-fmt tex-command-spec))))
    (unless org-tex-compiler
      (user-error "No `org-latex-preview-compiler-command-map' entry found for LaTeX processor %S, it should be a member of `org-latex-compilers' %S"
                  (plist-get extended-info :latex-processor)
                  org-latex-compilers))
    (list 'org-async-task
          tex-formatted-command
          :buffer tex-process-buffer
          :info extended-info
          :filter #'org-latex-preview--latex-preview-filter
          :failure
          (lambda (exit-code _buf _info)
            ;; With how preview.sty works, an exit code of 1 is expectd.
            (unless (eq exit-code 1)
              (message "LaTeX compilation for preview failed (error code %d). Please see %s for details"
                       exit-code
                       (propertize org-latex-preview--latex-log
                                   'face 'warning)))))))

(defun org-latex-preview--image-extract-async (extended-info)
  "Create an `org-async-call' spec to extract images according to EXTENDED-INFO."
  (let* ((img-process-buffer
          (with-current-buffer
              (get-buffer-create org-latex-preview--image-log)
            (erase-buffer)
            (current-buffer)))
         (img-extract-command
          (pcase
              (or (and (string= (plist-get org-latex-preview-options :background)
                                "Transparent")
                       (plist-get extended-info :transparent-image-converter))
                  (plist-get extended-info :image-converter))
            ((and (pred stringp) cmd) cmd)
            ((and (pred consp) cmds)
             (when (> (length cmds) 1)
               (warn "Preview converter must now be a single command.  %S will be ignored."
                     (cdr cmds)))
             (car cmds))))
         (dpi (* 1.4 ; This factor makes it so generated PNGs are not blury
                     ; at the displayed resulution.
                 (or (plist-get org-latex-preview-options :scale) 1.0)
                 (if (display-graphic-p)
                     (org-latex-preview--get-display-dpi)
                   140.0)))
         (texfile (plist-get extended-info :texfile))
         (texfile-base (file-name-base texfile))
         (img-command-spec
          `((?o . ,(shell-quote-argument temporary-file-directory))
            (?b . ,(shell-quote-argument (file-name-base texfile)))
            (?B . ,(shell-quote-argument
                    (expand-file-name texfile-base temporary-file-directory)))
            (?D . ,(shell-quote-argument (format "%s" dpi)))
            (?f . ,(shell-quote-argument
                    (expand-file-name
                     (concat texfile-base
                             "." (plist-get extended-info :image-input-type))
                     temporary-file-directory)))))
         (img-formatted-command
          (split-string-shell-command
           (format-spec img-extract-command img-command-spec))))
    (list 'org-async-task
          img-formatted-command
          :buffer img-process-buffer
          :info extended-info
          :failure
          (format "Creating LaTeX preview images failed (exit code %%d). Please see %s for details"
                  (propertize org-latex-preview--image-log 'face 'warning)))))

(defun org-latex-preview--cleanup-callback (_exit-code _stdout extended-info)
  "Schedule cleanup with EXTENDED-INFO."
  (run-with-idle-timer
   1.0 nil
   #'org-latex-preview--do-cleanup
   extended-info))

(defun org-latex-preview--do-cleanup (extended-info)
  "Delete files after image creation, in accords with EXTENDED-INFO."
  (let* ((texfile (plist-get extended-info :texfile))
         (outputs-no-ext (expand-file-name (file-name-base texfile)
                                           temporary-file-directory))
         (images
          (mapcar
           (lambda (fragment-info)
             (plist-get fragment-info :path))
           (plist-get extended-info :fragments)))
         (clean-exts
          (or (plist-get extended-info :post-clean)
              '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
                ".svg" ".png" ".jpg" ".jpeg" ".out"))))
    (when (file-exists-p texfile) (delete-file texfile))
    (dolist (img images)
      (and img (delete-file img)))
    (dolist (ext clean-exts)
      (when (file-exists-p (concat outputs-no-ext ext))
        (delete-file (concat outputs-no-ext ext))))))

(defun org-latex-preview--generic-callback (_exit-code _stdout extended-info)
  "Place generated images, in accords with EXTENDED-INFO."
  (let* ((texfile (plist-get extended-info :texfile))
         (outputs-no-ext (expand-file-name (file-name-base texfile)
                                           temporary-file-directory))
         (images
          (file-expand-wildcards
           (concat outputs-no-ext "*." (plist-get extended-info :image-output-type))
           'full)))
    (save-excursion
      (cl-loop
       for fragment-info in (plist-get extended-info :fragments)
       for image-file in images
       for ov = (plist-get fragment-info :overlay)
       for cached-img =
       (org-latex-preview--cache-image
        (plist-get fragment-info :key)
        image-file
        (org-latex-preview--display-info
         extended-info fragment-info))
       do
       (plist-put fragment-info :path image-file)
       (when (plist-get extended-info :place-preview-p)
         (org-latex-preview--update-overlay ov cached-img))))))

(defun org-latex-preview--check-all-fragments-produced (_exit-code _stdout extended-info)
  "Check each of the fragments in EXTENDED-INFO has a path.
Should this not be the case, the fragment immediately before the first
fragment without a path is marked as erronious, and the remaining
fragments are regenerated."
  (let ((fragments (cons nil (copy-sequence (plist-get extended-info :fragments)))))
    (while (cdr fragments)
      (if (or (plist-get (cadr fragments) :path)
              (plist-get (cadr fragments) :error))
          (setq fragments (cdr fragments))
        ;; If output ends prematurely, this is most likely due to an issue with
        ;; the last "succesfully" produced fragment, and so we mark it as erronious
        ;; and attempt to re-generate the rest.
        (let ((bad-fragment (car fragments))
              (bad-fragment-err (plist-get (car fragments) :errors)))
          (plist-put bad-fragment :errors
                     (concat bad-fragment-err
                             (and bad-fragment-err "\n\n")
                             "Preview generation catastrophically failed after this fragment."))
          (org-latex-preview--remove-cached
           (plist-get bad-fragment :key))
          (org-latex-preview--update-overlay
           (plist-get bad-fragment :overlay)
           (org-latex-preview--cache-image
            (plist-get bad-fragment :key)
            (plist-get bad-fragment :path)
            (org-latex-preview--display-info
             extended-info bad-fragment))))
        ;; Re-generate the remaining fragments.
        (org-latex-preview--create-image-async
         (plist-get extended-info :processor)
         (cdr fragments)
         :place-preview-p t)
        (setq fragments nil)))))

(defun org-latex-preview--display-info (extended-info fragment-info)
  "From FRAGMENT-INFO and EXTENDED-INFO obtain display-relevant information."
  (let ((image-type (intern (plist-get extended-info :image-output-type)))
        info)
    (setq info (plist-put info :image-type image-type))
    (dolist (key '(:width :height :depth))
      (when-let ((val (plist-get fragment-info key)))
        (plist-put info key val)))
    (plist-put info :errors (plist-get fragment-info :errors))
    info))

;TODO: Figure out why this factor is needed.
(defconst org-latex-preview--shameful-magic-tex-scaling-factor
  1.01659593
  "Extra factor for aligning preview image baselines.

Sometimes a little sprinkling of pixie dust is needed to get
things just right.  Even just 2.7% magic can suffice.

This is the ratio of image sizes as reported by preview.sty and
computed by dvisvgm.  The latter is correct.")

(defconst org-latex-preview--tex-scale-divisor
  (* 65781.76 org-latex-preview--shameful-magic-tex-scaling-factor)
  "Base pt to point conversion for preview.sty output.

This is the product of three scaling quantities:

- Point to scaled point ratio: 1:65536
- Base point to scaled point ratio: 72:72.27
- The magic scaling factor
  (see `org-latex-preview--shameful-magic-tex-scaling-factor').")

(defun org-latex-preview--latex-preview-filter (_proc _string extended-info)
  "Examine the stdout from LaTeX compilation with preview.sty.

- The detected fontsize is directly entered into EXTENDED-INFO.
- The tightpage bounds information is captured and stored in EXTENDED-INFO.
- Fragment geometry and alignment info is computed using the
  tightpage info and page geometry reported by preview.sty.
- Fragment errors are put into the :errors slot of the relevant
fragments in EXTENDED-INFO."
  (unless (plist-get extended-info :fontsize)
    (when (save-excursion
            (re-search-forward "^Preview: Fontsize \\([0-9]+\\)pt$" nil t))
      (plist-put extended-info :fontsize (string-to-number (match-string 1)))
      ;; Since at this point can infer that the preamble logging is complete,
      ;; we can also check for hyperref and warn if it seems to be used,
      ;; as it is currently known to cause issues.
      (save-excursion
        (goto-char (point-min))
        (when (if (and org-latex-preview-use-precompilation
                       (re-search-forward "^PRELOADED FILES:" nil t))
                  (re-search-forward "^ *hyperref\\.sty" nil t)
                (re-search-forward "^(.*hyperref/hyperref\\.sty" nil t))
          (display-warning
           '(org latex-preview hyperref)
           "Hyperref seems to be loaded, this is known to cause issues with the reported size information"
           :warning)))))
  (let ((preview-start-re
         "^! Preview: Snippet \\([0-9]+\\) started.\n<-><->\n *\nl\\.\\([0-9]+\\)[^\n]+\n")
        (preview-end-re
         "\\(?:^Preview: Tightpage.*$\\)?\n! Preview: Snippet [0-9]+ ended.(\\([0-9]+\\)\\+\\([0-9]+\\)x\\([0-9]+\\))")
        (fragments (plist-get extended-info :fragments))
        (tightpage-info (plist-get extended-info :tightpage))
        preview-marks)
    (beginning-of-line)
    (save-excursion
      (while (re-search-forward preview-start-re nil t)
        (push (list (match-beginning 0)
                    (match-end 0)
                    (string-to-number (match-string 1)) ; Preview index.
                    (1+ (string-to-number (match-string 2)))) ; Base line number.
              preview-marks)))
    (setq preview-marks (nreverse preview-marks))
    (while preview-marks
      (goto-char (caar preview-marks))
      ;; Check for tightpage-info, as long as XeLaTeX is not being used,
      ;; as it seems to behave differently to pdfLaTeX and luaLaTeX and
      ;; produces an image without the margins that tightpage reports.
      (unless (or tightpage-info (equal (plist-get extended-info :latex-processor)
                                        "xelatex"))
        (save-excursion
          (when (re-search-forward
                 "^Preview: Tightpage \\(-?[0-9]+\\) *\\(-?[0-9]+\\) *\\(-?[0-9]+\\) *\\(-?[0-9]+\\)"
                                   (or (caadr preview-marks) (point-max)) t)
            (setq tightpage-info
                  (mapcar #'string-to-number
                          ;; left-margin bottom-margin
                          ;; right-margin top-margin
                          (list (match-string 1) (match-string 2)
                                (match-string 3) (match-string 4))))
            (plist-put extended-info :tightpage tightpage-info))))
      ;; Check for processed fragment
      (if (re-search-forward preview-end-re (or (caadr preview-marks) (point-max)) t)
          (let ((fragment-info (nth (1- (nth 2 (car preview-marks))) fragments))
                (errors-substring
                 (save-match-data
                   (string-trim
                    (buffer-substring (cadar preview-marks)
                                      (match-beginning 0));; In certain situations we can end up with non-error
                    ;; logging informattion within the preview output.
                    ;; To make sure this is not captured, we rely on the fact
                    ;; that LaTeX error messages have a consistent format
                    ;; and start with an exclamation mark "!".  Thus, we
                    ;; can safely strip everything prior to the first "!"
                    ;; from the output.
                    "[^!]*")))
                depth)
            ;; Gather geometry and alignment info
            (if tightpage-info
                (progn
                  (setq depth
                        (/ (- (string-to-number (match-string 2))
                              (nth 1 tightpage-info))
                           org-latex-preview--tex-scale-divisor
                           (or (plist-get fragment-info :fontsize) 10)))
                  (plist-put fragment-info :depth depth)
                  (plist-put fragment-info :height
                             (+ (or depth 0)
                                (/ (+ (string-to-number (match-string 1))
                                      (nth 3 tightpage-info))
                                   org-latex-preview--tex-scale-divisor
                                   (or (plist-get fragment-info :fontsize) 10))))
                  (plist-put fragment-info :width
                             (/ (+ (string-to-number (match-string 3))
                                   (nth 2 tightpage-info)
                                   (- (nth 1 tightpage-info)))
                                org-latex-preview--tex-scale-divisor
                                (or (plist-get fragment-info :fontsize) 10))))
              (cl-loop for (geom . match-index)
                       in '((:height . 1) (:depth . 2) (:width . 3))
                       do
                       (plist-put fragment-info geom
                                  (/ (string-to-number (match-string match-index))
                                     org-latex-preview--tex-scale-divisor
                                     (or (plist-get fragment-info :fontsize) 10)))))
            (plist-put fragment-info :errors
                       (and (not (string-blank-p errors-substring))
                            (replace-regexp-in-string
                             "^l\\.[0-9]+"
                             (lambda (linum)
                               (format "l.%d"
                                       (- (string-to-number (substring linum 2))
                                          (nth 3 (car preview-marks)))))
                             errors-substring))))
        (goto-char (caar preview-marks)))
      (setq preview-marks (cdr preview-marks)))))

(defun org-latex-preview--dvisvgm-filter (_proc _string extended-info)
  "Look for newly created images in the dvisvgm stdout buffer.
Any matches found will be matched against the fragments recorded in
EXTENDED-INFO, and displayed in the buffer."
  (let ((dvisvgm-processing-re "^processing page \\([0-9]+\\)\n")
        (fragments (plist-get extended-info :fragments))
        page-marks fragments-to-show)
    (beginning-of-line)
    (save-excursion
      (while (re-search-forward dvisvgm-processing-re nil t)
        (push (cons (string-to-number (match-string 1))
                    (match-beginning 0))
              page-marks)))
    (setq page-marks (nreverse page-marks))
    (while page-marks
      (let ((start (cdar page-marks))
            (end (or (cdadr page-marks) (point-max)))
            (page (caar page-marks))
            fragment-info)
        (goto-char start)
        (when (save-excursion
                (re-search-forward "output written to \\(.*.svg\\)$" end t))
          (setq fragment-info (nth (1- page) fragments))
          (plist-put fragment-info :path (match-string 1))
          (when (save-excursion
                  (re-search-forward "^  page is empty" end t))
            (unless (plist-get fragment-info :error)
              (plist-put fragment-info :error "Image file not produced."))
            (plist-put fragment-info :path nil))
          (push fragment-info fragments-to-show)
          (goto-char end)))
      (setq page-marks (cdr page-marks)))
    (when fragments-to-show
      (setq fragments-to-show (nreverse fragments-to-show))
      (mapc #'org-latex-preview--svg-make-fg-currentColor fragments-to-show)
      (if (plist-get extended-info :place-preview-p)
          (org-latex-preview--place-images extended-info fragments-to-show)
        (dolist (fragment-info fragments-to-show)
          (org-latex-preview--cache-image
           (plist-get fragment-info :key)
           (plist-get fragment-info :path)
           (org-latex-preview--display-info
            extended-info fragment-info)))))))

(defun org-latex-preview--svg-make-fg-currentColor (svg-fragment)
  "Replace the foreground color in SVG-FRAGMENT's file with \"currentColor\".
The foreground color is guessed to be the first specified <g>
fill color, which appears to be a reliable heuristic from a few
tests with the output of dvisvgm."
  (let ((write-region-inhibit-fsync t)
        ;; dvisvgm produces UTF-8 encoded files, so we might as well
        ;; avoid calling `find-auto-coding'.
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        ;; Prevent any file handlers (specifically
        ;; `image-file-handler') from being called.
        (file-name-handler-alist nil)
        (path (plist-get svg-fragment :path)))
    (catch 'svg-exists
      (dotimes (_ 1000) ; Check for svg existance over 1s.
        (when (file-exists-p path)
          (throw 'svg-exists t))
        (sleep-for 0.001)))
    (when path
      (with-temp-buffer
        (insert-file-contents path)
        (unless ; When the svg is incomplete, wait for it to be completed.
            (string= (buffer-substring (max 1 (- (point-max) 6)) (point-max))
                     "</svg>")
          (catch 'svg-complete
            (dotimes (_ 1000) ; Check for complete svg over 1s.
              (if (string= (buffer-substring (max 1 (- (point-max) 6)) (point-max))
                           "</svg>")
                  (throw 'svg-complete t)
                (erase-buffer)
                (sleep-for 0.001)
                (insert-file-contents path)))
            (erase-buffer)))
        (goto-char (point-min))
        (if (or (= (buffer-size) 0)
                (re-search-forward "<svg[^>]*>\n<g[^>]*>\n</svg>" nil t))
            ;; We never want to show an empty SVG, instead it is better to delete
            ;; it and leave the LaTeX fragment without an image overlay.
            ;; This also works better with other parts of the system, such as
            ;; the display of errors.
            (delete-file path)
          (while (re-search-forward org-latex-preview--svg-fg-standin nil t)
            (replace-match "currentColor" t t))
          (write-region nil nil path nil 0))))))

(defun org-latex-preview--dvipng-filter (_proc _string extended-info)
  "Look for newly created images in the dvipng stdout buffer.
Any matches found will be matched against the fragments recorded in
EXTENDED-INFO, and displayed in the buffer."
  (let ((outputs-no-ext (expand-file-name
                         (file-name-base
                          (plist-get extended-info :texfile))
                         temporary-file-directory))
        (fragments (plist-get extended-info :fragments))
        fragments-to-show page-info-end)
    (while (search-forward "]" nil t)
      (setq page-info-end (point))
      (save-excursion
        (backward-list)
        (if (re-search-forward "\\=\\[\\([0-9]+\\) " page-info-end t)
            (let* ((page (string-to-number (match-string 1)))
                   (fragment-info (nth (1- page) fragments)))
              (plist-put fragment-info :path
                         (format "%s-%09d.png" outputs-no-ext page))
              (push fragment-info fragments-to-show)))))
    (when fragments-to-show
      (setq fragments-to-show (nreverse fragments-to-show))
      (if (plist-get extended-info :place-preview-p)
          (org-latex-preview--place-images extended-info fragments-to-show)
        (dolist (fragment-info fragments-to-show)
          (org-latex-preview--cache-image
           (plist-get fragment-info :key)
           (plist-get fragment-info :path)
           (org-latex-preview--display-info
            extended-info fragment-info)))))))

(defun org-latex-preview--place-images (extended-info &optional fragments)
  "Place images for each of FRAGMENTS, according to their data and EXTENDED-INFO.
Should FRAGMENTS not be explicitly provided, all of the fragments
listed in EXTENDED-INFO will be used."
  (let ((fragments (or fragments (plist-get extended-info :fragments))))
    (with-current-buffer (plist-get extended-info :org-buffer)
      (save-excursion
        (cl-loop
         for fragment-info in fragments
         for image-file = (plist-get fragment-info :path)
         for ov = (plist-get fragment-info :overlay)
         do (org-latex-preview--update-overlay
             ov
             (org-latex-preview--cache-image
              (plist-get fragment-info :key)
              image-file
              (org-latex-preview--display-info
               extended-info fragment-info))))))))

(defconst org-latex-preview--cache-name "LaTeX preview cached image data"
  "The name used for Org LaTeX Preview objects in the org-persist cache.")

(defvar org-latex-preview--table nil
  "Hash table to hold LaTeX preview image metadata.

This is only used if image caching is disabled by setting
`org-latex-preview-persist' to nil.")

(defun org-latex-preview--cache-image (key path info)
  "Save the image at PATH with associated INFO in the cache indexed by KEY.
Return (path . info).

The caching location depends on whether preview persistence is
enabled, see `org-latex-preview-persist'."
  (cond
   ((not path)
    (display-warning
     '(org latex-preview put-cache)
     (format "Tried to cache %S without a path, skipping. This should not happen, please report it as a bug to the Org mailing list (M-x org-submit-bug-report)." key)
     :warning))
   (org-latex-preview-persist
    (let ((label-path-info
           (or (org-persist-read org-latex-preview--cache-name
                                 (list :key key)
                                 nil nil :read-related t)
               (org-persist-register `(,org-latex-preview--cache-name
                                       (file ,path)
                                       (elisp-data ,info))
                                     (list :key key)
                                     :expiry org-latex-preview-persist-expiry
                                     :write-immediately t))))
      (cons (cadr label-path-info) info)))
   (t
    (unless org-latex-preview--table
      (setq org-latex-preview--table (make-hash-table :test 'equal :size 240)))
    (when-let ((path)
               (new-path (expand-file-name
                          (concat "org-tex-" key "." (file-name-extension path))
                          temporary-file-directory)))
      (copy-file path new-path 'replace)
      (puthash key (cons new-path info)
               org-latex-preview--table)))))

(defun org-latex-preview--get-cached (key)
  "Retrieve the image path and info associated with KEY.
The result will be of the form (path . info).

Example result:
  (\"/path/.../to/.../image.svg\"
   :type svg
   :height 1.4
   :width 7.6
   :depth 0.2
   :errors nil)"
  (if org-latex-preview-persist
      (when-let ((label-path-info
                  (org-persist-read org-latex-preview--cache-name
                                    (list :key key)
                                    nil nil :read-related t)))
        (cons (cadr label-path-info)
              (caddr label-path-info)))
    (when org-latex-preview--table
      (gethash key org-latex-preview--table))))

(defun org-latex-preview--remove-cached (key)
  "Remove the fragment cache associated with KEY."
  (if org-latex-preview-persist
      (org-persist-unregister org-latex-preview--cache-name
                              (list :key key)
                              :remove-related t)
    (when org-latex-preview--table
      (remhash key org-latex-preview--table)
      (dolist (ext '("svg" "png"))
        (when-let  ((image-file
                     (expand-file-name
                      (concat "org-tex-" key "." ext)
                      temporary-file-directory))
                    ((file-exists-p image-file)))
          (delete-file image-file))))))

(defun org-latex-preview-clear-cache (&optional beg end clear-entire-cache)
  "Clear LaTeX preview cache for fragments between BEG and END.

Interactively, act on
- the region if it is active,
- the fragment at point if in a fragment,
- the whole buffer otherwise.

When CLEAR-ENTIRE-CACHE is non-nil (interactively set by \\[universal-argument]),
the *entire* preview cache will be cleared, and `org-persist-gc' run."
  (interactive
   (if current-prefix-arg
       (list nil nil (y-or-n-p "This will clear the systemwide LaTeX preview cache, continue? "))
     (let ((context (if (derived-mode-p 'org-mode)
                        (org-element-context)
                      (user-error "This command must be run in an org-mode buffer"))))
       (cond
        ((use-region-p)
         (list (region-beginning) (region-end)))
        ((memq (org-element-type context)
               '(latex-fragment latex-environment))
         (list (org-element-property :begin context)
               (org-element-property :end context)))
        (t (list nil nil))))))
  ;; Clear the precompile cache if clearing the whole buffer or everything.
  (when (or clear-entire-cache (not (or beg end)))
    (or org-latex-preview--preamble-content
        (setq org-latex-preview--preamble-content
              (org-latex-preview--get-preamble)))
    (dolist (compiler org-latex-compilers)
      (org-latex--remove-cached-preamble
       compiler org-latex-preview--preamble-content nil)
      (org-latex--remove-cached-preamble
       compiler org-latex-preview--preamble-content t))
    (org-latex-preview--clear-preamble-cache))
  (org-latex-preview-clear-overlays beg end)
  (if clear-entire-cache
      (let ((n 0))
        (dolist (collection org-persist--index)
          (when (equal (cadar (plist-get collection :container))
                       org-latex-preview--cache-name)
            (org-latex-preview--remove-cached
             (plist-get (plist-get collection :associated) :key))
            (cl-incf n)))
        (if (= n 0)
            (message "The Org LaTeX preview cache was already empty.")
          (org-persist-gc)
          (message "Cleared all %d entries fom the Org LaTeX preview cache." n)))
    (let ((imagetype
           (or (plist-get (alist-get org-latex-preview-default-process
                                     org-latex-preview-process-alist)
                          :image-output-type)
               "png"))
          (numbering-table
           (and org-latex-preview-numbered
                (org-latex-preview--environment-numbering-table))))
      (dolist (element (org-latex-preview-collect-fragments beg end))
        (pcase-let* ((begin (or (org-element-property :post-affiliated element)
                                (org-element-property :begin element)))
                     (end (- (org-element-property :end element)
                             (or (org-element-property :post-blank element) 0)
                             (if (eq (char-before (org-element-property :end element))
                                     ?\n)
                                 1 0)))
                     (`(,fg ,bg) (org-latex-preview--colors-around begin end))
                     (value (org-element-property :value element))
                     (number (and numbering-table
                                  (eq (org-element-type element)
                                      'latex-environment)
                                  (gethash element numbering-table))))
          (org-latex-preview--remove-cached
           (org-latex-preview--hash
            org-latex-preview-default-process
            org-latex-preview--preamble-content
            value imagetype fg bg number))))
      (message "Cleared LaTeX preview cache for %s."
               (if (or beg end) "region" "buffer")))))

(defun org-latex-preview-precompile (processing-info preamble &optional tempfile-p)
  "Precompile/dump LaTeX PREAMBLE text.

The path to the format file (.fmt) is returned.  If the format
file could not be found in the persist cache, it is generated
according to PROCESSING-INFO and stored.

If TEMPFILE-P is non-nil, then it is assumed the preamble does
not contain any relative references to other files.

This is intended to speed up Org's LaTeX preview generation
process."
  (org-latex--precompile
   (list :latex-compiler (plist-get processing-info :latex-processor)
         :precompile-format-spec
         (let ((org-tex-compiler
                (cdr (assoc (plist-get processing-info :latex-processor)
                            org-latex-preview-compiler-command-map))))
           `((?l . ,org-tex-compiler)
             (?L . ,(car (split-string org-tex-compiler))))))
   preamble
   tempfile-p))

(defun org-latex-preview--tex-styled (processing-type value options)
  "Apply LaTeX style commands to VALUE based on OPTIONS.
If PROCESSING-TYPE is dvipng, the colours are set with DVI
\"\\special\" commands instead of \"\\color\" and
\"\\pagecolor\".

VALUE is the math fragment text to be previewed.

OPTIONS is the plist `org-latex-preview-options' with customized
color information for this run."
  (let* ((fg (pcase (plist-get options :foreground)
               ('default (org-latex-preview--format-color (org-latex-preview--attr-color :foreground)))
               ((pred null) (org-latex-preview--format-color "Black"))
               (color (org-latex-preview--format-color color))))
         (bg (pcase (plist-get options :background)
               ('default (org-latex-preview--attr-color :background))
               ("Transparent" nil)
               (bg (org-latex-preview--format-color bg))))
         (num (or (plist-get options :number)
                  (and (not (eq org-latex-preview-numbered 'preview))
                       1))))
    (concat (and (not (plist-get options :continue-color))
                 (if (eq processing-type 'dvipng)
                     (concat (and fg (format "\\special{color rgb %s}"
                                             (subst-char-in-string
                                              ?, ?\s fg)))
                             (and bg (format "\\special{background rgb %s}"
                                             (subst-char-in-string
                                              ?, ?\s bg))))
                   (concat
                    (and bg (format "\\pagecolor[rgb]{%s}" bg))
                    (and fg (format "\\color[rgb]{%s}" fg)))))
            (and num (format "\\setcounter{equation}{%d}" (1- num)))
            "%\n"
            value)))

(defun org-latex-preview-place-image-link (link block-type beg end value)
  "Place then link LINK at BEG END."
  (delete-region beg end)
  (insert
   (org-add-props link
       (list 'org-latex-src
             (replace-regexp-in-string "\"" "" value)
             'org-latex-src-embed-type
             (if block-type 'paragraph 'character)))))

(defun org-latex-preview--get-display-dpi ()
  "Get the DPI of the display.
The function assumes that the display has the same pixel width in
the horizontal and vertical directions."
  (if (display-graphic-p)
      (round (/ (display-pixel-height)
                (/ (display-mm-height) 25.4)))
    (error "Attempt to calculate the dpi of a non-graphic display")))

(defun org-latex-preview--attr-color (attr)
  "Return a RGB color for the LaTeX color package."
  (org-latex-preview--format-color (face-attribute 'default attr nil)))

(defvar org-latex-preview--format-color-cache nil
  "Cache for `org-latex-preview--format-color'.
Because `org-latex-preview--format-color' is called multiple
times for every fragment, even though only few colors will be
used it can be worth storing the results to avoid re-computing.")

(defun org-latex-preview--format-color (color-name)
  "Convert COLOR-NAME to a RGB color value."
  (or (alist-get color-name org-latex-preview--format-color-cache nil nil #'equal)
      (cdar (push (cons color-name
                        (apply #'format "%s,%s,%s"
                               (mapcar 'org-latex-preview--normalize-color
                                       (color-values color-name))))
                  org-latex-preview--format-color-cache))))

(defun org-latex-preview--normalize-color (value)
  "Return string to be used as color value for an RGB component."
  (format "%g" (/ value 65535.0)))

(provide 'org-latex-preview)
;;; org-latex-preview.el ends here
