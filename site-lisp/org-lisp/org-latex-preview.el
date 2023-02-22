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

(defgroup org-latex-preview nil
  "Options for generation of LaTeX previews in Org mode."
  :tag "Org LaTeX Preview"
  :group 'org)

;;;###autoload
(defcustom org-latex-preview-options
  '(:foreground auto :background "Transparent" :scale 1.0
    :html-foreground "Black" :html-background "Transparent"
    :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
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
:html-foreground, :html-background, :html-scale
             the same numbers for HTML export.
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
  :type 'plist)

(defcustom org-latex-to-mathml-jar-file nil
  "Value of\"%j\" in `org-latex-to-mathml-convert-command'.
Use this to specify additional executable file say a jar file.

When using MathToWeb as the converter, specify the full-path to
your mathtoweb.jar file."
  :group 'org-latex
  :version "24.1"
  :type '(choice
          (const :tag "None" nil)
          (file :tag "JAR file" :must-match t)))

(defcustom org-latex-to-mathml-convert-command nil
  "Command to convert LaTeX fragments to MathML.
Replace format-specifiers in the command as noted below and use
`shell-command' to convert LaTeX to MathML.
%j:     Executable file in fully expanded form as specified by
        `org-latex-to-mathml-jar-file'.
%I:     Input LaTeX file in fully expanded form.
%i:     The latex fragment to be converted.
%o:     Output MathML file.

This command is used by `org-create-math-formula'.

When using MathToWeb as the converter, set this option to
\"java -jar %j -unicode -force -df %o %I\".

When using LaTeXML set this option to
\"latexmlmath \"%i\" --presentationmathml=%o\"."
  :group 'org-latex
  :version "24.1"
  :type '(choice
          (const :tag "None" nil)
          (string :tag "\nShell command")))

(defcustom org-latex-to-html-convert-command nil
  "Command to convert LaTeX fragments to HTML.
This command is very open-ended: the output of the command will
directly replace the LaTeX fragment in the resulting HTML.
Replace format-specifiers in the command as noted below and use
`shell-command' to convert LaTeX to HTML.
%i:     The LaTeX fragment to be converted.

For example, this could be used with LaTeXML as
\"latexmlc \\='literal:%i\\=' --profile=math --preload=siunitx.sty 2>/dev/null\"."
  :group 'org-latex
  :package-version '(Org . "9.4")
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Shell command")))

(defcustom org-latex-preview-default-process
  (if (with-temp-buffer ; If dvisvgm>=3 installed.
        (and (executable-find "dvisvgm")
             (= (call-process "dvisvgm" nil t nil "--version") 0)
             (version<= "3" (cadr (split-string (buffer-string))))))
      'dvisvgm 'dvipng)
  "The default process to convert LaTeX fragments to image files.
All available processes and theirs documents can be found in
`org-latex-preview-process-alist', which see."
  :group 'org-latex-preview
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'symbol)

;;;###autoload
(defcustom org-latex-preview-process-alist
  '((dvipng
     :programs ("latex" "dvipng")
     :description "dvi > png"
     :message "you need to install the programs: latex and dvipng."
     :image-input-type "dvi"
     :image-output-type "png"
     :image-size-adjust (1.4 . 1.2)
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
     :image-size-adjust (1.4 . 1.2)
     :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
     :latex-precompiler ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
     ;; With dvisvgm the --bbox=preview flag is needed to emit the preview.sty-provided
     ;; height+width+depth information. The --optimise, --clipjoin, and --relative flags
     ;; cause dvisvgm do do some extra work to tidy up the SVG output, but barely add to
     ;; the overall dvisvgm runtime (<1% increace, from testing).
     :image-converter ("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview --scale=%S -o %B-%%9p.svg %f"))
    (imagemagick
     :programs ("pdflatex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: latex and imagemagick."
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.4 . 1.2)
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
  :image-size-adjust  cons of numbers, the car element is used to adjust LaTeX
                      image size showed in buffer and the cdr element is for
                      HTML file.  This option is only useful for process
                      developers, users should use variable
                      `org-latex-preview-options' instead.
  :post-clean         list of strings, files matched are to be cleaned up once
                      the image is generated.  When nil, the files with \".dvi\",
                      \".xdv\", \".pdf\", \".tex\", \".aux\", \".log\", \".svg\",
                      \".png\", \".jpg\", \".jpeg\" or \".out\" extension will
                      be cleaned up.
  :latex-header       list of strings, the LaTeX header of the snippet file.
                      When nil, the fallback value is used instead, which is
                      controlled by `org-latex-preview-header',
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

Place-holders used by `:image-converter', `:latex-precompiler', and `:latex-compiler':

  %f    input file name
  %b    base name of input file
  %o    base directory of input file
  %O    absolute output file name

Place-holders only used by `:latex-precompiler' and `:latex-compiler':

  %l   LaTeX compiler command string
  %L   LaTeX compiler command name

Place-holders only used by `:image-converter':

  %D    dpi, which is used to adjust image size by some processing commands.
  %S    the image size scale ratio, which is used to adjust image size by some
        processing commands."
  :group 'org-latex-preview
  :package-version '(Org . "9.6")
  :type '(alist :tag "LaTeX to image backends"
          :value-type (plist)))

(defcustom org-latex-preview-compiler-command-map
  '(("pdflatex" . "latex")
    ("xelatex" . "xelatex -no-pdf")
    ("lualatex" . "dvilualatex"))
  "A mapping from each of `org-latex-compilers' to command strings.
FIXME elaborate."
  :group 'org-latex-preview
  :package-version '(Org . "9.7")
  :type '(alist :tag "Compiler"
          :value-type (string :type "command")))

(defcustom org-preview-latex-image-directory "ltximg/"
  "Path to store latex preview images.
A relative path here creates many directories relative to the
processed Org files paths.  An absolute path puts all preview
images at the same place."
  :group 'org-latex
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'string)

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
  "Whether to calculate and apply correct equation numbering."
  :group 'org-latex
  :package-version '(Org . "9.7")
  :type 'boolean)

(defcustom org-latex-preview-processing-indicator 'fringe
  "How to indicate LaTeX fragments that are currently being
processed for preview.  This is a symbol with one of three
values:

nil: Do not indicate fragment processing.

face: Apply a special face to fragments that are being processed.
You can customize the face `org-latex-preview-processing-face' to
change how it appears.

fringe: Apply a fringe marker to lines where fragments are being
processed."
  :group 'org-latex
  :package-version '(Org . "9.7")
  :type '(choice
          (const :tag "No indicator" nil)
          (const :tag "Fringe marker" fringe)
          (const :tag "Processing face" face)))

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

(defun org-format-latex-mathml-available-p ()
  "Return t if `org-latex-to-mathml-convert-command' is usable."
  (save-match-data
    (when (and (boundp 'org-latex-to-mathml-convert-command)
               org-latex-to-mathml-convert-command)
      (let ((executable (car (split-string
                              org-latex-to-mathml-convert-command))))
        (when (executable-find executable)
          (if (string-match
               "%j" org-latex-to-mathml-convert-command)
              (file-readable-p org-latex-to-mathml-jar-file)
            t))))))

(defcustom org-latex-preview-header "\\documentclass{article}
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
`org-elemetnt-context' should be used to verify that matches are
indeed LaTeX fragments/environments.")

(defconst org-latex-preview--ignored-faces
  '(org-indent)
  "Faces that should not affect the color of preview overlays.")

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
          (overlay-put ov 'hidden-face nil)   ;(re)store svg face
          ;; Do not set the display property of preview image
          ;; overlays to nil when ensuring that an overlay exists.
          ;; This causes flicker during regeneration as the the
          ;; underlying text is shown and then replaced with the new
          ;; image.
          (overlay-put ov 'preview-image nil) ;(re)store image spec
          (overlay-put ov 'preview-state nil) ;is fragment modified?
          (overlay-put ov 'view-text nil))))
    (unless ov
      (setq ov (make-overlay beg end))
      (overlay-put ov 'org-overlay-type 'org-latex-overlay)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'modification-hooks
                   (list #'org-latex-preview-auto--mark-overlay-modified))
      (overlay-put ov 'insert-in-front-hooks
                   (list #'org-latex-preview-auto--insert-front-handler))
      (overlay-put ov 'insert-behind-hooks
                   (list #'org-latex-preview-auto--insert-behind-handler)))
    ov))

(defsubst org-latex-preview--indicate-processing (ov &optional on)
  "Provide visual indication of LaTeX fragment preview generation.

See `org-latex-preview-processing-indicator' to customize this
behavior."
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
    (overlay-put ov 'preview-state 'modified)
    (overlay-put ov 'display nil)))

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
    (when org-latex-preview-processing-indicator
      (org-latex-preview--indicate-processing ov))
    ;; This is a temporary measure until a more sophisticated
    ;; interface for errors is available in Org.
    (when (and errors tooltip-mode)
      (overlay-put ov 'help-echo errors))
    (when image-display
      (overlay-put ov 'display image-display)
      (overlay-put ov 'preview-image image-display))
    (cond
     ((eq image-type 'svg)
      (overlay-put
       ov 'face
       (or (and errors 'error)
           (and (> (overlay-start ov) (point-min))
                (not (eq (char-before (overlay-start ov)) ?\n))
                (let ((face (get-text-property (1- (overlay-start ov)) 'face)))
                  (cond
                   ((consp face)
                    (cl-set-difference face org-latex-preview--ignored-faces))
                   ((not (memq face org-latex-preview--ignored-faces))
                    face))))
           'default)))
     (errors
      (overlay-put
       ov 'before-string
       (propertize "!" 'display
                   `(left-fringe exclamation-mark error)))))))

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
(defvar-local org-latex-preview-auto--marker (make-marker)
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
            (while (search-forward "\n" end t)
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
                   ;; This is a nice way of quickly checking for a type
                   ;; across all elements, but it does assume that
                   ;; every element is well formed, more so than
                   ;; `org-element-type' in fact.
                   (memq 'latex-environment (mapcar #'car fragments)))
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
                   (elem-beg (or (or (org-element-property :post-affiliated element)
                                     (org-element-property :begin element))
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
    (when (eq (overlay-get ov 'org-overlay-type)
              'org-latex-overlay)
      (overlay-put ov 'display nil)
      (overlay-put ov 'view-text t)
      (when-let ((f (overlay-get ov 'face)))
        (overlay-put ov 'hidden-face f)
        (overlay-put ov 'face nil))
      (org-latex-preview-auto--move-into ov)
      (setq org-latex-preview-auto--from-overlay nil))))

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
        (overlay-put ov 'display (overlay-get ov 'preview-image))))))

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
      (org-latex-preview--place-from-elements
       org-latex-preview-default-process
       (append (list fragment) others)))))

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
  "`org-latex-preview-auto-mode' enables automatic previewing of
latex fragments in Org buffers, and toggles the preview images
when the cursor moves into them.  This allows one to seamlessly
edit and preview latex in Org buffers.

To enable auto-toggling of the preview images without
auto-generating them or vice-versa, customize the variable
`org-latex-preview-auto-generate'."
  :global nil
  (if org-latex-preview-auto-mode
      (progn
        (add-hook 'pre-command-hook #'org-latex-preview-auto--handle-pre-cursor nil 'local)
        (add-hook 'post-command-hook #'org-latex-preview-auto--handle-post-cursor nil 'local)
        (add-hook 'after-change-functions #'org-latex-preview-auto--detect-fragments-in-change nil 'local))
    (remove-hook 'pre-command-hook #'org-latex-preview-auto--handle-pre-cursor 'local)
    (remove-hook 'post-command-hook #'org-latex-preview-auto--handle-post-cursor 'local)
    (remove-hook 'after-change-functions #'org-latex-preview-auto--detect-fragments-in-change 'local)))

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
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (org-latex-preview-fragments
     org-latex-preview-default-process
     beg end)))

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

(defun org-latex-preview-replace-fragments (prefix processing-type &optional dir msg)
  "Replace all LaTeX fragments in the buffer with export appropriate forms.
The way this is done is set by PROCESSING-TYPE, which can be either:
- verabtim, in which case nothing is done
- mathjax, in which case the TeX-style delimeters are replaced with
  LaTeX-style delimeters.
- html, in which case the math fragment is replaced by the result of
  `org-format-latex-as-html'.
- mathml, in which case the math fragment is replace by the result of
  `org-format-latex-as-mathml'.
- an entry in `org-latex-preview-process-alist', in which case the
  math fragment is replaced with `org-create-latex-export'.

Generated image files are placed in DIR with the prefix PREFIX.
Note that PREFIX may itself contain a directory path component.

When generating output files, MSG will be `message'd if given."
  (let* ((cnt 0))
    (save-excursion
      (dolist (element (org-latex-preview-collect-fragments))
        (let ((block-type (eq (org-element-type element)
                              'latex-environment))
              (value (org-element-property :value element))
              (beg (org-element-property :begin element))
              (end (save-excursion
                     (goto-char (org-element-property :end element))
                     (skip-chars-backward " \r\t\n")
                     (point))))
          (cond
           ((eq processing-type 'verbatim)) ; Do nothing.
           ((eq processing-type 'mathjax)
            ;; Prepare for MathJax processing.
            (if (not (string-match "\\`\\$\\$?" value))
                (goto-char end)
              (delete-region beg end)
              (if (string= (match-string 0 value) "$$")
                  (insert "\\[" (substring value 2 -2) "\\]")
                (insert "\\(" (substring value 1 -1) "\\)"))))
           ((eq processing-type 'html)
            (goto-char beg)
            (delete-region beg end)
            (insert (org-format-latex-as-html value)))
           ((eq processing-type 'mathml)
            ;; Process to MathML.
            (unless (org-format-latex-mathml-available-p)
              (user-error "LaTeX to MathML converter not configured"))
            (cl-incf cnt)
            (when msg (message msg cnt))
            (goto-char beg)
            (delete-region beg end)
            (insert (org-format-latex-as-mathml
                     value block-type prefix dir)))
           ((assq processing-type org-latex-preview-process-alist)
            (let ((image-dir (expand-file-name prefix dir)))
              (unless (file-exists-p image-dir)
                (make-directory image-dir t)))
            (org-create-latex-export
             processing-type element prefix dir block-type))
           (t (error "Unknown conversion process %s for LaTeX fragments"
                     processing-type))))))))

(defun org-latex-preview-fragments (processing-type &optional beg end)
  "Produce image overlays of LaTeX math fragments between BEG and END.

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

(defun org-format-latex
    (prefix &optional beg end dir overlays msg forbuffer processing-type)
  "Replace LaTeX fragments with links to an image.

The function takes care of creating the replacement image.

Only consider fragments between BEG and END when those are
provided.

When optional argument OVERLAYS is non-nil, display the image on
top of the fragment instead of replacing it.

PROCESSING-TYPE is the conversion method to use, as a symbol.

Some of the options can be changed using the variable
`org-latex-preview-options', which see."
  (if (and overlays forbuffer)
      (org-latex-preview-fragments processing-type beg end)
    (org-latex-preview-replace-fragments prefix processing-type dir msg)))

(defun org-latex-preview--place-from-elements (processing-type elements)
  "Preview LaTeX math fragments ELEMENTS using PROCESSING-TYPE."
  (let* ((numbering-table (and org-latex-preview-numbered
                               ;; This is a nice way of quickly checking for a type
                               ;; across all elements, but it does assume that
                               ;; every element is well formed, more so than
                               ;; `org-element-type' in fact.
                               (memq 'latex-environment (mapcar #'car elements))
                               (org-latex-preview--environment-numbering-table)))
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
    (org-latex-preview-place processing-type entries numbering-offsets)))

;; * Make all the images
;; (<org-element latex-fragment> . key)...

;; Backend make-an-image
;; (get-cached-image (alist-get <org-element latex-fragment>))

;;;###autoload
(defun org-latex-preview-place (processing-type entries &optional numbering-offsets latex-header)
  "Preview LaTeX math fragments ENTRIES using PROCESSING-TYPE.
Each entry of ENTRIES should be a list of 2-3 items, either
  (BEG END), or
  (BEG END VALUE)
Where BEG and END are the positions in the buffer, and the LaTeX previewed
is either the substring between BEG and END or (when provided) VALUE."
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
                     (`(,fg ,bg) (org-latex-preview--colors-at beg))
                     (number (car (setq numbering-offsets (cdr numbering-offsets))))
                     (hash (org-latex-preview--hash
                            processing-type value imagetype fg bg number))
                     (options (org-combine-plists
                               org-latex-preview-options
                               (list :foreground fg
                                     :background bg
                                     :number number
                                     :continue-color
                                     (and (equal prev-bg bg)
                                          (equal prev-fg fg))))))
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
       :latex-header latex-header
       :place-preview-p t))))

(defun org-latex-preview--colors-at (pos)
  "Find colors for LaTeX previews to be inserted at POS."
  (let* ((face (or (and (> pos 1)
                        (get-text-property (1- pos) 'face))
                   'default))
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

(defun org-latex-preview--hash (processing-type string imagetype fg bg &optional number)
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
               org-latex-preview-header
               org-latex-default-packages-alist
               org-latex-packages-alist
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

(defun org-latex-preview--environment-numbering-table ()
  "Creat a hash table from numbered equations to their initial index."
  (let ((table (make-hash-table :test (if (org-element--cache-active-p)
                                          #'eq #'equal)))
        (counter 1))
    (save-match-data
      (dolist (element (org-latex-preview--get-numbered-environments))
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

(defun org-latex-preview--get-numbered-environments (&optional beg end)
  "Find all numbered environments between BEG and END."
  (if (org-element--cache-active-p)
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
       :to-pos (or end (point-max-marker)))
    (org-element-map
        (org-element-parse-buffer 'element)
        '(latex-environment)
      (lambda (datum)
        (and (<= (or beg (point-min)) (org-element-property :begin datum)
                 (org-element-property :end datum) (or end (point-max)))
             (message "elt!")
             (let* ((content (org-element-property :value datum))
                    (env (and (string-match "\\`\\\\begin{\\([^}]+\\)}" content)
                              (match-string 1 content))))
               (and (member env org-latex-preview--numbered-environments)
                    (save-excursion
                      (goto-char (org-element-property :begin datum))
                      (org-element-context)))))))))

(cl-defun org-latex-preview--create-image-async (processing-type fragments-info &key latex-processor latex-header place-preview-p)
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

LATEX-PROCESSOR is a member of `org-latex-compilers' which is guessed if unset."
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
                       :latex-header latex-header)
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
    ;;            └─ Message "creating latex previews... failed. please see %s for details".
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
    ;;       └─ Message "creating latex previews... failed. please see %s for details".
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
    ;;            └─ Message "creating latex previews... failed. please see %s for details".
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
                  (format "Creating LaTeX previews... failed. Please see %s for details"
                          (propertize org-latex-preview--image-log 'face 'warning))))
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
          (org-async-call img-extract-async)
        (plist-put (cddr tex-compile-async) :success img-extract-async)
        (plist-put (cddr tex-compile-async) :failure img-extract-async))
      (org-async-call tex-compile-async))))

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

(defun org-latex-preview--get-preamble (&optional buf)
  "Obtain the LaTeX preview for snippet preview in BUF."
  (with-current-buffer (or buf (current-buffer))
    (org-fold-core-ignore-modifications
      (let ((info (org-combine-plists
                   (org-export--get-export-attributes
                    (org-export-get-backend 'latex))
                   (org-export--get-buffer-attributes)
                   '(:time-stamp-file nil)))
            org-export-babel-evaluate
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
         (setq
          the-preamble
          (org-latex-make-preamble
          (org-combine-plists
           (org-export-get-environment
            (org-export-get-backend 'latex))
           '(:time-stamp-file nil))
          org-latex-preview-header 'snippet))
         the-preamble
         )))))

(defun org-latex-preview--create-tex-file (processing-info fragments)
  "Create a LaTeX file based on PROCESSING-INFO and PREVIEW-STRINGS.

More specifically, a preamble will be generated based on
PROCESSING-INFO.  Then, if `org-latex-preview-use-precompilation' is
non-nil, a precompiled format file will be generated if needed
and used.  Otherwise the preamble is used normally.

Within the body of the created LaTeX file, each of
PREVIEW-STRINGS will be placed in order, wrapped within a
\"preview\" environment.

The path of the created LaTeX file is returned."
  (let ((tex-temp-name
         (expand-file-name (concat (make-temp-name "org-tex-") ".tex")))
        (header
         (concat
          (or (plist-get processing-info :latex-header)
              org-latex-preview--preamble-content
              (setq org-latex-preview--preamble-content
                    (org-latex-preview--get-preamble)))
          (let ((w org-latex-preview-width))
            (cond
             ((stringp w)
              (format "\n\\setlength{\\textwidth}{%s}\n" w))
             ((and (floatp w) (<= 0.0 w 1.0))
              (format "\n\\setlength{\\textwidth}{%s\\paperwidth}\n" w))))
          "\n\\usepackage[active,tightpage,auctex]{preview}\n"))
        (write-region-inhibit-fsync t)
        (coding-system-for-write buffer-file-coding-system))
    (with-temp-file tex-temp-name
      (insert (if-let ((format-file
                        (and org-latex-preview-use-precompilation
                             (org-latex-preview-precompile processing-info header))))
                  (concat "%& " (file-name-sans-extension format-file))
                header))
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
    (list 'org-async-task
          tex-formatted-command
          :buffer tex-process-buffer
          :info extended-info
          :filter #'org-latex-preview--latex-preview-filter
          :failure "LaTeX compilation for preview failed! (error code %d)")))

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
         (image-size-adjust (or (plist-get extended-info :image-size-adjust)
                                '(1.0 . 1.0)))
         (scale (* (car image-size-adjust)
                   (or (plist-get org-latex-preview-options :scale) 1.0)))
         (dpi (* scale (if (display-graphic-p) (org-latex-preview--get-display-dpi) 140.0)))
         (texfile (plist-get extended-info :texfile))
         (texfile-base (file-name-base texfile))
         (img-command-spec
          `((?o . ,(shell-quote-argument temporary-file-directory))
            (?b . ,(shell-quote-argument (file-name-base texfile)))
            (?B . ,(shell-quote-argument
                    (expand-file-name texfile-base temporary-file-directory)))
            (?D . ,(shell-quote-argument (format "%s" dpi)))
            (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))
            (?f . ,(shell-quote-argument
                    (expand-file-name
                     (concat texfile-base
                             "." (plist-get extended-info :image-input-type))
                     temporary-file-directory)))))
         (img-formatted-command
          (split-string-shell-command
           (format-spec img-extract-command img-command-spec))))
    (plist-put extended-info :dpi-scale-factor (/ dpi 140.0))
    (list 'org-async-task
          img-formatted-command
          :buffer img-process-buffer
          :info extended-info
          :failure "LaTeX preview image conversion failed! (error code %d)")))

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
         (image-output-type (intern (plist-get extended-info :image-output-type)))
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
        (dpi-factor (or (plist-get extended-info :dpi-scale-factor) 1.0))
        info)
    (setq info (plist-put info :image-type image-type))
    (dolist (key '(:width :height :depth))
      (when-let ((val (plist-get fragment-info key)))
        (plist-put info key (/ val dpi-factor))))
    (plist-put info :errors (plist-get fragment-info :errors))
    info))

(defconst org-latex-preview--tex-scale-divisor 65781.76
  "The ratio between ")

(defun org-latex-preview--latex-preview-filter (_proc _string extended-info)
  "Examine the stdout from LaTeX compilation with preview.sty.
The detected fontsize is directly entered into EXTENDED-INFO, and
fragment errors are put into the :errors slot of the relevant
fragments in EXTENDED-INFO."
  (unless (plist-get extended-info :fontsize)
    (when (save-excursion
            (re-search-forward "^Preview: Fontsize \\([0-9]+\\)pt$" nil t))
      (plist-put extended-info :fontsize (string-to-number (match-string 1)))))
  (let ((preview-start-re
         "^! Preview: Snippet \\([0-9]+\\) started.\n<-><->\n *\nl\\.\\([0-9]+\\)[^\n]+\n")
        (preview-end-re
         "\\(?:^Preview: Tightpage.*$\\)?\n! Preview: Snippet [0-9]+ ended.(\\([0-9]+\\)+\\([0-9]+\\)x\\([0-9]+\\))")
        (fragments (plist-get extended-info :fragments))
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
      (if (re-search-forward preview-end-re (or (caadr preview-marks) (point-max)) t)
          (let ((fragment-info (nth (1- (nth 2 (car preview-marks))) fragments))
                (errors-substring
                 (string-trim
                  (buffer-substring (cadar preview-marks)
                                    (match-beginning 0)))))
            (cl-loop for (geom . match-index)
                     in '((:height . 1) (:depth . 2) (:width . 3))
                     do
                     (plist-put fragment-info geom
                                (/ (string-to-number (match-string match-index))
                                   org-latex-preview--tex-scale-divisor
                                   (or (plist-get fragment-info :fontsize) 10))))
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
        ;; (dvisvgm-depth-re "depth=\\([0-9.]+\\)pt$")
        ;; (dvisvgm-size-re "^ *graphic size: \\([0-9.]+\\)pt x \\([0-9.]+\\)pt")
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
          ;; (when (save-excursion
          ;;         (re-search-forward dvisvgm-depth-re end t))
          ;;   (plist-put fragment-info :depth (string-to-number (match-string 1))))
          ;; (when (save-excursion (re-search-forward dvisvgm-size-re end t))
          ;;   (plist-put fragment-info :height (string-to-number (match-string 2)))
          ;;   (plist-put fragment-info :width (string-to-number (match-string 1))))
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
      ;; There seems to often be a slight delay between dvisvgm reporting
      ;; to have written a file, and all the content actually being there.
      ;; On my machine, an 0.002s delay is sufficient to eliminate this issue,
      ;; to be a bit safer this we use 5x that here.
      (run-at-time
       0.01 nil
       (if (plist-get extended-info :place-preview-p)
           (lambda (fragments)
             (mapc #'org-latex-preview--svg-make-fg-currentColor fragments)
             (org-latex-preview--place-images extended-info fragments))
         (lambda (fragments)
           (mapc #'org-latex-preview--svg-make-fg-currentColor fragments)
           (dolist (fragment-info fragments)
             (org-latex-preview--cache-image
              (plist-get fragment-info :key)
              (plist-get fragment-info :path)
              (org-latex-preview--display-info
               extended-info fragment-info)))))
       fragments-to-show))))

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
    (when path
      (with-temp-buffer
        (insert-file-contents path)
        (unless ; When the svg is incomplete, wait for it to be completed.
            (string= (buffer-substring (- (point-max) 6) (point-max))
                     "</svg>")
          (catch 'svg-complete
            (dotimes (_ 1000) ; Check for complete svg over 1s.
              (if (string= (buffer-substring (- (point-max) 6) (point-max))
                           "</svg>")
                  (throw 'svg-complete t)
                (erase-buffer)
                (sit-for 0.001)
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
          (when (re-search-forward "<g fill='\\(#[0-9a-f]\\{6\\}\\)'" nil t)
            (let* ((same-color (format "\\(?:fill\\|stroke\\)='\\(%s\\)'" (match-string 1))))
              (replace-match "currentColor" t t nil 1)
              (while (re-search-forward same-color nil t)
                (replace-match "currentColor" t t nil 1)))
            (write-region nil nil path nil 0)))))))

(defconst org-latex-preview--dvipng-dpi-pt-factor 0.5144
  "Factor that converts dvipng reported depth at 140 DPI to pt.

This value was obtained by observing linear scaling between the
set DPI and reported height/depth, then calling dvipng with a DPI
of 5600 and dividing the reported height + depth (692) by the dvisvgm
reported values in pt (8.899pt).")

(defun org-latex-preview--dvipng-filter (_proc _string extended-info)
  "Look for newly created images in the dvipng stdout buffer.
Any matches found will be matched against the fragments recorded in
EXTENDED-INFO, and displayed in the buffer."
  (let (;; (dvipng-depth-height-re "depth=\\([0-9]+\\) height=\\([0-9]+\\)")
        (outputs-no-ext (expand-file-name
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
              ;; (when (re-search-forward dvipng-depth-height-re page-info-end t)
              ;;   (let ((depth (* (string-to-number (match-string 1))
              ;;                   org-latex-preview--dvipng-dpi-pt-factor))
              ;;         (height (* (string-to-number (match-string 2))
              ;;                    org-latex-preview--dvipng-dpi-pt-factor)))
              ;;     (plist-put fragment-info :depth depth)
              ;;     (plist-put fragment-info :height (+ depth height))))
              (push fragment-info fragments-to-show)))))
    (when fragments-to-show
      (setq fragments-to-show (nreverse fragments-to-show))
      (if (plist-get extended-info :place-preview-p)
          (org-latex-preview--place-images extended-info fragments-to-show)
        (dolist (fragment-info fragments)
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
  (if org-latex-preview-persist
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
        (cons (cadr label-path-info) info))
    (unless org-latex-preview--table
      (setq org-latex-preview--table (make-hash-table :test 'equal :size 240)))
    (when-let ((path)
               (new-path (expand-file-name
                          (concat "org-tex-" key "." (file-name-extension path))
                          temporary-file-directory)))
      (copy-file path new-path 'replace)
      (puthash key (cons new-path info)
               org-latex-preview--table))))

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
the *entire* preview cache will be cleared."
  (interactive (let ((context (org-element-context)))
                 (cond
                  (current-prefix-arg
                   (list nil nil (y-or-n-p "This will clear the systemwide LaTeX preview cache, continue? ")))
                  ((use-region-p)
                   (list (region-beginning) (region-end)))
                  ((memq (org-element-type context)
                         '(latex-fragment latex-environment))
                   (list (org-element-property :begin context)
                         (org-element-property :end context)))
                  (t (list nil nil)))))
  (org-latex-preview-clear-overlays beg end)
  (if clear-entire-cache
      (let ((n 0))
        (dolist (collection org-persist--index)
          (when (equal (cadar (plist-get collection :container))
                       org-latex-preview--cache-name)
            (org-latex-preview--remove-cached
             (plist-get (plist-get collection :associated) :key))
            (cl-incf n)))
        (if (> n 0)
            (message "Cleared all %d entries fom the Org LaTeX preview cache." n)
          (message "The Org LaTeX preview cache was already empty.")))
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
                     (`(,fg ,bg) (org-latex-preview--colors-at begin))
                     (value (org-element-property :value element))
                     (number (and numbering-table
                                  (eq (org-element-type element)
                                      'latex-environment)
                                  (gethash element numbering-table))))
          (org-latex-preview--remove-cached
           (org-latex-preview--hash
            org-latex-preview-default-process
            value imagetype fg bg number))))
      (message "Cleared LaTeX preview cache for %s."
               (if (or beg end) "region" "buffer")))))

(defun org-latex-preview-precompile (processing-info preamble)
  "Precompile/dump LaTeX PREAMBLE text.

The path to the format file (.fmt) is returned.  If the format
file could not be found in the persist cache, it is generated
according to PROCESSING-INFO and stored.

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
   preamble))

(defun org-latex-preview--tex-styled (processing-type value options &optional html-p)
  "Apply LaTeX style commands to VALUE based on OPTIONS.

VALUE is the math fragment text to be previewed.

OPTIONS is the plist `org-latex-preview-options' with customized
color information for this run.

HTML-P, if true, uses colors required for HTML processing."
  (let* ((fg (pcase (plist-get options (if html-p :html-foreground :foreground))
               ('default (org-latex-preview--format-color (org-latex-preview--attr-color :foreground)))
               ((pred null) (org-latex-preview--format-color "Black"))
               (color (org-latex-preview--format-color color))))
         (bg (pcase (plist-get options (if html-p :html-background :background))
               ('default (org-latex-preview--attr-color :background))
               ("Transparent" nil)
               (bg (org-latex-preview--format-color bg))))
         (num (plist-get options :number)))
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

(defun org-create-latex-export (processing-type element prefix dir &optional block-type)
  "Create a export of the LaTeX math fragment ELEMENT using PROCESSING-TYPE.

Generated image files are placed in DIR with the prefix PREFIX.
Note that PREFIX may itself contain a directory path component.

BLOCK-TYPE determines whether the result is placed inline or as a paragraph."
  (let* ((processing-info
          (cdr (assq processing-type org-latex-preview-process-alist)))
         (beg (org-element-property :begin element))
         (end (save-excursion
                (goto-char (org-element-property :end element))
                (skip-chars-backward " \r\t\n")
                (point)))
         (value (org-element-property :value element))
         (fg (plist-get org-latex-preview-options :foreground))
         (bg (plist-get org-latex-preview-options :background))
         (hash (sha1 (prin1-to-string
                      (list processing-type
                            org-latex-preview-header
                            org-latex-default-packages-alist
                            org-latex-packages-alist
                            org-latex-preview-options
                            'export value fg bg))))
         (imagetype (or (plist-get processing-info :image-output-type) "png"))
         (absprefix (expand-file-name prefix dir))
         (linkfile (format "%s_%s.%s" prefix hash imagetype))
         (movefile (format "%s_%s.%s" absprefix hash imagetype))
         (sep (and block-type "\n\n"))
         (link (concat sep "[[file:" linkfile "]]" sep))
         (options (org-combine-plists
                   org-latex-preview-options
                   (list :foreground fg :background bg))))
    (unless (file-exists-p movefile)
      (org-latex-preview-create-image
       value movefile options nil processing-type))
    (org-latex-preview-place-image-link link block-type beg end value)))

(defun org-latex-preview-place-image-link (link block-type beg end value)
  "Place then link LINK at BEG END."
  (delete-region beg end)
  (insert
   (org-add-props link
       (list 'org-latex-src
             (replace-regexp-in-string "\"" "" value)
             'org-latex-src-embed-type
             (if block-type 'paragraph 'character)))))

(defun org-create-math-formula (latex-frag &optional mathml-file)
  "Convert LATEX-FRAG to MathML and store it in MATHML-FILE.
Use `org-latex-to-mathml-convert-command'.  If the conversion is
successful, return the portion between \"<math...> </math>\"
elements otherwise return nil.  When MATHML-FILE is specified,
write the results in to that file.  When invoked as an
interactive command, prompt for LATEX-FRAG, with initial value
set to the current active region and echo the results for user
inspection."
  (interactive (list (let ((frag (when (org-region-active-p)
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end)))))
                       (read-string "LaTeX Fragment: " frag nil frag))))
  (unless latex-frag (user-error "Invalid LaTeX fragment"))
  (let* ((tmp-in-file
          (let ((file (file-relative-name
                       (make-temp-name (expand-file-name "ltxmathml-in")))))
            (write-region latex-frag nil file)
            file))
         (tmp-out-file (file-relative-name
                        (make-temp-name (expand-file-name  "ltxmathml-out"))))
         (cmd (format-spec
               org-latex-to-mathml-convert-command
               `((?j . ,(and org-latex-to-mathml-jar-file
                             (shell-quote-argument
                              (expand-file-name
                               org-latex-to-mathml-jar-file))))
                 (?I . ,(shell-quote-argument tmp-in-file))
                 (?i . ,latex-frag)
                 (?o . ,(shell-quote-argument tmp-out-file)))))
         mathml shell-command-output)
    (when (called-interactively-p 'any)
      (unless (org-format-latex-mathml-available-p)
        (user-error "LaTeX to MathML converter not configured")))
    (message "Running %s" cmd)
    (setq shell-command-output (shell-command-to-string cmd))
    (setq mathml
          (when (file-readable-p tmp-out-file)
            (with-current-buffer (find-file-noselect tmp-out-file t)
              (goto-char (point-min))
              (when (re-search-forward
                     (format "<math[^>]*?%s[^>]*?>\\(.\\|\n\\)*</math>"
                             (regexp-quote
                              "xmlns=\"http://www.w3.org/1998/Math/MathML\""))
                     nil t)
                (prog1 (match-string 0) (kill-buffer))))))
    (cond
     (mathml
      (setq mathml
            (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" mathml))
      (when mathml-file
        (write-region mathml nil mathml-file))
      (when (called-interactively-p 'any)
        (message mathml)))
     ((warn "LaTeX to MathML conversion failed")
      (message shell-command-output)))
    (delete-file tmp-in-file)
    (when (file-exists-p tmp-out-file)
      (delete-file tmp-out-file))
    mathml))

(defun org-format-latex-as-mathml (latex-frag latex-frag-type
                                              prefix &optional dir)
  "Use `org-create-math-formula' but check local cache first."
  (let* ((absprefix (expand-file-name prefix dir))
         (print-length nil) (print-level nil)
         (formula-id (concat
                      "formula-"
                      (sha1
                       (prin1-to-string
                        (list latex-frag
                              org-latex-to-mathml-convert-command)))))
         (formula-cache (format "%s-%s.mathml" absprefix formula-id))
         (formula-cache-dir (file-name-directory formula-cache)))

    (unless (file-directory-p formula-cache-dir)
      (make-directory formula-cache-dir t))

    (unless (file-exists-p formula-cache)
      (org-create-math-formula latex-frag formula-cache))

    (if (file-exists-p formula-cache)
        ;; Successful conversion.  Return the link to MathML file.
        (org-add-props
            (format  "[[file:%s]]" (file-relative-name formula-cache dir))
            (list 'org-latex-src (replace-regexp-in-string "\"" "" latex-frag)
                  'org-latex-src-embed-type (if latex-frag-type
                                                'paragraph 'character)))
      ;; Failed conversion.  Return the LaTeX fragment verbatim
      latex-frag)))

(defun org-format-latex-as-html (latex-fragment)
  "Convert LATEX-FRAGMENT to HTML.
This uses  `org-latex-to-html-convert-command', which see."
  (let ((cmd (format-spec org-latex-to-html-convert-command
                          `((?i . ,latex-fragment)))))
    (message "Running %s" cmd)
    (shell-command-to-string cmd)))

(defun org-latex-preview--get-display-dpi ()
  "Get the DPI of the display.
The function assumes that the display has the same pixel width in
the horizontal and vertical directions."
  (if (display-graphic-p)
      (round (/ (display-pixel-height)
                (/ (display-mm-height) 25.4)))
    (error "Attempt to calculate the dpi of a non-graphic display")))

(defun org-latex-preview-create-image
    (string tofile options buffer &optional processing-type)
  "Create an image from LaTeX source using external processes.

The LaTeX STRING is saved to a temporary LaTeX file, then
converted to an image file by process PROCESSING-TYPE defined in
`org-latex-preview-process-alist'.  A nil value defaults to
`org-latex-preview-default-process'.

The generated image file is eventually moved to TOFILE.

The OPTIONS argument controls the size, foreground color and
background color of the generated image.

When BUFFER non-nil, this function is used for LaTeX previewing.
Otherwise, it is used to deal with LaTeX snippets showed in
a HTML file."
  (let* ((processing-type (or processing-type
                              org-latex-preview-default-process))
         (processing-info
          (cdr (assq processing-type org-latex-preview-process-alist)))
         (programs (plist-get processing-info :programs))
         (error-message (or (plist-get processing-info :message) ""))
         (image-input-type (plist-get processing-info :image-input-type))
         (image-output-type (plist-get processing-info :image-output-type))
         (post-clean (or (plist-get processing-info :post-clean)
                         '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
                           ".svg" ".png" ".jpg" ".jpeg" ".out")))
         (latex-header
          (or (plist-get processing-info :latex-header)
              (org-latex-make-preamble
               (org-export-get-environment (org-export-get-backend 'latex))
               org-latex-preview-header
               'snippet)))
         (latex-compiler (plist-get processing-info :latex-compiler))
         (tmpdir temporary-file-directory)
         (texfile-base (make-temp-name
                        (expand-file-name "orgtex" tmpdir)))
         (texfile (concat texfile-base ".tex"))
         (image-size-adjust (or (plist-get processing-info :image-size-adjust)
                                '(1.0 . 1.0)))
         (scale (* (if buffer (car image-size-adjust) (cdr image-size-adjust))
                   (or (plist-get options (if buffer :scale :html-scale)) 1.0)))
         (dpi (* scale (if (and buffer (display-graphic-p)) (org-latex-preview--get-display-dpi) 140.0)))
         (fg (or (plist-get options (if buffer :foreground :html-foreground))
                 "Black"))
         (bg (or (plist-get options (if buffer :background :html-background))
                 "Transparent"))
         (image-converter
          (or (and (string= bg "Transparent")
                   (plist-get processing-info :transparent-image-converter))
              (plist-get processing-info :image-converter)))
         (log-buf (get-buffer-create org-latex-preview--latex-log))
         (resize-mini-windows nil)) ;Fix Emacs flicker when creating image.
    (dolist (program programs)
      (org-check-external-command program error-message))
    (if (eq fg 'default)
        (setq fg (org-latex-preview--attr-color :foreground))
      (setq fg (org-latex-preview--format-color fg)))
    (setq bg (cond
              ((eq bg 'default) (org-latex-preview--attr-color :background))
              ((string= bg "Transparent") nil)
              (t (org-latex-preview--format-color bg))))
    ;; Remove TeX \par at end of snippet to avoid trailing space.
    (if (string-suffix-p string "\n")
        (aset string (1- (length string)) ?%)
      (setq string (concat string "%")))
    (with-temp-file texfile
      (insert latex-header)
      (insert "\n\\begin{document}\n"
              "\\definecolor{fg}{rgb}{" fg "}%\n"
              (if bg
                  (concat "\\definecolor{bg}{rgb}{" bg "}%\n"
                          "\n\\pagecolor{bg}%\n")
                "")
              "\n{\\color{fg}\n"
              string
              "\n}\n"
              "\n\\end{document}\n"))
    (let* ((err-msg (format "Please adjust `%s' part of \
`org-latex-preview-process-alist'."
                            processing-type))
           (image-input-file
            (org-compile-file
             texfile latex-compiler image-input-type err-msg log-buf))
           (image-output-file
            (org-compile-file
             image-input-file image-converter image-output-type err-msg log-buf
             `((?D . ,(shell-quote-argument (format "%s" dpi)))
               (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))))
      (copy-file image-output-file tofile 'replace)
      (dolist (e post-clean)
        (when (file-exists-p (concat texfile-base e))
          (delete-file (concat texfile-base e))))
      image-output-file)))

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
