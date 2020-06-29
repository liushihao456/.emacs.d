;;; company-quickdoc.el --- Popup documentation for completion candidates  -*- lexical-binding: t; -*-

;; Author: Shihao Liu <liushihao@pku.edu.com>
;; Keywords: company popup documentation quickdoc
;; Version: 2.2.0
;; Package-Requires: ((emacs "24.3") (company "0.8.9"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; When idling on a completion candidate the documentation for the
;; candidate will pop up after `company-quickdoc-delay' seconds.

;;; Usage:
;;  put (company-quickdoc-mode) in your init.el to activate
;;  `company-quickdoc-mode'.

;; You can adjust the time it takes for the documentation to pop up by
;; changing `company-quickdoc-delay'

;;; Code:
(require 'company)
(require 'cl-lib)
(require 's)
(require 'dash)

(defgroup company-quickdoc nil
  "Documentation popups for `company-mode'"
  :group 'company)

(defcustom company-quickdoc-delay 0.5
  "Delay, in seconds, before the quickdoc popup appears.

If set to nil the popup won't automatically appear, but can still
be triggered manually using `company-quickdoc-manual-begin'."
  :type '(choice (number :tag "Delay in seconds")
                 (const :tag "Don't popup help automatically" nil))
  :group 'company-quickdoc)

(defcustom company-quickdoc-max-lines nil
  "When not NIL, limits the number of lines in the popup."
  :type '(choice (integer :tag "Max lines to show in popup")
                 (const :tag "Don't limit the number of lines shown" nil))
  :group 'company-quickdoc)

(defface company-quickdoc-background
  '((((background light)) :background "#6F6F6F")
    (t :background "#272A36"))
  "Background color of the documentation.
Only the `background' is used in this face."
  :group 'company-quickdoc)

(defface company-quickdoc-header
  '((t :foreground "black"
       :background "deep sky blue"))
  "Face used on the header."
  :group 'company-quickdoc)

(defface company-quickdoc-url
  '((t :inherit link))
  "Face used on links."
  :group 'company-quickdoc)

(defvar company-quickdoc-overlays nil
  "Quickdoc overlays.")

(defvar-local company-quickdoc--timer nil
  "Quickhelp idle timer.")

(defun company-quickdoc-frontend (command)
  "`company-mode' front-end showing documentation in a popup."
  (pcase command
    (`pre-command
     (company-quickdoc--cancel-timer)
     (company-quickdoc--hide))
    (`post-command
     (unless (company--show-inline-p)
       (company-quickdoc--set-timer)))
    ))

(defun company-quickdoc--skip-footers-backwards ()
  "Skip backwards over footers and blank lines."
  (beginning-of-line)
  (while (and (not (= (point-at-eol) (point-min)))
              (or
               ;; [back] appears at the end of the help elisp help buffer
               (looking-at-p "\\[back\\]")
               ;; [source] cider's help buffer contains a link to source
               (looking-at-p "\\[source\\]")
               (looking-at-p "^\\s-*$")))
    (forward-line -1)))

(defun company-quickdoc--goto-max-line ()
  "Go to last line to display in popup."
  (if company-quickdoc-max-lines
      (forward-line company-quickdoc-max-lines)
    (goto-char (point-max))))

(defun company-quickdoc--docstring-from-buffer (start)
  "Fetch docstring from START."
  (goto-char start)
  (company-quickdoc--goto-max-line)
  (let ((truncated (< (point-at-eol) (point-max))))
    (company-quickdoc--skip-footers-backwards)
    (list :doc (buffer-substring-no-properties start (point-at-eol))
          :truncated truncated)))

(defun company-quickdoc--completing-read (prompt candidates &rest rest)
  "`cider', and probably other libraries, prompt the user to
resolve ambiguous documentation requests.  Instead of failing we
just grab the first candidate and press forward."
  (car candidates))

(defun company-quickdoc--fetch-docstring (backend)
  "Fetch docstring from BACKEND."
  (let ((quickdoc-str (company-call-backend 'quickdoc-string backend)))
    (if (stringp quickdoc-str)
        (with-temp-buffer
          (insert quickdoc-str)
          (company-quickdoc--docstring-from-buffer (point-min)))
      (let ((doc (company-call-backend 'doc-buffer backend)))
        (when doc
          ;; The company backend can either return a buffer with the doc or a
          ;; cons containing the doc buffer and a position at which to start
          ;; reading.
          (let ((doc-buffer (if (consp doc) (car doc) doc))
                (doc-begin (when (consp doc) (cdr doc))))
            (with-current-buffer doc-buffer
              (company-quickdoc--docstring-from-buffer (or doc-begin (point-min))))))))))

(defun company-quickdoc--doc (selected)
  "Get docstring for SELECTED candidate."
  (cl-letf (((symbol-function 'completing-read)
             #'company-quickdoc--completing-read))
    (let* ((doc-and-meta (company-quickdoc--fetch-docstring selected))
           (truncated (plist-get doc-and-meta :truncated))
           (doc (plist-get doc-and-meta :doc)))
      (unless (member doc '(nil ""))
        (if truncated
            (concat doc "\n\n[...]")
          doc)))))

(defun company-quickdoc--manual-begin ()
  "Manually trigger the `company-quickdoc' popup for the currently active `company' completion candidate."
  (interactive)
  ;; This might seem a bit roundabout, but when attempting to call
  ;; `company-quickdoc--show' in a more direct manner it may trigger a
  ;; redisplay of company's list of completion candidates which looked
  ;; quite weird.
  (let ((company-quickdoc-delay 0.01))
    (company-quickdoc--set-timer)))

(defun company-quickdoc--hide ()
  "Hide the current quickdoc tip."
  (when (> (length company-quickdoc-overlays) 0)
    (dolist (ov company-quickdoc-overlays)
      (delete-overlay ov))
    (setq company-quickdoc-overlays nil)))

(defun company-quickdoc--wrapped-line (line line-width)
  "Wrap a line of text LINE to max width LINE-WIDTH."
  (let ((trimmed (string-trim-right line)))
    (cond ((string-empty-p trimmed) "")
          ((< (length trimmed) (- line-width 2)) trimmed)
          (t (s-word-wrap (- line-width 2) trimmed)))))

(defun company-quickdoc--padding (line len)
  "Add trailing whitespaces to string LINE to reach length LEN.
Then add one whitespace to begin and end of it.

There might be words longer than LINE-WIDTH, in which case they have to be cut
off."
  (let ((line (if (> (string-width line) len) (concat " "(substring line 0 len) " ")
                (concat " " line (make-string (- len (string-width line)) ?\s) " "))))
    (add-face-text-property 0 (length line) (list :background (face-background 'company-quickdoc-background nil t) :foreground (face-foreground 'default)) t line)
    line))

(defun company-quickdoc--format-string (string line-width)
  "Wrap STRING to max width LINE-WIDTH, and cutoff at max height HEIGHT."
  (--> string
       (split-string it "[\n\v\f\r](?![\n\v\f\r])")
       (-map (lambda (line) (company-quickdoc--wrapped-line line line-width)) it)
       (string-join it "\n")
       (split-string it "[\n\v\f\r]")
       (-map (lambda (line) (company-quickdoc--padding line line-width)) it)))

(defun company-quickdoc--pos-after-lines (start n)
  "Get the position at START + forward N lines."
  (save-excursion (goto-char start)
                  (forward-line n)
                  (point)))

(defun company-quickdoc--merge-docstrings (old-strings doc-strings position)
  "Concatenate DOC-STRINGS to OLD-STRINGS.

The 3rd arg POSITION, indicates at which side the doc will be rendered."
  (let* ((ov company-pseudo-tooltip-overlay)
         (tooltip-width (overlay-get ov 'company-width))
         (company-column (overlay-get ov 'company-column))
         (horizontal-span (+ (company--window-width) (window-hscroll)))
         (tooltip-column (min (- horizontal-span tooltip-width) company-column))
         (index-start (if (eq position 'right)
                          (+ tooltip-column tooltip-width)
                        (1+ (window-hscroll)))))
    (company-quickdoc--merge-lines old-strings doc-strings index-start)))

(defun company-quickdoc--merge-lines (lines1 lines2 start)
  "Concatenate LINES1 and LINES2.

Replacing a substring starting from START of each line in LINES1 with the
corresponding line in LINES2."
  (let* ((index-start start)
         (result-strings nil))
    (dotimes (i
              (max (length lines1) (length lines2))
              (nreverse result-strings))
      (!cons
       (let* ((line1 (nth i lines1))
              (line2 (nth i lines2))
              (index-end (+ index-start (length line2))))
         (cond
          ((not line2)
           line1)
          ((not line1)
           (concat (truncate-string-to-width "" index-start nil ?\s)
                   line2))
          (t
           (concat (truncate-string-to-width line1 index-start nil ?\s)
                   line2
                   (truncate-string-to-width line1 (length line1) (min (length line1) index-end) ?\s)))))
       result-strings))))

(defun company-quickdoc--render-doc-part-above (doc-lines doc-position)
  "Render a part of the doc above the company tooltip.

The 1st arg DOC-LINES is a list containing doc string lines.  The 2nd arg
DOC-POSITION indicates at which side the doc will be rendered."
  (let* ((tooltip-height
          (abs (overlay-get company-pseudo-tooltip-overlay 'company-height)))
         (tooltip-abovep (nth 3 (overlay-get company-pseudo-tooltip-overlay 'company-replacement-args)))
         (n-lines (length doc-lines))

         (ov-end (if tooltip-abovep
                     (company-quickdoc--pos-after-lines
                      (line-beginning-position) (- tooltip-height))
                   (line-beginning-position)))
         (ov-start (company-quickdoc--pos-after-lines ov-end (- n-lines)))
         (buffer-lines
          (split-string (buffer-substring ov-start ov-end) "[\n\v\f\r]"))
         (ov (make-overlay ov-start ov-end)))
    (!cons ov company-quickdoc-overlays)
    (--> (company-quickdoc--merge-docstrings buffer-lines doc-lines doc-position)
         (string-join it "\n")
         (if (< (overlay-start ov) (overlay-end ov))
             (overlay-put ov 'display it)
           (overlay-put ov 'after-string it)))
    (overlay-put ov 'window (selected-window))))

(defun company-quickdoc--render-doc-part-current-line (doc-lines doc-position)
  "Render a part of the doc on current line.

The 1st arg DOC-LINES is a list of doc string lines.  The 2nd arg DOC-POSITION
indicates at which side the doc will be rendered."
  (let* ((company-nl
          (nth 2 (overlay-get company-pseudo-tooltip-overlay
                              'company-replacement-args)))
         (current-buffer-line
          (buffer-substring (line-beginning-position) (line-end-position)))
         (tooltip-lines
          (split-string
           (overlay-get company-pseudo-tooltip-overlay 'company-display)
           "[\n\v\f\r]"))
         (use-after-string
          (overlay-get company-pseudo-tooltip-overlay 'after-string))
         (ov-start-col
          (save-excursion
            (goto-char (overlay-start company-pseudo-tooltip-overlay))
            (current-column)))
         (tooltip-popped-nl
          (and use-after-string company-nl (pop tooltip-lines))))

    (if (and (eq doc-position 'right) tooltip-popped-nl)

        (--> (->
              (company-quickdoc--merge-docstrings (list current-buffer-line) doc-lines doc-position)
              (string-join "\n")
              (substring ov-start-col))
             (cons it tooltip-lines)
             (string-join it "\n")
             (overlay-put company-pseudo-tooltip-overlay 'company-display it)
             (overlay-put company-pseudo-tooltip-overlay 'after-string it))

      (let* ((ov-start (line-beginning-position))
             (ov-end (line-end-position))
             (ov (make-overlay ov-start ov-end)))
        (!cons ov company-quickdoc-overlays)
        (--> (company-quickdoc--merge-docstrings (list current-buffer-line) doc-lines doc-position)
             (car it)
             (or (put-text-property (current-column) (+ 1 (current-column)) 'cursor (length it) it) it)
             (if (< ov-start ov-end)
                 (overlay-put ov 'display it)
               (overlay-put ov 'after-string it)))
        (overlay-put ov 'window (selected-window))))))

(defun company-quickdoc--render-doc-part-matching-tooltip (doc-lines doc-position)
  "Render a part of the doc matching the company tooltip.

The 1st arg DOC-LINES is a list containing doc string lines.  The 2nd arg
DOC-POSITION indicates at which side the doc will be rendered."
  (let* ((company-nl
          (nth 2 (overlay-get company-pseudo-tooltip-overlay
                              'company-replacement-args)))
         (tooltip-lines
          (split-string
           (overlay-get company-pseudo-tooltip-overlay 'company-display)
           "[\n\v\f\r]"))
         (use-after-string
          (overlay-get company-pseudo-tooltip-overlay 'after-string))
         (tooltip-popped-nl
          (and use-after-string company-nl (pop tooltip-lines))))
    (--> (company-quickdoc--merge-docstrings tooltip-lines doc-lines doc-position)
         (if tooltip-popped-nl (cons tooltip-popped-nl it) it)
         (string-join it "\n")
         (overlay-put company-pseudo-tooltip-overlay
                      (if use-after-string 'after-string 'display) it))))

(defun company-quickdoc--render-doc-part-below (doc-lines doc-position)
  "Render a part of the doc below the company tooltip.

The 1st arg DOC-LINES is a list containing doc string lines.  The 2nd arg
DOC-POSITION indicates at which side the doc will be rendered."
  (let* ((n-lines (length doc-lines))
         (tooltip-lines
          (split-string
           (overlay-get company-pseudo-tooltip-overlay 'company-display)
           "[\n\v\f\r]"))
         (tooltip-height
          (abs (overlay-get company-pseudo-tooltip-overlay 'company-height)))
         (tooltip-abovep (nth 3 (overlay-get company-pseudo-tooltip-overlay 'company-replacement-args)))
         (ov-start
          (if tooltip-abovep
              (company-quickdoc--pos-after-lines
               (line-beginning-position) 1)
              (company-quickdoc--pos-after-lines
               (line-beginning-position) (1+ tooltip-height))))
         (ov-end (company-quickdoc--pos-after-lines ov-start (1+ n-lines)))
         (buffer-lines
          (split-string (buffer-substring ov-start ov-end) "[\n\v\f\r]"))
         (use-after-string (>= ov-start ov-end))
         (ov (make-overlay ov-start ov-end)))
    (!cons ov company-quickdoc-overlays)
    (--> (company-quickdoc--merge-docstrings buffer-lines doc-lines doc-position)
         (if (and use-after-string
                  (or tooltip-abovep (< (length tooltip-lines) (+ 1 tooltip-height))))
             (cons "" it) it)
         (string-join it "\n")
         (if use-after-string
             (overlay-put ov 'after-string it)
           (overlay-put ov 'display it)))
    (overlay-put ov 'window (selected-window))))

(defun company-quickdoc--get-layout (doc-lines-length)
  "Get the layout for doc parts.  DOC-LINES-LENGTH is the number of lines of doc."
  (let* ((tooltip-height (abs (overlay-get company-pseudo-tooltip-overlay 'company-height)))
         (tooltip-abovep (nth 3 (overlay-get company-pseudo-tooltip-overlay 'company-replacement-args)))
         (current-row (cdr (company--col-row (line-beginning-position)))))
    (if tooltip-abovep
        (let* ((nlines-above
                (min (max (- current-row tooltip-height) 0)
                     (max (- doc-lines-length tooltip-height) 0)))
               (nlines-matching-tooltip
                (min tooltip-height
                     doc-lines-length))
               (nlines-current-line
                (if (> doc-lines-length current-row) 1 0))
               (nlines-below
                (min (- (company--window-height) current-row 1)
                     (max (- doc-lines-length current-row 1) 0)))
               (layout-alist '()))
          (when (> nlines-above 0)
            (!cons `(:above . ,nlines-above) layout-alist))
          (when (> nlines-matching-tooltip 0)
            (!cons `(:matching-tooltip . ,nlines-matching-tooltip) layout-alist))
          (when (> nlines-current-line 0)
            (!cons `(:current-line . ,nlines-current-line) layout-alist))
          (when (> nlines-below 0)
            (!cons `(:below . ,nlines-below) layout-alist))
          (nreverse layout-alist))

      (let* ((nrows-below-current-line (- (company--window-height) current-row 1))
             (nlines-below
              (min (max (- nrows-below-current-line tooltip-height) 0)
                   (max (- doc-lines-length tooltip-height) 0)))
             (nlines-matching-tooltip
              (min tooltip-height
                   doc-lines-length))
             (nlines-current-line
              (if (> doc-lines-length nrows-below-current-line) 1 0))
             (nlines-above
              (min current-row
                   (max (- doc-lines-length nrows-below-current-line 1) 0)))
             (layout-alist '()))
        (when (> nlines-above 0)
          (!cons `(:above . ,nlines-above) layout-alist))
        (when (> nlines-current-line 0)
          (!cons `(:current-line . ,nlines-current-line) layout-alist))
        (when (> nlines-matching-tooltip 0)
          (!cons `(:matching-tooltip . ,nlines-matching-tooltip) layout-alist))
        (when (> nlines-below 0)
          (!cons `(:below . ,nlines-below) layout-alist))
        (nreverse layout-alist)))))

(defun company-quickdoc--render-sidewise (doc-lines position)
  "Show doc on the right side of company pseudo tooltip.

DOC-LINES is a list of doc string lines.  The 2nd arg POSITION, should be
either 'right, meaning showing the doc on the right side, or 'left, meaning left
side."
  (let* ((layout (company-quickdoc--get-layout (length doc-lines))))
    ;; (message "%s" layout)
    (dolist (i layout t)
      (let* ((doc-part-lines (cl-subseq doc-lines 0 (cdr i))))
        (setq doc-lines (cl-subseq doc-lines (cdr i)))
        (cond
         ((eq (car i) :above)
          (company-quickdoc--render-doc-part-above doc-part-lines position))
         ((eq (car i) :current-line)
          (company-quickdoc--render-doc-part-current-line doc-part-lines position))
         ((eq (car i) :matching-tooltip)
          (company-quickdoc--render-doc-part-matching-tooltip doc-part-lines position))
         ((eq (car i) :below)
          (company-quickdoc--render-doc-part-below doc-part-lines position)))))))

(defun company-quickdoc--render-stackwise (doc-strings position)
  "Show doc on the top or bottom of company pseudo tooltip.

DOC-STRINGS is a list of doc string lines.  The 2nd arg POSITION, should be
either 'top, meaning showing the doc on the top side, or 'bottom, meaning bottom
side."
  (let* ((ov company-pseudo-tooltip-overlay)
         (ncandidates (length company-candidates))
         (tooltip-abovep (nth 3 (overlay-get ov 'company-replacement-args)))
         (tooltip-height (abs (overlay-get ov 'company-height)))
         (tooltip-string (overlay-get ov 'company-display))
         (tooltip-strings
          (if tooltip-abovep
              (cl-subseq (split-string tooltip-string "[\n\v\f\r]") 0 -1)
            (split-string tooltip-string "[\n\v\f\r]"))))
    (cond
     ((eq position 'top)
      (if tooltip-abovep
          (let* ((doc-part-matching-tooltip
                  (and (< ncandidates tooltip-height)
                       (append
                        (if (< (length doc-strings) (- tooltip-height ncandidates))
                            (mapcar
                             (lambda (l) (substring l (+ 1 (window-hscroll))))
                             (cl-subseq tooltip-strings 0 (- (- tooltip-height ncandidates) (length doc-strings)))))
                        (cl-subseq doc-strings (- (min (length doc-strings) (- tooltip-height ncandidates))))
                        (mapcar
                         (lambda (l) (substring l (+ 1 (window-hscroll))))
                         (cl-subseq tooltip-strings (- ncandidates))))))

                 (doc-part-nlines-above
                  (if doc-part-matching-tooltip
                      (- (+ (length doc-strings) ncandidates) (length doc-part-matching-tooltip))
                    (length doc-strings)))
                 (doc-part-lines-above
                  (and (> doc-part-nlines-above 0)
                       (cl-subseq doc-strings (- doc-part-nlines-above)))))
            (when doc-part-matching-tooltip
              (company-quickdoc--render-doc-part-matching-tooltip doc-part-matching-tooltip position))
            (when doc-part-lines-above
              (company-quickdoc--render-doc-part-above doc-part-lines-above position)))
        (company-quickdoc--render-doc-part-above doc-strings position)))
     ((eq position 'bottom)
      (if tooltip-abovep
          (company-quickdoc--render-doc-part-below doc-strings position)
        (let* ((doc-part-matching-tooltip
                (and (< ncandidates tooltip-height)
                     (append (mapcar (lambda (l) (substring l (+ 1 (window-hscroll)))) (cl-subseq tooltip-strings 0 ncandidates))
                             (cl-subseq doc-strings 0 (min (length doc-strings) (- tooltip-height ncandidates))))))

               (doc-part-nlines-below
                (if doc-part-matching-tooltip
                    (- (+ (length doc-strings) ncandidates) (length doc-part-matching-tooltip))
                  (length doc-strings)))
               (doc-part-lines-below
                (and (> doc-part-nlines-below 0)
                     (cl-subseq doc-strings (- doc-part-nlines-below)))))
          (when doc-part-matching-tooltip
            (company-quickdoc--render-doc-part-matching-tooltip doc-part-matching-tooltip position))
          (when doc-part-lines-below
            (company-quickdoc--render-doc-part-below doc-part-lines-below position))))))))

(defun company-quickdoc--show ()
  "Show doc."
  (while-no-input
    (let* ((selected (nth company-selection company-candidates))
           (doc (let ((inhibit-message t))
                  (ignore-errors (company-quickdoc--doc selected))))
           (ov company-pseudo-tooltip-overlay)
           ;; (ov-str (overlay-get ov 'company-display))
           (tooltip-width (overlay-get ov 'company-width))
           (tooltip-height (abs (overlay-get ov 'company-height)))
           (company-column (overlay-get ov 'company-column))
           (window-width (company--window-width))
           (window-height (company--window-height))
           (horizontal-span (+ window-width (window-hscroll)))
           (tooltip-column (min (- horizontal-span tooltip-width) company-column))
           (tooltip-abovep (nth 3 (overlay-get ov 'company-replacement-args)))
           (current-row (cdr (company--col-row (line-beginning-position))))
           (remaining-cols-right
            (- (+ (company--window-width) (window-hscroll)) tooltip-column tooltip-width 2))
           (remaining-cols-left
            (- tooltip-column (window-hscroll) 5))
           (remaining-rows-top
            (- current-row
               (if tooltip-abovep (min tooltip-height (length company-candidates)) 0)))
           (remaining-rows-bottom
            (- window-height current-row
               (if tooltip-abovep 0 (min tooltip-height (length company-candidates))) 1)))
      (when (and ov doc)
        (let (doc-strings-right
              doc-strings-left
              doc-strings-top-bottom)
          (or
           (and (> remaining-cols-right 5)
                (setq doc-strings-right (company-quickdoc--format-string doc remaining-cols-right))
                (and (<= (length doc-strings-right) (company--window-height))
                     (company-quickdoc--render-sidewise doc-strings-right 'right)))
           (and (> remaining-cols-left 5)
                (setq doc-strings-left (company-quickdoc--format-string doc remaining-cols-left))
                (and (<= (length doc-strings-left) (company--window-height))
                     (company-quickdoc--render-sidewise doc-strings-left 'left)))
           (and (> remaining-rows-top 3)
                (setq doc-strings-top-bottom (company-quickdoc--format-string doc (- window-width 3)))
                (and (<= (length doc-strings-top-bottom) remaining-rows-top)
                     (company-quickdoc--render-stackwise doc-strings-top-bottom 'top)))
           (and (> remaining-rows-bottom 3)
                (or doc-strings-top-bottom
                    (setq doc-strings-top-bottom (company-quickdoc--format-string doc (- window-width 3))))
                (and (<= (length doc-strings-top-bottom) remaining-rows-bottom)
                     (company-quickdoc--render-stackwise doc-strings-top-bottom 'bottom)))
           (and t
                (let* ((area-right (* remaining-cols-right window-height))
                       (area-left (* remaining-cols-left window-height))
                       (area-top (* remaining-rows-top window-width))
                       (area-bottom (* remaining-rows-bottom window-width)))
                  (cond
                   ((>= area-right (max area-left area-top area-bottom))
                    (or doc-strings-right
                        (setq doc-strings-right (company-quickdoc--format-string doc remaining-cols-right)))
                    (company-quickdoc--render-sidewise (cl-subseq doc-strings-right 0 window-height) 'right))
                   ((>= area-left (max area-right area-top area-bottom))
                    (or doc-strings-left
                        (setq doc-strings-left (company-quickdoc--format-string doc remaining-cols-left)))
                    (company-quickdoc--render-sidewise (cl-subseq doc-strings-left 0 window-height) 'left))
                   ((>= area-top (max area-right area-left area-bottom))
                    (or doc-strings-top-bottom
                        (setq doc-strings-top-bottom (company-quickdoc--format-string doc (- window-width 3))))
                    (company-quickdoc--render-sidewise (cl-subseq doc-strings-top-bottom 0 remaining-rows-top) 'top))
                   ((>= area-bottom (max area-right area-left area-top))
                    (or doc-strings-top-bottom
                        (setq doc-strings-top-bottom (company-quickdoc--format-string doc (- window-width 3))))
                    (company-quickdoc--render-sidewise (cl-subseq doc-strings-top-bottom 0 remaining-rows-bottom) 'bottom))
                   )))))

        ;; (message "Changed overlay string: ---------------------------")
        ;; (message "%s" (or (overlay-get ov 'display) (overlay-get ov 'after-string)))
        ;; (message "End of changed overlay string: --------------------")
        ))))

(defun company-quickdoc--set-timer ()
  (company-quickdoc--hide)
  (when (or (null company-quickdoc--timer)
            (eq this-command #'company-quickdoc--manual-begin))
    (setq company-quickdoc--timer
          (run-with-idle-timer company-quickdoc-delay nil
                               'company-quickdoc--show))))

(defun company-quickdoc--cancel-timer ()
  (when (timerp company-quickdoc--timer)
    (cancel-timer company-quickdoc--timer)
    (setq company-quickdoc--timer nil)))

(defun company-quickdoc--enable ()
  (add-hook 'focus-out-hook #'company-cancel nil t)
  (make-local-variable 'company-frontends)
  (add-to-list 'company-frontends 'company-quickdoc-frontend :append))

(defun company-quickdoc--disable ()
  (remove-hook 'focus-out-hook #'company-cancel t)
  (company-quickdoc--cancel-timer)
  (setq-local company-frontends (delq 'company-quickdoc-frontend company-frontends)))

;;;###autoload
(define-minor-mode company-quickdoc-local-mode
  "Provides documentation popups for `company-mode' using `popup-tip'."
  :global nil
  (if company-quickdoc-local-mode
      (company-quickdoc--enable)
    (company-quickdoc--disable)))

;;;###autoload
(define-globalized-minor-mode company-quickdoc-mode
  company-quickdoc-local-mode company-quickdoc-local-mode)

(provide 'company-quickdoc)

;;; company-quickdoc.el ends here
