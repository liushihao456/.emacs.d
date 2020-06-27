;;; company-quickdoc.el --- Popup documentation for completion candidates

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

(defcustom company-quickdoc-default-tip-width 80
  "Default popup tip width.

If not suitable, the tip width will adapt according to current window."
  :type 'integer
  :group 'company-quickdoc)

(defcustom company-quickdoc-max-lines nil
  "When not NIL, limits the number of lines in the popup."
  :type '(choice (integer :tag "Max lines to show in popup")
                 (const :tag "Don't limit the number of lines shown" nil))
  :group 'company-quickdoc)

(defface company-quickdoc-background
  '((((background light)) :background "#9E9E9E")
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

(defvar company-quickdoc--current-row nil
  "Current row the pointer is on, relative to window.")

(defvar-local company-quickdoc--timer nil
  "Quickhelp idle timer.")

(defvar-local company-quickdoc--original-overlay-start nil
  "The original start of company pesudo tooltip overlay start.")

(defvar-local company-quickdoc--original-overlay-end nil
  "The original start of company pesudo tooltip overlay end.")

(defun company-quickdoc-frontend (command)
  "`company-mode' front-end showing documentation in a popup."
  (pcase command
    (`show
     (setq company-quickdoc--current-row (company--row)))
    (`hide
     (setq company-quickdoc--current-row nil))
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
  "Manually trigger the `company-quickdoc' popup for the
currently active `company' completion candidate."
  (interactive)
  ;; This might seem a bit roundabout, but when I attempted to call
  ;; `company-quickdoc--show' in a more direct manner it triggered a
  ;; redisplay of company's list of completion candidates which looked
  ;; quite weird.
  (let ((company-quickdoc-delay 0.01))
    (company-quickdoc--set-timer)))

(defun company-quickdoc--hide ()
  "Hide the current quickdoc tip."
  (when (> (length company-quickdoc-overlays) 0)
    (dolist (element company-quickdoc-overlays)
      (delete-overlay element))))

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

(defun company-quickdoc--pos-eol (pos)
  "Get the end of line position at line that POS is on."
  (save-excursion (goto-char pos)
                  (line-end-position)))

(defun company-quickdoc--merge-docstrings (old-strings doc-strings position)
  "Concatenate DOC-STRINGS to OLD-STRINGS.

The 3rd arg RIGHT, if non-nil, means to concat to the right, otherwise to the left."
  (let* ((ov company-pseudo-tooltip-overlay)
         (tooltip-width (overlay-get ov 'company-width))
         (company-column (overlay-get ov 'company-column))
         (horizontal-span (+ (company--window-width) (window-hscroll)))
         (tooltip-column (min (- horizontal-span tooltip-width) company-column))
         (index-start (if (eq position 'right)
                          (+ tooltip-column tooltip-width)
                        (1+ (window-hscroll))))
         (result-strings nil))
    (company-quickdoc--merge-lines old-strings doc-strings index-start)))

(defun company-quickdoc--merge-lines (lines1 lines2 start)
  (let* ((index-start start)
         (index-end (+ index-start (length (car lines2))))
         (result-strings nil))
    (dotimes (i
              (max (length lines1) (length lines2))
              (nreverse result-strings))
      (!cons
       (cond
        ((not (nth i lines2))
         (nth i lines1))
        ((not (nth i lines1))
         (concat (truncate-string-to-width "" index-start nil ?\s)
                 (nth i lines2)))
        (t
         (concat (truncate-string-to-width (nth i lines1) index-start nil ?\s)
                 (nth i lines2)
                 (truncate-string-to-width
                  (nth i lines1)
                  (length (nth i lines1))
                  (min (length (nth i lines1)) index-end)
                  ?\s))))
       result-strings))))

(defun company-quickdoc--show-sidewise (doc-strings position)
  "Show doc on the right side of company pseudo tooltip.

DOC-STRINGS is a list of doc string lines.  The 2nd arg POSITION, should be
either 'right, meaning showing the doc on the right side, or 'left, meaning left
side."
  (let* ((right (eq position 'right))
         (ncandidates (length company-candidates))
         (ov company-pseudo-tooltip-overlay)
         (tooltip-height (overlay-get ov 'company-height))
         (tooltip-string (overlay-get ov 'company-display))
         (tooltip-strings (split-string tooltip-string "[\n\v\f\r]"))
         (tooltip-start-line (1+ company-quickdoc--current-row))
         ;; Above tooltip
         (extra-nlines-above (- (length doc-strings) (- (company--window-height) tooltip-start-line) 1))
         (extra-buffer-lines-above-end
          (and (> extra-nlines-above 0) (line-beginning-position)))
         (extra-buffer-lines-above-start
          (and extra-buffer-lines-above-end
               (company-quickdoc--pos-after-lines extra-buffer-lines-above-end (- extra-nlines-above))))
         (extra-buffer-string-above
          (and extra-buffer-lines-above-end
               (buffer-substring extra-buffer-lines-above-start extra-buffer-lines-above-end)))
         (extra-buffer-lines-above
          (and extra-buffer-string-above (split-string extra-buffer-string-above "[\n\v\f\r]")))
         (extra-doc-lines-above
          (and extra-buffer-lines-above
               (cl-subseq doc-strings 0 extra-nlines-above)))
         ;; Whether add overlay on current line
         (extra-buffer-line-current
          (and (>= extra-nlines-above 0)
               (buffer-substring (line-beginning-position) (line-end-position))))
         (extra-doc-line-current
          (and extra-buffer-line-current (cl-subseq doc-strings extra-nlines-above (1+ extra-nlines-above))))
         ;; Below tooltip
         (extra-nlines-below (- (length doc-strings) tooltip-height))
         (extra-buffer-lines-below-start
          (and (> extra-nlines-below 0)
               (company-quickdoc--pos-after-lines (line-beginning-position) (1+ tooltip-height))))
         (extra-buffer-lines-below-end
          (and extra-buffer-lines-below-start
               (company-quickdoc--pos-after-lines extra-buffer-lines-below-start (1+ extra-nlines-below))))
         (extra-buffer-string-below
          (and extra-buffer-lines-below-start
               (buffer-substring extra-buffer-lines-below-start extra-buffer-lines-below-end)))
         (extra-buffer-lines-below
          (and extra-buffer-string-below (split-string extra-buffer-string-below "[\n\v\f\r]")))
         (extra-doc-lines-below
          (and extra-buffer-lines-below
               (cl-subseq doc-strings
                          (+ tooltip-height (max 0 extra-nlines-above) (if extra-buffer-line-current 1 0)))))
         ;; Matching tooltip
         (matching-doc-lines
          (cl-subseq doc-strings
                     (+ (max 0 extra-nlines-above) (if extra-buffer-line-current 1 0))
                     (min (length doc-strings) (+ tooltip-height (max 0 extra-nlines-above) (if extra-buffer-line-current 1 0)))))
         ;; Company uses after-string in overlay when current point is on last
         ;; or second last line of buffer
         (use-after-string (overlay-get ov 'after-string))
         (tooltip-first-line
          (and use-after-string
               (string-empty-p (string-trim (car tooltip-strings)))
               (pop tooltip-strings)))
         (ov-start-col (save-excursion (goto-char (overlay-start ov)) (current-column))))
    ;; Add overlay above tooltip
    (when extra-doc-lines-above
      (let ((extra-overlay (make-overlay extra-buffer-lines-above-start extra-buffer-lines-above-end)))
        (!cons extra-overlay company-quickdoc-overlays)
        (--> (company-quickdoc--merge-docstrings extra-buffer-lines-above extra-doc-lines-above position)
             (string-join it "\n")
             (if (< (overlay-start extra-overlay) (overlay-end extra-overlay))
                 (overlay-put extra-overlay 'display it)
               (overlay-put extra-overlay 'after-string it)))
        (overlay-put extra-overlay 'window (selected-window))))
    ;; Add overlay on current line
    (when extra-buffer-line-current
      (if (and right tooltip-first-line)
          (setq tooltip-first-line
                (->
                 (company-quickdoc--merge-docstrings (split-string extra-buffer-line-current "[\n\v\f\r]") extra-doc-line-current position)
                 (string-join "\n")
                 (substring ov-start-col)))
        (let* ((extra-overlay-start (if right (point) (line-beginning-position)))
               (extra-overlay-end (if right (line-end-position) (- (point) 1)))
               (extra-overlay (make-overlay extra-overlay-start extra-overlay-end)))
          (!cons extra-overlay company-quickdoc-overlays)
          (--> (company-quickdoc--merge-docstrings (split-string extra-buffer-line-current "[\n\v\f\r]") extra-doc-line-current position)
               (string-join it "\n")
               (substring it (if right (current-column) 0) (if right nil (- (current-column) 1)))
               (if (< extra-overlay-start extra-overlay-end)
                   (overlay-put extra-overlay 'display it)
                 (overlay-put extra-overlay 'after-string it)))
          (overlay-put extra-overlay 'window (selected-window)))))
    ;; Add overlay below tooltip
    (when extra-doc-lines-below
      (let ((extra-overlay (make-overlay extra-buffer-lines-below-start extra-buffer-lines-below-end)))
        (!cons extra-overlay company-quickdoc-overlays)
        (--> (company-quickdoc--merge-docstrings extra-buffer-lines-below extra-doc-lines-below position)
             (string-join (if (and use-after-string (< ncandidates tooltip-height)) (!cons "" it) it) "\n")
             (if (< (overlay-start extra-overlay) (overlay-end extra-overlay))
                 (overlay-put extra-overlay 'display it)
               (overlay-put extra-overlay 'after-string it)))
        (overlay-put extra-overlay 'window (selected-window))))
    ;; Modify company tooltip overlay
    (--> (company-quickdoc--merge-docstrings tooltip-strings matching-doc-lines position)
         (string-join (if tooltip-first-line (!cons tooltip-first-line it) it) "\n")
         (overlay-put ov (if use-after-string 'after-string 'display) it))))

(defun company-quickdoc--show-stackwise (doc-strings position)
  "Show doc on the top or bottom of company pseudo tooltip.

DOC-STRINGS is a list of doc string lines.  The 2nd arg POSITION, should be
either 'top, meaning showing the doc on the top side, or 'bottom, meaning bottom
side."
  (let* ((ov company-pseudo-tooltip-overlay)
         (tooltip-height (overlay-get ov 'company-height))
         (tooltip-string (overlay-get ov 'company-display))
         (tooltip-strings (split-string tooltip-string "[\n\v\f\r]"))
         (tooltip-start-line (1+ company-quickdoc--current-row)))
    (cond
     ((eq position 'top)
      (let* ((extra-nlines-above (length doc-strings))
             (extra-buffer-lines-above-end
              (and (> extra-nlines-above 0) (line-beginning-position)))
             (extra-buffer-lines-above-start
              (and extra-buffer-lines-above-end
                   (company-quickdoc--pos-after-lines extra-buffer-lines-above-end (- extra-nlines-above))))
             (extra-buffer-string-above
              (and extra-buffer-lines-above-end
                   (buffer-substring extra-buffer-lines-above-start extra-buffer-lines-above-end)))
             (extra-buffer-lines-above
              (and extra-buffer-string-above (split-string extra-buffer-string-above "[\n\v\f\r]"))))
        (let ((extra-overlay (make-overlay extra-buffer-lines-above-start extra-buffer-lines-above-end)))
          (!cons extra-overlay company-quickdoc-overlays)
          (--> (company-quickdoc--merge-docstrings extra-buffer-lines-above doc-strings position)
               (string-join it "\n")
               (if (< (overlay-start extra-overlay) (overlay-end extra-overlay))
                   (overlay-put extra-overlay 'display it)
                 (overlay-put extra-overlay 'after-string it)))
          (overlay-put extra-overlay 'window (selected-window)))))
     ((eq position 'bottom)
      (let* (
             ;; Company uses after-string in overlay when current point is on last
             ;; or second last line of buffer
             (use-after-string (overlay-get ov 'after-string))
             (tooltip-first-line
              (and use-after-string
                   (string-empty-p (string-trim (car tooltip-strings)))
                   (pop tooltip-strings)))
             (ncandidates (length company-candidates))
             (matching-doc-lines
              (append (mapcar (lambda (l) (substring l (+ 1 (window-hscroll)))) (cl-subseq tooltip-strings 0 ncandidates))
                      (cl-subseq doc-strings 0 (min (length doc-strings) (- tooltip-height ncandidates)))))

             (extra-nlines-below (- (+ (length doc-strings) ncandidates) (length matching-doc-lines)))
             (extra-buffer-lines-below-start
              (and (> extra-nlines-below 0)
                   (company-quickdoc--pos-after-lines (line-beginning-position) (1+ tooltip-height))))
             (extra-buffer-lines-below-end
              (and extra-buffer-lines-below-start
                   (company-quickdoc--pos-after-lines extra-buffer-lines-below-start (1+ extra-nlines-below))))
             (extra-buffer-string-below
              (and extra-buffer-lines-below-start
                   (buffer-substring extra-buffer-lines-below-start extra-buffer-lines-below-end)))
             (extra-buffer-lines-below
              (and extra-buffer-string-below (split-string extra-buffer-string-below "[\n\v\f\r]")))
             (extra-doc-lines-below
              (and extra-buffer-lines-below
                   (cl-subseq doc-strings (- extra-nlines-below)))))
        ;; (message "%s" (string-join matching-doc-lines "\n"))
        (--> (company-quickdoc--merge-docstrings tooltip-strings matching-doc-lines position)
             (string-join (if tooltip-first-line (!cons tooltip-first-line it) it) "\n")
             (overlay-put ov (if use-after-string 'after-string 'display) it))
        (let ((extra-overlay (make-overlay extra-buffer-lines-below-start extra-buffer-lines-below-end)))
          (!cons extra-overlay company-quickdoc-overlays)
          (--> (company-quickdoc--merge-docstrings extra-buffer-lines-below extra-doc-lines-below position)
               (string-join (if (and use-after-string (< ncandidates tooltip-height)) (!cons "" it) it) "\n")
               (if (< (overlay-start extra-overlay) (overlay-end extra-overlay))
                   (overlay-put extra-overlay 'display it)
                 (overlay-put extra-overlay 'after-string it)))
          (overlay-put extra-overlay 'window (selected-window))))))))

(defun company-quickdoc--show ()
  "Override `company-quickdoc--show' function from `company-quickdoc'."
  (company-quickdoc--cancel-timer)
  (while-no-input
    (let* ((selected (nth company-selection company-candidates))
           (doc (let ((inhibit-message t))
                  (ignore-errors (company-quickdoc--doc selected))))
           (ov company-pseudo-tooltip-overlay)
           (ov-str (overlay-get ov 'company-display))
           (tooltip-width (overlay-get ov 'company-width))
           (tooltip-height (overlay-get ov 'company-height))
           (company-column (overlay-get ov 'company-column))
           (window-width (company--window-width))
           (horizontal-span (+ window-width (window-hscroll)))
           (tooltip-column (min (- horizontal-span tooltip-width) company-column))
           (remaining-cols-right (- (+ (company--window-width) (window-hscroll)) tooltip-column tooltip-width 2))
           (remaining-cols-left (- tooltip-column (window-hscroll) 5))
           (remaining-rows-top company-quickdoc--current-row)
           (remaining-rows-bottom (- (window-height) company-quickdoc--current-row (length company-candidates) 2)))
      (when (and ov doc)
        ;; (message "%d" (length company-candidates))
        ;; (message "rb %d %d %d" remaining-rows-top remaining-rows-bottom tooltip-height)
        ;; (message "Doc: ----------------------------------------------")
        ;; (message "%s" doc)
        ;; (message "Overlay string: -----------------------------------")
        ;; (message "%s" ov-str)
        ;; (message "company-width: -----------------------------------")
        ;; (message "%d" (overlay-get ov 'company-width))
        ;; (message "company-column: -----------------------------------")
        ;; (message "%d" (overlay-get ov 'company-column))
        ;; (message "column: ------------------------------------------")
        ;; (message "%d" (current-column))
        ;; (message "company-height: -----------------------------------")
        ;; (message "%d" (overlay-get ov 'company-height))
        ;; (message "company-replacement-args: -------------------------")
        ;; (message "%s" (overlay-get ov 'company-replacement-args))
        ;; (message "company-prefix: -----------------------------------")
        ;; (message company-prefix)
        ;; (message "horizontal span: -----------------------------------")
        ;; (message "%d" (+ (company--window-width) (window-hscroll)))
        ;; (message "company--col-row: ---------------------------------")
        ;; (message "%s" (company--col-row (point)))
        ;; (message "company--window-width: ----------------------------")
        ;; (message "%d %d" (company--window-width) (window-width))
        ;; (message "company--window-height: ---------------------------")
        ;; (message "%d %d" (company--window-height) (window-height))
        ;; (message "company--row: ---------------------------------------")
        ;; (message "%d" (company--row))
        ;; (message "company-tooltip-offset: ---------------------------")
        ;; (message "%d" company-tooltip-offset)
        ;; (message "overlay-start: -----------------------------------")
        ;; (message "%d" (overlay-start ov))
        ;; (message "overlay-end: -----------------------------------")
        ;; (message "%d" (overlay-end ov))
        ;; (message "%s" (string-join doc-strings "\n"))
        (or
         (and (> remaining-cols-right 5)
          (let ((doc-strings (company-quickdoc--format-string doc remaining-cols-right)))
            (and (<= (length doc-strings) (company--window-height))
                 (company-quickdoc--show-sidewise doc-strings 'right))))
         (and (> remaining-cols-left 5)
          (let ((doc-strings (company-quickdoc--format-string doc remaining-cols-left)))
            (and (<= (length doc-strings) (company--window-height))
                 (company-quickdoc--show-sidewise doc-strings 'left))))
         (and t
          (let ((doc-strings (company-quickdoc--format-string doc (- window-width 3))))
            (cond
             ((and (> remaining-rows-top 3) (<= (length doc-strings) remaining-rows-top))
              (company-quickdoc--show-stackwise doc-strings 'top))
             ((and (> remaining-rows-bottom 3) (<= (length doc-strings) remaining-rows-bottom))
              (company-quickdoc--show-stackwise doc-strings 'bottom))
             ((>= remaining-rows-top remaining-rows-bottom)
              (company-quickdoc--show-stackwise (cl-subseq doc-strings 0 remaining-rows-top) 'top))
             ((>= remaining-rows-bottom remaining-rows-top)
              (company-quickdoc--show-stackwise (cl-subseq doc-strings 0 remaining-rows-bottom) 'bottom))
             ))))

        ;; (message "Changed overlay string: ---------------------------")
        ;; (message "%s" (or (overlay-get ov 'display) (overlay-get ov 'after-string)))
        ;; (message "End of changed overlay string: --------------------")
        ))))

(defun company-quickdoc--set-timer ()
  (when (or (null company-quickdoc--timer)
            (eq this-command #'company-quickdoc--manual-begin))
    (setq company-quickdoc--timer
          (run-with-idle-timer company-quickdoc-delay nil
                               'company-quickdoc--show))))

(defun company-quickdoc--cancel-timer ()
  (when (timerp company-quickdoc--timer)
    (cancel-timer company-quickdoc--timer)
    (setq company-quickdoc--timer nil)))

;; (defun company-quickdoc--popup-tip-scroll-up ()
;;   "Scroll up 3 lines of the popup tip."
;;   (interactive)
;;   (when company-quickdoc-popup-tip
;;     (popup-scroll-up company-quickdoc-popup-tip 3))
;;   )

;; (defun company-quickdoc--popup-tip-scroll-down ()
;;   "Scroll up 3 lines of the popup down."
;;   (interactive)
;;   (when company-quickdoc-popup-tip
;;     (popup-scroll-down company-quickdoc-popup-tip 3))
;;   )

;; (define-key company-active-map "\M-p" 'company-quickdoc--popup-tip-scroll-up)
;; (define-key company-active-map "\M-n" 'company-quickdoc--popup-tip-scroll-down)

(defun company-quickdoc--enable ()
  (add-hook 'focus-out-hook #'company-cancel nil t)
  (setq-local company-quickdoc--original-tooltip-width company-tooltip-minimum-width)
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
