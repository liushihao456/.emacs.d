;;; company-quickdoc.el --- Popup documentation for completion candidates

;; Copyright (C) 2016, Lars Andersen

;; Author: Lars Andersen <expez@expez.com>
;; URL: https://www.github.com/expez/company-quickdoc
;; Package-Version: 20180525.1003
;; Keywords: company popup documentation quickdoc
;; Version: 2.2.0
;; Package-Requires: ((emacs "24.3") (company "0.8.9") (pos-tip "0.4.6"))

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
(require 'pos-tip)
(require 'popup)
(require 'cl-lib)

(defgroup company-quickdoc nil
  "Documentation popups for `company-mode'"
  :group 'company)

(defcustom company-quickdoc-use-propertized-text nil
  "Allow the text to have properties like color, font size, etc."
  :type '(choice (boolean :tag "Allow"))
  :group 'company-quickdoc)

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

(defcustom company-quickdoc-color-foreground nil
  "Popup text foreground color."
  :type '(choice (color)
                 (const :tag "Default" nil))
  :group 'company-quickdoc)

(defcustom company-quickdoc-color-background nil
  "Popup text background color."
  :type '(choice (color)
                 (const :tag "Default" nil))
  :group 'company-quickdoc)

(defvar-local company-quickdoc-pos-tip-available (featurep 'pos-tip)
  "Whether popup-tip is available.")

(defvar-local company-quickdoc-popup-available (featurep 'popup)
  "Whether popup-tip is available.")

(defvar-local company-quickdoc-use-pos-tip (and (display-graphic-p) company-quickdoc-pos-tip-available)
  "Whether use pos-tip to render documentation tip.")

(defvar-local company-quickdoc-popup-tip nil
  "Quickhelp popup-tip instance")

(defvar-local company-quickdoc--timer nil
  "Quickhelp idle timer.")

(defvar-local company-quickdoc--original-tooltip-width company-tooltip-minimum-width
  "The documentation popup breaks inexplicably when we transition
  from a large pseudo-tooltip to a small one.  We solve this by
  overriding `company-tooltip-minimum-width' and save the
  original value here so we can restore it.")

(defun company-quickdoc-frontend (command)
  "`company-mode' front-end showing documentation in a popup."
  (pcase command
    (`pre-command
     (when company-quickdoc-delay
       (company-quickdoc--cancel-timer)
       (company-quickdoc--hide)))
    (`post-command
     (when company-quickdoc-delay
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
  (when company-quickdoc-use-pos-tip
    (with-no-warnings
      (pos-tip-hide)))
  (when company-quickdoc-popup-tip
    (popup-delete company-quickdoc-popup-tip)
    (setq company-quickdoc-popup-tip nil)))

(defun company-quickdoc--show ()
  "Override `company-quickdoc--show' function from `company-quickdoc'."
  (when company-quickdoc-pos-tip-available
    (company-quickdoc--cancel-timer)
    (while-no-input
      (let* ((selected (nth company-selection company-candidates))
             (doc (let ((inhibit-message t))
                    (company-quickdoc--doc selected)))
             (width 80)
             (timeout 300)
             (ovl company-pseudo-tooltip-overlay)
             (overlay-width (* (frame-char-width)
                               (if ovl (overlay-get ovl 'company-width) 0)))
             (overlay-position (* (frame-char-width)
                                  (- (if ovl (overlay-get ovl 'company-column) 1) 1)))
             (fg-bg `(,company-quickdoc-color-foreground
                      . ,company-quickdoc-color-background)))
        (when (and ovl doc)
          (with-no-warnings
            (if company-quickdoc-use-propertized-text
                (let* ((frame (window-frame (selected-window)))
                       (max-width (pos-tip-x-display-width frame))
                       (max-height (pos-tip-x-display-height frame))
                       (w-h (pos-tip-string-width-height doc)))
                  (cond
                   ((> (car w-h) width)
                    (setq doc (pos-tip-fill-string doc width nil 'none nil max-height)
                          w-h (pos-tip-string-width-height doc)))
                   ((or (> (car w-h) max-width)
                        (> (cdr w-h) max-height))
                    (setq doc (pos-tip-truncate-string doc max-width max-height)
                          w-h (pos-tip-string-width-height doc))))
                  (if company-quickdoc-use-pos-tip
                      (pos-tip-show-no-propertize doc fg-bg (overlay-start ovl) nil timeout
                                                  (pos-tip-tooltip-width (car w-h) (frame-char-width frame))
                                                  (pos-tip-tooltip-height (cdr w-h) (frame-char-height frame) frame)
                                                  nil (+ overlay-width overlay-position) 1)
                    (setq company-quickdoc-popup-tip (popup-tip doc :point (overlay-start ovl)
                                                                :width (pos-tip-tooltip-width (car w-h) (frame-char-width frame))
                                                                :height (pos-tip-tooltip-height (cdr w-h) (frame-char-height frame) frame)
                                                                :nowait t
                                                                :nostrip nil))))
              (if company-quickdoc-use-pos-tip
                  (pos-tip-show doc fg-bg (overlay-start ovl) nil timeout width nil
                                (+ overlay-width overlay-position) 1)
                (setq company-quickdoc-popup-tip (popup-tip doc :point (overlay-start ovl)
                                                            :width width
                                                            :nowait t
                                                            :nostrip t))))))))))

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
;;   (when (and (not company-quickdoc-use-pos-tip)
;;              company-quickdoc-popup-tip)
;;     (popup-scroll-up company-quickdoc-popup-tip 3))
;;   )

;; (defun company-quickdoc--popup-tip-scroll-down ()
;;   "Scroll up 3 lines of the popup down."
;;   (interactive)
;;   (when (and (not company-quickdoc-use-pos-tip)
;;              company-quickdoc-popup-tip)
;;     (popup-scroll-down company-quickdoc-popup-tip 3))
;;   )

;; (define-key company-active-map "\M-p" 'company-quickdoc--popup-tip-scroll-up)
;; (define-key company-active-map "\M-n" 'company-quickdoc--popup-tip-scroll-down)

(defun company-quickdoc--enable ()
  (add-hook 'focus-out-hook #'company-cancel nil t)
  (setq-local company-quickdoc--original-tooltip-width company-tooltip-minimum-width)
  (setq-local company-tooltip-minimum-width (max company-tooltip-minimum-width 40))
  (make-local-variable 'company-frontends)
  (add-to-list 'company-frontends 'company-quickdoc-frontend :append))

(defun company-quickdoc--disable ()
  (remove-hook 'focus-out-hook #'company-cancel t)
  (company-quickdoc--cancel-timer)
  (setq-local company-tooltip-minimum-width company-quickdoc--original-tooltip-width)
  (setq-local company-frontends (delq 'company-quickdoc-frontend company-frontends)))

;;;###autoload
(define-minor-mode company-quickdoc-local-mode
  "Provides documentation popups for `company-mode' using `pos-tip'."
  :global nil
  (if company-quickdoc-local-mode
      (company-quickdoc--enable)
    (company-quickdoc--disable)))

;;;###autoload
(define-globalized-minor-mode company-quickdoc-mode
  company-quickdoc-local-mode company-quickdoc-local-mode)

(provide 'company-quickdoc)

;;; company-quickdoc.el ends here
