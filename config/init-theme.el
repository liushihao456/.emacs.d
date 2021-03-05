;;; init-theme.el --- Configurations for theme	-*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Configurations for theme.
;; --------------------------------------

;;; Code:
(require 'solar)
(with-eval-after-load 'solar
  (setq calendar-latitude 32.0105)
  (setq calendar-longitude 112.0856))

(defun hour-fraction-to-time (time)
  "Convert decimal fraction TIME like 18.555 to time string HH:mm."
  (let* ((time (round (* 60 time)))
         (24-hours (/ time 60))
         (minutes (% time 60)))
    (list 24-hours minutes)))

(defun get-sunset-time ()
  "Return the sunset time as list (hours, minutes)."
  (let ((l (solar-sunrise-sunset (calendar-current-date))))
    (hour-fraction-to-time (caadr l))))

(defun get-current-time ()
  "Return the current time as list (hours, minutes)."
  (let ((str (current-time-string)))
    (list
     (string-to-number (substring str -13 -11))
     (string-to-number (substring str -10 -8)))))

(defun time-before (time1 time2)
  "Compare TIME1 and TIME2, both in the form of list (hours, minutes)."
  (if (= (car time1) (car time2))
      (< (cadr time1) (cadr time2))
    (< (car time1) (car time2))))

(defun load-solarized-dark ()
  "Load solarized-dark theme."
  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)
  ;; Use less bolding
  ;; (setq solarized-use-less-bold t)
  ;; Use more italics
  ;; (setq solarized-use-more-italic t)
  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  (setq solarized-emphasize-indicators nil)
  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark t))

(defun load-zenburn ()
  "Load zenburn theme."
  (load-theme 'zenburn t)
  (custom-set-faces
   `(lsp-ui-doc-background ((t (:background "#272A36"))))))

(if (time-before (get-current-time) (get-sunset-time))
    (load-zenburn)
  (load-solarized-dark))

(unless (display-graphic-p)
  (set-face-background 'default "unspecified-bg"))

(provide 'init-theme)

;;; init-theme.el ends here
