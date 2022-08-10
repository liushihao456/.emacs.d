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
  "Convert decimal fraction TIME like 18.555 to list of (hours minutes)."
  (let* ((time (round (* 60 time)))
         (24-hours (/ time 60))
         (minutes (% time 60)))
    (list 24-hours minutes)))

(defun get-sunset-time ()
  "Return the sunset time as list (hours minutes)."
  (let ((l (solar-sunrise-sunset (calendar-current-date))))
    (hour-fraction-to-time (caadr l))))

(defun get-current-time ()
  "Return the current time as list (hours minutes)."
  (let ((str (current-time-string)))
    (list
     (string-to-number (substring str -13 -11))
     (string-to-number (substring str -10 -8)))))

(defun time-before (time1 time2)
  "Compare TIME1 and TIME2, both in the form of list (hours minutes)."
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

(defun load-spacemacs-theme ()
  "Load spacemacs theme."
  (load-theme 'spacemacs-dark t))

;; (let ((current-time (get-current-time))
;;       (sunset-time (get-sunset-time)))
;;   (message "Sunset time: %s: %s" (car sunset-time) (cadr sunset-time))
;;   (setcar sunset-time (- (car sunset-time) 1))
;;   (if (time-before current-time sunset-time)
;;       (load-zenburn)
;;     (load-solarized-dark)))

;; (load-solarized-dark)
;; (load-zenburn)
;; (load-spacemacs-theme)

(defun load-base16-theme (&optional theme)
  "Load base16 theme.

If called with the optional arg THEME, then THEME is loaded, otherwise the theme
in ``~/.vimrc_background'' which base16-shell produces is loaded.

THEME could be a string or a symbol."
  (if theme
      (cond ((symbolp theme)
             (load-theme theme t))
            ((stringp theme)
             (load-theme (intern theme) t)))
    (with-temp-buffer
      (insert-file-contents "~/.vimrc_background")
      (save-match-data
        (let ((theme1 (buffer-substring-no-properties (search-forward "colorscheme ") (point-at-eol))))
          (message "Loaded %s theme" theme1)
          (load-theme (intern theme1) t)))
      )))

;; (load-theme 'doom-one t)
(setq base16-theme-256-color-source 'base16-shell)
(if (display-graphic-p)
    (load-base16-theme 'base16-tomorrow-night-eighties)
  (load-base16-theme))
(setq frame-background-mode 'dark)

(unless (display-graphic-p)
  (set-face-background 'default "unspecified-bg"))

(provide 'init-theme)

;;; init-theme.el ends here
