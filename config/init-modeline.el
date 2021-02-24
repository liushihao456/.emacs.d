;;; init-modeline.el --- Configurations for modeline	-*- lexical-binding: t -*-

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
;; Configurations for modeline.
;; --------------------------------------

;;; Code:

;; Powerline
;; In terminal version of emacs, transparency must be set to 0 for the
;; separators' color to match
;; (require 'powerline)
;; (powerline-center-theme)

;; Tidy mode line
(defun tidy-modeline--fill (reserve)
  "Return empty space leaving RESERVE space on the right.  Adapted from powerline.el."
  (let ((real-reserve (if (and window-system (eq 'right (get-scroll-bar-mode)))
                          (- reserve 3)
                        reserve)))
    (propertize " "
                'display `((space :align-to (- (+ right right-fringe right-margin) ,real-reserve))))))


(defun tidy-modeline--fill-center (reserve)
  "Return empty space leaving RESERVE space on the right in order to center a string.  Adapted from powerline.el."
  (propertize " "
              'display `((space :align-to (- (+ center (0.5 . right-margin)) ,reserve
                                             (0.5 . left-margin))))))


(setq-default mode-line-format
              (list "%e"
                    '(:eval (cond (buffer-read-only " %*")
                                  ((buffer-modified-p) " *")
                                  (t " -")))
                    '(:eval (propertize " %P" 'help-echo "Position in buffer"))
                    ;; '(:eval (when (file-remote-p default-directory)
                    ;;           (propertize " %1@"
                    ;;                       'mouse-face 'mode-line-highlight
                    ;;                       'help-echo (concat "remote: " default-directory))))
                    '(:eval (propertize "  %12b" 'face 'mode-line-buffer-id 'help-echo default-directory))
                    " "
                    vc-mode
                    " "
                    '(:eval (let* ((modes (-remove #'(lambda (x) (or (equal x "(") (equal x ")"))) mode-line-modes)))
                              (list (tidy-modeline--fill-center (/ (length modes) 2)) modes)))
                    '(:eval (let* ((row (format-mode-line (list (propertize "%l" 'help-echo "Line number"))))
                                   (col (format-mode-line (list " : " (propertize "%c " 'help-echo "Column number"))))
                                   (col-length (max 5 (+ (length col))))
                                   (row-length (+ col-length (length row))))
                              (list
                               (tidy-modeline--fill row-length)
                               row
                               (tidy-modeline--fill col-length)
                               col)))))

(provide 'init-modeline)

;;; init-modeline.el ends here
