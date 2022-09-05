;;; init-font.el --- Configurations for font	-*- lexical-binding: t -*-

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
;; Configurations for font.
;; --------------------------------------

;;; Code:

;; When in GUI, set fonts
(when (display-graphic-p)
  (defun load-local-config-font ()
    "Load the font specified in .emacs.d/local-config.el.

Return t if successfully loaded the font."
    (let ((local-config-file
           (file-name-concat user-emacs-directory "local-config.el"))
          (loaded))
      (when (file-exists-p local-config-file)
        (require 'local-config local-config-file)
        (when (and font-to-use font-size-to-use)
          (add-to-list 'default-frame-alist `(font . ,(concat font-to-use "-" font-size-to-use)))
          (set-face-attribute 'fixed-pitch nil :family font-to-use)
          (set-face-attribute 'fixed-pitch-serif nil :family font-to-use)
          ;; Fix unicode font height bug on macOS
          (set-fontset-font "fontset-default" 'unicode (concat "Menlo" "-" font-size-to-use))
          (setq loaded t)))
      (unless loaded
        (message "No theme loaded from .emacs.d/local-config.el file."))
      loaded))
  (load-local-config-font))

(provide 'init-font)

;;; init-font.el ends here
