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
  (when (and local-config-font local-config-font-size)
    (add-to-list 'default-frame-alist
                 `(font . ,(concat local-config-font
                                   "-"
                                   local-config-font-size)))
    (set-face-attribute 'fixed-pitch nil :family local-config-font)
    (set-face-attribute 'fixed-pitch-serif nil :family local-config-font)
    (when (boundp local-config-chinese-font)
      (set-fontset-font t 'han local-config-chinese-font))
    ;; Fix unicode font height bug on macOS
    (when (memq window-system '(mac ns))
      (set-fontset-font t 'unicode
                        (concat "Menlo" "-" local-config-font-size)))))

(provide 'init-font)

;;; init-font.el ends here
