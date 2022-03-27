;;; treemacs-nerd.el --- Treemacs theme with nerd icons  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: treemacs nerd icon
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3") (treemacs "2.9.5"))

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
;; A treemacs theme with nerd font icons that work in TUI.
;; --------------------------------------

;;; Usage:
;;
;; --------------------------------------

;;; Code:
(require 'treemacs)

(defgroup treemacs-nerd nil
  "Options for treemacs-nerd theme."
  :group 'treemacs-nerd)

(with-eval-after-load 'treemacs
  (treemacs-create-theme "treemacs-nerd"
    :config
    (progn
      ;; book
      (treemacs-create-icon :icon (propertize " " 'face 'treemacs-term-node-face) :fallback 'same-as-icon :extensions (root-open))
      (treemacs-create-icon :icon (propertize " " 'face 'treemacs-term-node-face) :fallback 'same-as-icon :extensions (root-closed))
      ;; chevron
      (treemacs-create-icon :icon (propertize " " 'face 'treemacs-term-node-face) :fallback 'same-as-icon :extensions (dir-closed))
      (treemacs-create-icon :icon (propertize " " 'face 'treemacs-term-node-face) :fallback 'same-as-icon :extensions (dir-open))
      ;; tag
      (treemacs-create-icon :icon (propertize "炙" 'face 'font-lock-constant-face) :fallback 'same-as-icon :extensions (tag-leaf))
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-funtion-name-face)   :fallback 'same-as-icon :extensions (tag-open))
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-funtion-name-face)   :fallback 'same-as-icon :extensions (tag-closed))
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-warning-face)   :fallback 'same-as-icon :extensions (error))
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-warning-face)   :fallback 'same-as-icon :extensions (warning))
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-warning-face)   :fallback 'same-as-icon :extensions (info))
      ;; code
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-string-face)   :fallback 'same-as-icon :extensions (fallback))
      ))
  (treemacs-load-theme "treemacs-nerd"))

;;;###autoload
(defun treemacs-nerd-config ()
  "Install treemacs-nerd theme configuration.")

(provide 'treemacs-nerd)

;;; company-tip.el ends here
