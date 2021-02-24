;;; init-delight.el --- De-light package mode-line lighters	-*- lexical-binding: t -*-

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
;; De-light package mode-line lighters
;; --------------------------------------

;;; Code:

(delight '((eldoc-mode nil "eldoc")
           (emacs-lisp-mode "Elisp" :major)
           (which-key-mode nil "which-key")
           (abbrev-mode nil "abbrev")
           (lsp-mode nil "lsp-mode")
           (company-mode nil "company")
           (yas-minor-mode nil "yasnippet")
           (auto-revert-mode nil "autorevert")
           (hi-lock-mode nil "hi-lock")
           (auto-fill-function nil "simple")
           (emmet-mode nil "emmet-mode")))

(provide 'init-delight)

;;; init-delight.el ends here
