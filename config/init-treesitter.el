;;; init-treesitter.el --- Configurations for emacs-tree-sitter	-*- lexical-binding: t -*-

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
;; Configurations for emacs-tree-sitter.
;; --------------------------------------

;;; Code:

(with-eval-after-load 'tree-sitter
  (require 'tree-sitter-langs)
  (setq tree-sitter-major-mode-language-alist (cons '(typescript-mode . tsx) tree-sitter-major-mode-language-alist)))

(add-hook 'c-mode-common-hook 'tree-sitter-mode)
(add-hook 'python-mode-hook 'tree-sitter-mode)
(add-hook 'java-mode-hook 'tree-sitter-mode)
(add-hook 'js-mode-hook 'tree-sitter-mode)
(add-hook 'typescript-mode-hook 'tree-sitter-mode)
(add-hook 'css-mode-hook 'tree-sitter-mode)
(add-hook 'mhtml-mode-hook 'tree-sitter-mode)

(add-hook 'tree-sitter-after-on-hook (lambda ()
                                       (setq font-lock-defaults '(nil))
                                       (tree-sitter-hl-mode)))

(provide 'init-treesitter)

;;; init-treesitter.el ends here
