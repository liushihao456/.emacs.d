;;; init-yasnippet.el --- Configurations for yasnippet	-*- lexical-binding: t -*-

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
;; Configurations for yasnippet
;; --------------------------------------

;;; Code:

(with-eval-after-load 'yasnippet
  (defun yas-try-key-from-dot (_start-point)
    "As `yas-key-syntaxes' element, look for dot key.
It enables expanding `foo.' to `foo->'."
    (skip-chars-backward "\."))
  (add-to-list 'yas-key-syntaxes 'yas-try-key-from-dot)

  (setq yas-triggers-in-field t)
  (yas-reload-all))
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)
(add-hook 'org-mode-hook 'yas-minor-mode)

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
