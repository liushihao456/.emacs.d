;;; init-web.el --- Configurations for web development	-*- lexical-binding: t -*-

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
;; Configurations for web development
;; --------------------------------------

;;; Code:

;; Web mode and emmet mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-hook 'web-mode-hook (lambda ()
                           (setq-local electric-pair-pairs (append electric-pair-pairs '((?\' . ?\'))))
                           (setq-local electric-pair-text-pairs electric-pair-pairs)
                           (cond
                            ((string-match-p "\\.\\(ts\\|js\\)\\'" (buffer-name))
                             (lsp)
                             (yas-activate-extra-mode 'js-mode))
                            ((string-match-p "\\.\\(jsx\\|tsx\\)\\'" (buffer-name))
                             (lsp)
                             (emmet-mode)
                             (setq-local emmet-expand-jsx-className? t)
                             (yas-activate-extra-mode 'js-mode))
                            ((string-match-p "\\.html?\\'" (buffer-name))
                             (lsp)
                             (emmet-mode)))))

(with-eval-after-load 'web-mode
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  ;; Prettier
  (define-key web-mode-map (kbd "<f5>")
    (lambda () (interactive)
      (lsp-organize-imports)
      (prettier-js))))

(provide 'init-web)

;;; init-web.el ends here
