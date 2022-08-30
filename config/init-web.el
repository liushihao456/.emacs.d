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

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . react-mode))
;; Use react mode for .jsx files as the builtin js-mode has bugs:
;; ``internal--syntax-propertize did not move syntax-propertize--done''
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . react-mode))
(defun setup-ts-js ()
  "Setup development environment for ts(x) and js(x) files."
  (unless (derived-mode-p 'json-mode)
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?\' . ?\')) '((?\` . ?\`))))
    (setq-local electric-pair-text-pairs electric-pair-pairs)
    (lsp)
    (emmet-mode)
    (setq-local emmet-expand-jsx-className? t)))
(add-hook 'js-mode-hook 'setup-ts-js)
(add-hook 'typescript-mode-hook (lambda ()
                                  (setup-ts-js)
                                  (when (fboundp 'yas-activate-extra-mode)
                                    (yas-activate-extra-mode 'js-mode))))
(add-hook 'react-mode-hook (lambda ()
                             (setup-ts-js)
                             (when (fboundp 'yas-activate-extra-mode)
                               (yas-activate-extra-mode 'js-mode)
                               (yas-activate-extra-mode 'typescript-mode))))

(add-hook 'mhtml-mode-hook 'emmet-mode)
(with-eval-after-load 'sgml-mode
  (setq sgml-basic-offset 4))

(defun prettier-buffer ()
  "Organize imports and call prettier to format buffer."
  (interactive)
  (when (fboundp 'lsp-organize-imports) (lsp-organize-imports))
  (when (fboundp 'prettier-js) (prettier-js)))

(with-eval-after-load 'typescript-mode
  (define-key typescript-mode-map (kbd "<f5>") 'prettier-buffer))
(with-eval-after-load 'react-mode
  (define-key react-mode-map (kbd "<f5>") 'prettier-buffer))
(with-eval-after-load 'js-mode
  (define-key js-mode-map (kbd "<f5>") 'prettier-buffer))

(provide 'init-web)

;;; init-web.el ends here
