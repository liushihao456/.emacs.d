;;; init-latex.el --- Configurations for LaTeX	-*- lexical-binding: t -*-

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
;; Configurations for LaTeX
;; --------------------------------------

;;; Code:

(with-eval-after-load 'latex
  (setq TeX-auto-save t)
  (setq TeX-command-extra-options "-shell-escape")
  (setq TeX-engine 'xetex)
  (setq TeX-parse-self t)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  ;; (add-hook 'LaTeX-mode-hook 'cdlatex-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq font-latex-fontify-sectioning 'color)
  (setq-default TeX-master nil))

(with-eval-after-load 'font-latex
  (set-face-attribute 'font-latex-slide-title-face
                      nil
                      :height 'unspecified
                      :weight 'unspecified
                      :inherit 'font-lock-type-face))


(provide 'init-latex)

;;; init-latex.el ends here
