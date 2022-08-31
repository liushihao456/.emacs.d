;;; init-gnuplot.el --- Configurations for gnuplot	-*- lexical-binding: t -*-

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
;; Configurations for gnuplot
;; --------------------------------------

;;; Code:

(add-to-list 'auto-mode-alist '("\\.gnuplot\\'" . gnuplot-mode))
(add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
(with-eval-after-load 'gnuplot-mode
  (defun gnuplot-epslatex-generate-eps ()
    "Generate eps output in an epslatex terminal in gnuplot using dvips."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "set \\(?:terminal\\|term\\) epslatex[^\n]+standalone" nil t)
        (when (search-forward-regexp "set output \"\\([^\"]+\\)\.tex\"")
          (let ((output-file (match-string 1))
                (buffer (and (if (get-buffer "*shell-output*") (kill-buffer "*shell-output*") t)
                             (get-buffer-create "*shell-output*"))))
            (gnuplot-run-buffer)
            (pop-to-buffer buffer)
            (help-mode)
            (setq-local buffer-read-only nil)
            (erase-buffer)
            (shell-command
             (concat "latex " output-file ".tex && dvips -E "
                     output-file ".dvi -o " output-file ".eps")
             t)
            (setq-local buffer-read-only t))))))
  (define-key gnuplot-mode-map (kbd "C-c C-e") 'gnuplot-epslatex-generate-eps))

(provide 'init-gnuplot)

;;; init-gnuplot.el ends here
