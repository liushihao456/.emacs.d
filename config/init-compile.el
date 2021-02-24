;;; init-compile.el --- Configurations for compilation	-*- lexical-binding: t -*-

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
;; Configurations for compiling in Emacs.
;; --------------------------------------

;;; Code:

(defun my/compile-project ()
  "Compile the project."
  (interactive)
  (let ((default-directory (cdr (project-current))))
    (call-interactively 'compile)))
(global-set-key (kbd "C-c m") 'my/compile-project)

(with-eval-after-load 'compile
  (setq compilation-save-buffers-predicate
        '(lambda nil
           (string-prefix-p
            (cdr
             (project-current))
            (file-truename
             (buffer-file-name)))))
  (setq compilation-scroll-output t)

  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    "Apply ansi color rendering in compilation buffer."
    (ansi-color-apply-on-region compilation-filter-start (point-max)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (add-hook 'compilation-mode-hook (lambda () (pop-to-buffer (buffer-name)))))



(provide 'init-compile)

;;; init-compile.el ends here
