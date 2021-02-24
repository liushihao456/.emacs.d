;;; init-cpp.el --- Configurations for C/C++	-*- lexical-binding: t -*-

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
;; Configurations for C/C++
;; --------------------------------------

;;; Code:

(dolist (m (list 'c-mode-hook 'c++-mode-hook 'objc-mode-hook))
  (add-hook m (lambda ()
                (lsp)
                (setq-local company-backends (delete 'company-clang company-backends))
                (setq comment-start "/* "
                      comment-end " */"))))
(with-eval-after-load 'cc-mode
  (defun my/cmake-project-generate-compile-commands ()
    "Generate the compile_commands.json file containing build flags in a cmake
project in order for clangd to understand the project code."
    (interactive)
    (let ((default-directory (cdr (project-current))))
      (shell-command
       (concat "cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES"
               "\nln -s Debug/compile_commands.json"))))
  (dolist (m (list c-mode-map c++-mode-map objc-mode-map))
    (define-key m (kbd "C-c l s") 'my/cmake-project-generate-compile-commands)))

;; Cmake
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/cmake")
(autoload 'cmake-mode "/usr/local/share/emacs/site-lisp/cmake/cmake-mode.el" "Cmake mode autoload" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(with-eval-after-load 'cmake-mode
  (setq cmake-tab-width 4))

(provide 'init-cpp)

;;; init-cpp.el ends here
