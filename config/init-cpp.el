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

;; Use c++-mode for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-common-hook 'lsp)
(with-eval-after-load 'cc-mode
  (defun my/cmake-project-generate-compile-commands ()
    "Generate the compile_commands.json file containing build flags in a cmake
project in order for clangd to understand the project code."
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (shell-command
       (if (memq system-type '(ms-dos windows-nt cygwin))
           "cmake . -G Ninja -Bbuild -DCMAKE_EXPORT_COMPILE_COMMANDS=YES"
         "cmake . -Bbuild -DCMAKE_EXPORT_COMPILE_COMMANDS=YES"))))
  (define-key c-mode-base-map (kbd "C-c l s") 'my/cmake-project-generate-compile-commands))

;; Cmake
(when (executable-find "cmake")
  (require 'dash)
  (setq cmake-load-path (--> "cmake"
                             (executable-find it)
                             (file-truename it)
                             (file-name-concat it ".." ".." "share" "emacs" "site-lisp")
                             ;; cmake-mode.el either resides in cmake/share/emacs/site-lisp/
                             ;; or cmake/share/emacs/site-lisp/cmake/
                             (if (file-exists-p (file-name-concat it "cmake-mode.el"))
                                 it
                               (file-name-concat it "cmake"))
                             (file-truename it)))
  (add-to-list 'load-path cmake-load-path)
  (autoload 'cmake-mode (file-name-concat cmake-load-path "cmake-mode.el") "Major mode for editing CMake listfiles." t)
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
  (when (executable-find "cmake-language-server")
    (add-hook 'cmake-mode-hook 'lsp)))

(provide 'init-cpp)

;;; init-cpp.el ends here
