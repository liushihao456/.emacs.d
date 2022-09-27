;;; init-java.el --- Configurations for Java	-*- lexical-binding: t -*-

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
;; Configurations for Java
;; --------------------------------------

;;; Code:

(defun java-compile-run-current-main-class ()
  "If the current java file contains main method, compile project and run it."
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (file-class-name (file-name-base buffer-file-name))
         (compile-command
          (save-excursion
            (goto-char (point-min))
            (if (and
                 (search-forward-regexp
                  (concat "public[[:space:]]+\\(?:abstract[[:space:]]+\\)?class[[:space:]]+" file-class-name "\s?[^{]*{")
                  nil t)
                 (search-forward-regexp
                  (regexp-quote "public static void main(String[] args)")
                  nil t)
                 (search-backward-regexp "package\s+\\(.+?\\);" nil t))
                (concat "mvn compile exec:java -Dexec.mainClass=\""
                        (match-string 1)
                        "." file-class-name "\"")
              "mvn compile"))))
    (call-interactively 'compile)))

(with-eval-after-load 'cc-mode
  (require 'lsp-java)
  (define-key java-mode-map (kbd "C-c C-m") 'java-compile-run-current-main-class))

(add-hook 'kotlin-mode-hook (lambda () (lsp)))

(add-hook 'java-mode-hook 'lsp)
(add-hook 'java-mode-hook
          (lambda ()
            (setq (make-local-variable lsp-ui-sideline-show-code-actions) nil)))
(with-eval-after-load 'lsp-java
  (setq lsp-java-autobuild-enabled nil)
  (setq lsp-java-code-generation-generate-comments t)
  (setq lsp-java-completion-overwrite nil)
  (setq lsp-java-format-on-type-enabled nil)
  (setq lsp-java-save-action-organize-imports nil))

(provide 'init-java)

;;; init-java.el ends here
