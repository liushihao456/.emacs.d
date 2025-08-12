;;; init-java.el --- Configurations for Java	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for Java
;; --------------------------------------

;;; Code:

(use-package cc-mode
  :config
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

  (define-key java-mode-map (kbd "C-c C-m") 'java-compile-run-current-main-class)
  (use-package java-ts-mode
    :config
    (define-key java-ts-mode-map (kbd "C-c C-m") 'java-compile-run-current-main-class)))

(use-package eglot
  :ensure t
  :hook ((java-mode java-ts-mode) . eglot-ensure)
  :config
  ;; Download jdtls from http://download.eclipse.org/jdtls/milestones/
  ;; Then put it in .emacs.d/.cache/jdtls
  (add-to-list 'exec-path (file-name-concat user-emacs-directory ".cache/jdtls/bin")))

(provide 'init-java)

;;; init-java.el ends here
