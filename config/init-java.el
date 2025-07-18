;;; init-java.el --- Configurations for Java	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for Java
;; --------------------------------------

;;; Code:

(use-package cc-mode
  :defer t
  :config
  (require 'lsp-java)

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

  (define-key java-mode-map (kbd "C-c C-m") 'java-compile-run-current-main-class))

(use-package lsp-mode
  :ensure t
  :hook (java-mode . lsp))

(use-package kotlin-mode
  :ensure t
  :defer t
  :config
  (add-hook 'kotlin-mode-hook (lambda () (lsp))))

(use-package lsp-java
  :ensure t
  :defer t
  :config
  (setq lsp-java-autobuild-enabled nil)
  (setq lsp-java-code-generation-generate-comments t)
  (setq lsp-java-completion-overwrite nil)
  (setq lsp-java-format-on-type-enabled nil)
  (setq lsp-java-save-action-organize-imports nil))

(provide 'init-java)

;;; init-java.el ends here
