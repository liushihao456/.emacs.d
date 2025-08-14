;;; init-compile.el --- Configurations for compilation	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for compiling in Emacs.
;; --------------------------------------

;;; Code:

(defun my/compile-project ()
  "Compile the project."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (call-interactively 'compile)))
(global-set-key (kbd "C-c m") 'my/compile-project)

(use-package compile
  :config
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
  (add-hook 'compilation-finish-functions 'switch-to-buffer-other-window))

(provide 'init-compile)

;;; init-compile.el ends here
