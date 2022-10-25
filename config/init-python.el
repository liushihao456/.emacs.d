;;; init-python.el --- Configurations for Python	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for Python
;; --------------------------------------

;;; Code:

(add-hook 'python-mode-hook 'lsp)
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local imenu-create-index-function #'python-imenu-create-flat-index)))
(with-eval-after-load 'python
  (require 'lsp-pyright)
  (setq python-shell-interpreter "python3")
  (defun my/format-buffer ()
    "Format buffer using yapf."
    (interactive)
    (let ((old-point (point)))
      (erase-buffer)
      (insert (shell-command-to-string (concat "yapf " (buffer-name))))
      (goto-char old-point)))
  (define-key python-mode-map (kbd "C-c C-l") nil)
  (define-key python-mode-map (kbd "<f5>") 'my/format-buffer))

(with-eval-after-load 'lsp-python-ms
  (setq lsp-python-ms-cache "Library"))

(with-eval-after-load 'lsp-pyright
  ;; (setq lsp-pyright-use-library-code-for-types nil)
  )

(provide 'init-python)

;;; init-python.el ends here
