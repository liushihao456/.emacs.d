;;; init-python.el --- Configurations for Python	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for Python
;; --------------------------------------

;;; Code:

(use-package eglot
  :ensure t
  :hook (python-base-mode . eglot-ensure))

(use-package python
  :config
  (setq python-shell-interpreter "python3")
  (defun my/format-buffer ()
    "Format buffer using yapf."
    (interactive)
    (let ((old-point (point)))
      (erase-buffer)
      (insert (shell-command-to-string (concat "yapf " (buffer-name))))
      (goto-char old-point)))
  (define-key python-base-mode-map (kbd "C-c C-l") nil)
  (define-key python-base-mode-map (kbd "<f5>") 'my/format-buffer))

(provide 'init-python)

;;; init-python.el ends here
