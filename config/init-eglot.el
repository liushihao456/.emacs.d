;;; init-eglot.el --- Configurations of eglot	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for Language Server Protocal support.
;; --------------------------------------

;;; Code:

(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :ensure t
  :config
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l x") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l m") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c l e") 'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c l t") 'eglot-find-typeDefinition))

(provide 'init-eglot)

;;; init-eglot.el ends here
