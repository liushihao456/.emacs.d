;;; init-treesit.el --- Configurations for treesit	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for treesit
;; --------------------------------------

;;; Code:

(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'init-treesit)

;;; init-treesit.el ends here
