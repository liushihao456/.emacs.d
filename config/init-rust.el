;;; init-rust.el --- Configurations for Rust	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for Rust
;; --------------------------------------

;;; Code:

(use-package rust-mode
  :ensure t
  )

(use-package eglot
  :ensure t
  :hook (rust-mode . eglot-ensure))

(provide 'init-rust)

;;; init-rust.el ends here
