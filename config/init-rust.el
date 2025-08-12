;;; init-rust.el --- Configurations for Rust	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for Rust
;; --------------------------------------

;;; Code:

(use-package eglot
  :ensure t
  :hook (rust-ts-mode . eglot-ensure))

(provide 'init-rust)

;;; init-rust.el ends here
