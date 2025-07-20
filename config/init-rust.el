;;; init-rust.el --- Configurations for Rust	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for Rust
;; --------------------------------------

;;; Code:

(use-package lsp-mode
  :ensure t
  :hook (rust-ts-mode . lsp))

(provide 'init-rust)

;;; init-rust.el ends here
