;;; init-treesitter.el --- Configurations for emacs-tree-sitter	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for emacs-tree-sitter.
;; --------------------------------------

;;; Code:

(with-eval-after-load 'tree-sitter
  (require 'tree-sitter-langs)
  (add-to-list 'tree-sitter-major-mode-language-alist '(jsts-mode . tsx))
  (setq tree-sitter-debug-highlight-jump-region t)
  (setq tree-sitter-debug-jump-buttons t)
  ;; (defun tree-sitter-rust-imenu-index-function ()
  ;;   (thread-last (tsc-make-query tree-sitter-language [(function_item (identifier) @function)])
  ;;                (seq-map (lambda (capture)
  ;;                           (pcase-let ((`(_ . ,node) capture))
  ;;                             (cons (tsc-node-text node)
  ;;                                   (tsc-node-start-position node)))))))
  ;; (setq imenu-create-index-function #'tree-sitter-rust-imenu-index-function)
  )

(add-hook 'c-mode-common-hook 'tree-sitter-mode)
(add-hook 'python-mode-hook 'tree-sitter-mode)
(add-hook 'java-mode-hook 'tree-sitter-mode)
(add-hook 'js-mode-hook 'tree-sitter-mode)
(add-hook 'typescript-mode-hook 'tree-sitter-mode)
(add-hook 'css-mode-hook 'tree-sitter-mode)
(add-hook 'mhtml-mode-hook 'tree-sitter-mode)

(add-hook 'tree-sitter-after-on-hook
          (lambda ()
            (setq font-lock-defaults '(nil))
            (tree-sitter-hl-mode)))
(add-hook 'yas-after-exit-snippet-hook
          (lambda ()
            (when (bound-and-true-p tree-sitter-mode)
              (setq tree-sitter-tree nil)
              (tree-sitter--do-parse))))


(provide 'init-treesitter)

;;; init-treesitter.el ends here
