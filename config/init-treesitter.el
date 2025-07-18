;;; init-treesitter.el --- Configurations for emacs-tree-sitter	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for emacs-tree-sitter.
;; --------------------------------------

;;; Code:

(use-package tree-sitter-langs
  :ensure t
  :defer t)

(use-package tree-sitter
  :ensure t
  :hook ((c-mode-common python-mode java-mode js-mode typescript-mode css-mode mhtml-mode) . tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  (add-to-list 'tree-sitter-major-mode-language-alist '(jsts-mode . tsx))
  (setq tree-sitter-debug-highlight-jump-region t)
  (setq tree-sitter-debug-jump-buttons t)

  (add-hook 'tree-sitter-after-on-hook
            (lambda ()
              (setq font-lock-defaults '(nil))
              (tree-sitter-hl-mode)))
  (add-hook 'yas-after-exit-snippet-hook
            (lambda ()
              (when (bound-and-true-p tree-sitter-mode)
                (setq tree-sitter-tree nil)
                (tree-sitter--do-parse)))))

(provide 'init-treesitter)

;;; init-treesitter.el ends here
