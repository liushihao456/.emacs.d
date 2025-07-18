;;; init-js.el --- Configurations for js/ts development	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for js/ts development
;; --------------------------------------

;;; Code:

(use-package prettier-js
  :ensure t
  :defer t)

(use-package jsts-mode
  :load-path "packages/jsts-mode"
  :mode ("\\.jsx\\'" "\\.js\\'" "\\.tsx\\'" "\\.ts\\'")
  :config
  (defun prettier-buffer ()
    "Organize imports and call prettier to format buffer."
    (interactive)
    (when (fboundp 'lsp-organize-imports) (lsp-organize-imports))
    (when (fboundp 'prettier-js) (prettier-js)))
  (define-key jsts-mode-map (kbd "<f5>") 'prettier-buffer))

(use-package lsp-mode
  :ensure t
  :hook (jsts-mode . lsp))

(use-package emmet-mode
  :ensure t
  :hook ((jsts-mode mhtml-mode) . emmet-mode))

(use-package yasnippet
  :ensure t
  :hook (jsts-mode . (lambda () (when yas-minor-mode
                                  (yas-activate-extra-mode 'js-mode)
                                  (yas-activate-extra-mode 'typescript-mode)))))

(use-package sgml-mode
  :defer t
  :config
  (setq sgml-basic-offset 4))

(provide 'init-js)

;;; init-js.el ends here
