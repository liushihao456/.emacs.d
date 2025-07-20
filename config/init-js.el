;;; init-js.el --- Configurations for js/ts development	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for js/ts development
;; --------------------------------------

;;; Code:

(use-package prettier-js
  :ensure t)

(use-package js
  :config
  (defun prettier-buffer ()
    "Organize imports and call prettier to format buffer."
    (interactive)
    (when (fboundp 'lsp-organize-imports) (lsp-organize-imports))
    (when (fboundp 'prettier-js) (prettier-js)))
  (add-hook 'js-base-mode-hook (lambda () (when yas-minor-mode
                                            (yas-activate-extra-mode 'js-mode)
                                            (yas-activate-extra-mode 'typescript-mode))))
  (define-key js-base-mode-map (kbd "<f5>") 'prettier-buffer))

(use-package typescript-ts-mode
  :config
  (defun prettier-buffer ()
    "Organize imports and call prettier to format buffer."
    (interactive)
    (when (fboundp 'lsp-organize-imports) (lsp-organize-imports))
    (when (fboundp 'prettier-js) (prettier-js)))
  (add-hook 'typescript-ts-base-mode-hook (lambda () (when yas-minor-mode
                                            (yas-activate-extra-mode 'js-mode)
                                            (yas-activate-extra-mode 'typescript-mode))))
  (define-key typescript-ts-base-mode-map (kbd "<f5>") 'prettier-buffer))

(use-package lsp-mode
  :ensure t
  :hook ((js-base-mode typescript-ts-base-mode) . lsp))

(use-package emmet-mode
  :ensure t
  :hook ((js-base-mode typescript-ts-base-mode mhtml-mode) . emmet-mode))

(use-package sgml-mode
  :config
  (setq sgml-basic-offset 4))

(provide 'init-js)

;;; init-js.el ends here
