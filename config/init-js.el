;;; init-js.el --- Configurations for js/ts development	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for js/ts development
;; --------------------------------------

;;; Code:

(with-eval-after-load 'jsts-mode
  (defun prettier-buffer ()
    "Organize imports and call prettier to format buffer."
    (interactive)
    (when (fboundp 'lsp-organize-imports) (lsp-organize-imports))
    (when (fboundp 'prettier-js) (prettier-js)))
  (define-key jsts-mode-map (kbd "<f5>") 'prettier-buffer))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . jsts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . jsts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . jsts-mode))

(add-hook 'jsts-mode-hook 'lsp)
(add-hook 'jsts-mode-hook 'emmet-mode)
(add-hook 'jsts-mode-hook (lambda () (when yas-minor-mode
                                  (yas-activate-extra-mode 'js-mode)
                                  (yas-activate-extra-mode 'typescript-mode))))

(add-hook 'mhtml-mode-hook 'emmet-mode)
(with-eval-after-load 'sgml-mode
  (setq sgml-basic-offset 4))

(provide 'init-js)

;;; init-js.el ends here
