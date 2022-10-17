;;; init-yasnippet.el --- Configurations for yasnippet	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for yasnippet
;; --------------------------------------

;;; Code:

(with-eval-after-load 'yasnippet
  (defun yas-try-key-from-dot (_start-point)
    "As `yas-key-syntaxes' element, look for dot key.
It enables expanding `foo.' to `foo->'."
    (skip-chars-backward "\."))
  (add-to-list 'yas-key-syntaxes 'yas-try-key-from-dot)

  (setq yas-triggers-in-field t)
  (yas-reload-all))
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)
(add-hook 'org-mode-hook 'yas-minor-mode)

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
