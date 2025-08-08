;;; init-theme.el --- Configurations for theme	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for theme.
;; --------------------------------------

;;; Code:

(use-package zenburn-theme
  :ensure t)

(use-package spacemacs-theme
  :ensure t)

(use-package base16-theme
  :ensure t
  :config
  (setq base16-theme-256-color-source 'colors))

(use-package solarized-theme
  :ensure t
  :config
  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)
  ;; Use less bolding
  (setq solarized-use-less-bold t)
  ;; Use more italics
  (setq solarized-use-more-italic t)
  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  (setq solarized-emphasize-indicators nil)
  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil))

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-region '(bg-only))
  (setq modus-themes-org-blocks 'gray-background))

(load-theme local-config-theme t)

;; (setq frame-background-mode 'dark)

(when (display-graphic-p)
  (set-face-background 'fringe "unspecified"))

(provide 'init-theme)

;;; init-theme.el ends here
