;;; init-latex.el --- Configurations for LaTeX	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for LaTeX
;; --------------------------------------

;;; Code:

(with-eval-after-load 'latex
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-complete-expert-commands t)
  (setq-default TeX-command-extra-options "-shell-escape")
  (setq-default TeX-engine 'xetex)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  ;; (add-hook 'LaTeX-mode-hook 'cdlatex-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq font-latex-fontify-sectioning 'color))

(with-eval-after-load 'font-latex
  (set-face-attribute 'font-latex-slide-title-face
                      nil
                      :height 'unspecified
                      :weight 'unspecified
                      :inherit 'font-lock-type-face))


(provide 'init-latex)

;;; init-latex.el ends here
