;;; init-custom.el --- Custom file	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Custom file where customization are saved.
;; ------------------------------------------

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Variables customized by Custom                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-disable-insert-state-bindings t)
 '(evil-kill-on-visual-paste nil)
 '(evil-symbol-word-search t)
 '(evil-want-C-u-scroll t)
 '(package-selected-packages
   '(prettier-js wgrep-deadgrep ibuffer-project lua-mode org-tidy evil-commentary
     evil-surround evil-textobj-tree-sitter lsp-treemacs evil-anzu evil origami
     rust-mode ccls company-flx flx highlight-quoted rainbow-delimiters
     orderless vertico diff-hl consult-lsp org-bullets org-roam all-the-icons
     editorconfig doom-themes base16-theme dashboard embark embark-consult
     consult marginalia company-box spacemacs-theme lsp-pyright solarized-theme
     benchmark-init tree-sitter tree-sitter-langs anzu lsp-mode lsp-ui
     swift-mode kotlin-mode cdlatex deadgrep wgrep json-mode emmet-mode
     expand-region gnuplot-mode lsp-java delight auctex magit company which-key
     flycheck zenburn-theme yasnippet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Faces customized by Custom                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init-custom)
;;; init-custom.el ends here
