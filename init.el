;;; package --- Summary
;;; Commentary:
;;   init.el --- Emacs configuration
;;   This is where Emacs is inited
;; --------------------------------------

;;; Code:
(setq gc-cons-threshold 100000000)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;                          ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'load-path (concat user-emacs-directory "config"))

;; (define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
;;   (let ((obsolete-name (pop ll))
;;         (current-name (pop ll))
;;         (when (if ll (pop ll) "1"))
;;         (docstring (if ll (pop ll) nil)))
;;     (list obsolete-name current-name when docstring)))
;; (require 'benchmark-init)
;; ;; (benchmark-init/activate)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

;; BASIC CUSTOMIZATION
;; --------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Variables customized by Custom                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(meow telega all-the-icons editorconfig doom-themes base16-theme dashboard embark embark-consult consult doom-modeline marginalia company-box spacemacs-theme lsp-pyright solarized-theme benchmark-init tree-sitter tree-sitter-langs anzu lsp-mode typescript-mode lsp-ui swift-mode kotlin-mode cdlatex writeroom-mode company-prescient deadgrep wgrep selectrum selectrum-prescient json-mode emmet-mode expand-region gnuplot-mode lsp-java delight auctex magit company yasnippet-snippets which-key flycheck zenburn-theme yasnippet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Faces customized by Custom                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background "#272A36")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Basic customizations                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-custom-packages)
(require 'init-misc)
(require 'init-theme)
(require 'init-meow)
(require 'init-compile)
(require 'init-selectrum)
(require 'init-wgrep)
(require 'init-modeline)
(require 'init-flycheck)
(require 'init-yasnippet)
(require 'init-company)
(require 'init-latex)
(require 'init-gnuplot)
(require 'init-org)
(require 'init-lsp)
(require 'init-python)
(require 'init-cpp)
(require 'init-java)
(require 'init-web)
(require 'init-treesitter)

;; (setq gc-cons-threshold (* 800 1000))

(provide 'init)
;;; init.el ends here
