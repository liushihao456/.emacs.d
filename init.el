;;; package --- Summary
;;; Commentary:
;;   init.el --- Emacs configuration
;;   This is where Emacs is inited
;; --------------------------------------

;;; Code:
(setq gc-cons-threshold 100000000)
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-check-signature nil)

(add-to-list 'load-path (file-name-concat user-emacs-directory "config"))
(setq custom-file (file-name-concat user-emacs-directory "config" "init-custom.el"))

;; (define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
;;   (let ((obsolete-name (pop ll))
;;         (current-name (pop ll))
;;         (when (if ll (pop ll) "1"))
;;         (docstring (if ll (pop ll) nil)))
;;     (list obsolete-name current-name when docstring)))
;; (require 'benchmark-init)
;; ;; (benchmark-init/activate)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

(require 'init-custom)
(require 'init-custom-packages)
(require 'init-selectrum)
(require 'init-misc)
(require 'init-theme)
(require 'init-meow)
(require 'init-compile)
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
