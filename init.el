;;; package --- Summary

;;; Commentary:
;;
;;   init.el --- Emacs configuration
;;   This is where Emacs is inited
;; --------------------------------------

;;; Code:

;; Define a function to find init file at first, in case there's any error
;; during initialization
(defun find-init-file ()
  "Find the `user-init-file'."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c f i") 'find-init-file)

(setq gc-cons-threshold 100000000)

(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))

(add-to-list 'load-path (file-name-concat user-emacs-directory "config"))

;; (define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
;;   (let ((obsolete-name (pop ll))
;;         (current-name (pop ll))
;;         (when (if ll (pop ll) "1"))
;;         (docstring (if ll (pop ll) nil)))
;;     (list obsolete-name current-name when docstring)))
;; (use-package benchmark-init
;;   :ensure t)
;; (benchmark-init/activate)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

;; (setq use-package-compute-statistics t)

(setq use-package-always-defer t)

(require 'init-local-config)
(require 'init-minibuffer)
(require 'init-font)
(require 'init-theme)
(require 'init-modeline)
(require 'init-evil)
(require 'init-misc)
(require 'init-compile)
(require 'init-flycheck)
(require 'init-company)
(require 'init-imenu)
(require 'init-latex)
(require 'init-gnuplot)
(require 'init-org)
(require 'init-elisp)
(require 'init-python)
(require 'init-cpp)
(require 'init-java)
(require 'init-eglot)
(require 'init-js)
(require 'init-rust)
(require 'init-treesit)
(require 'init-fold)
(require 'init-dired)
(require 'init-vc)

;; (setq gc-cons-threshold (* 800 1000))

(provide 'init)
;;; init.el ends here
