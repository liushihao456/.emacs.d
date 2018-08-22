;;; Commentary:
;;init.el --- Emacs configuration

;; INSTALL PACKAGES;;; init -- init Emacs
;;
;; Commentary:
;;   This is where Emacs is inited
;; --------------------------------------

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/packages/use-package")
  (add-to-list 'load-path "~/.emacs.d/packages/bind-key")
  (require 'use-package))

(setq use-package-compute-statistics t)

;; (use-package auto-package-update
;;   :ensure t
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

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
 '(column-number-mode t)
 '(global-hl-line-mode t)
 '(package-selected-packages
   (quote
    (helm-ag dashboard matlab-mode auctex-latexmk cdlatex helm-bibtex auctex company-lsp lsp-java lsp-ui lsp-mode company-irony irony ein py-autopep8 jedi treemacs-projectile treemacs dumb-jump helm-swoop helm-projectile projectile smartparens hydra aggressive-indent auto-yasnippet multiple-cursors expand-region hungry-delete undo-tree company yasnippet-snippets yasnippet ace-window which-key powerline zerodark-theme auto-package-update)))
 '(python-shell-interpreter "python3")
 '(scroll-bar-mode nil)
 '(truncate-lines t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Faces customized by Custom                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Basic customizations                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t) ; hide the startup message
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)
(set-frame-font "Monaco-12") ; set font to Monaco

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn-theme t)
;;   )

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-one t)
;;   )

(use-package zerodark-theme
  :ensure t
  :config
  (load-theme 'zerodark t)
  )

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme)
  )

(add-hook 'after-init-hook 'toggle-frame-fullscreen) ; start emacs in fullscreen

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Emacs and the changing world.") ; change the title
  (setq dashboard-items '((recents  . 10)
                          (projects . 10)))
  (message "Dashboard loaded.")
  )

(setq-default bidi-display-reordering nil) ; improve long-line performance

;; Show key bindings on the right
(use-package which-key
  :ensure t
  :hook ((prog-mode . which-key-mode)
	 (text-mode . which-key-mode)
	 (ein:notebook-multilang-mode . which-key-mode))
  :config
  (which-key-setup-side-window-right)
  )

;; Quickly switch between windows
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(97 115 100 102 103 104 106 107 108))
  )

(use-package yasnippet
  :ensure t
  :ensure yasnippet-snippets
  ;; :custom
  ;; (yas/root-directory "~/.emacs.d/snippets")
  :hook ((prog-mode . yas-minor-mode)
	 (LaTeX-mode . yas-minor-mode)
	 (ein:notebook-multilang-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode))
  :config
  (setq yas/root-directory "~/.emacs.d/snippets")
  (yas-load-directory yas/root-directory)
  (add-hook 'python-mode-hook (lambda ()
				(setq-local yas-indent-line 'fixed)))
  )

(use-package company
  :ensure t
  :hook ((c-mode c++-mode matlab-mode emacs-lisp-mode ein:notebook-multilang-mode) . company-mode)
  :custom
  (company-idle-delay 0)
  (company-backends '((company-files company-keywords company-capf company-yasnippet)
		      (company-abbrev company-dabbrev)))
  :config
  (setq company-idle-delay 0)
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-selection-wrap-around t)
  )

(use-package flycheck
  :ensure t
  :hook ((prog-mode . flycheck-mode)
	 (ein:notebook-multilang-mode . flycheck-mode))
  :config
  (add-hook 'python-mode-hook (lambda ()
				(setq-local flycheck-python-flake8-executable "/usr/local/bin/flake8")))
  )

;; This sets $MANPATH, $PATH and exec-path from your shell
;; in order for flycheck to take effect
;; (use-package exec-path-from-shell
;;   :if (memq window-system '(mac ns))
;;   :ensure t
;;   :config
;;   (exec-path-from-shell-initialize))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package hungry-delete
  :ensure t
  :hook ((emacs-lisp-mode . hungry-delete-mode)
	 (c-mode . hungry-delete-mode)
	 (c++-mode . hungry-delete-mode)
	 (python-mode . hungry-delete-mode)
	 (matlab-mode . hungry-delete-mode)
	 (ein:notebook-multilang-mode . hungry-delete-mode))
  )

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  )

(use-package multiple-cursors
  :ensure t
  :bind (("C-c '" . mc/edit-lines)
	 ("C-c ;" . mc/mark-all-like-this)
	 ("C-," . mc/mark-previous-like-this)
	 ("C-." . mc/mark-next-like-this))
  )

(defun narrow-or-widen-dwim (p)
  
  "If the buffer is narrowed, it widens.  Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.
 
  With prefix P, don't widen, just narrow even if buffer is already
narrowed."

  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
	((region-active-p)
	 (narrow-to-region (region-beginning) (region-end)))
	(t (narrow-to-defun))))
(global-set-key (kbd "C-c n") 'narrow-or-widen-dwim)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (message "Magit loaded.")
  )

(use-package auto-yasnippet
  :ensure t
  :bind (("C-c c" . aya-create)
	 ("C-c e" . aya-expand))
  :config
  (message "Auto-yasnippet loaded.")
  )

(use-package aggressive-indent
  :ensure t
  :custom
  (aggressive-indent-excluded-modes '(inf-ruby-mode makefile-mode makefile-gmake-mode python-mode
						    text-mode yaml-mode java-mode))
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
	 (c-mode . aggressive-indent-mode)
	 (c++-mode . aggressive-indent-mode)
	 (org-mode . aggressive-indent-mode))
  :config
  (setq-local electric-indent-mode nil)
  (defun turn-off-electric-indent-mode ()
    "Turn off electric indent mode in current buffer."
    (setq-local electric-indent-mode nil))
  (add-hook 'emacs-lisp-mode 'turn-off-electric-indent-mode)
  (add-hook 'c-mode 'turn-off-electric-indent-mode)
  (add-hook 'c++-mode 'turn-off-electric-indent-mode)
  (add-hook 'LaTeX-mode 'turn-off-electric-indent-mode)
  )

(use-package use-package-hydra
  :load-path "~/.emacs.d/packages/use-package-hydra"
  :ensure hydra)

(use-package smartparens
  :ensure t
  :hook (((prog-mode ein:notebook-multilang-mode) . show-smartparens-mode)
	 ((prog-mode ein:notebook-multilang-mode) . turn-on-smartparens-mode))
  :bind (:map smartparens-mode-map
	      ("C-c s" . hydra-smartparens/body)
	      ("C-k" . sp-kill-hybrid-sexp)
	      ("M-k" . sp-backward-kill-sexp)
	      ("C-<right>" . sp-forward-slurp-sexp)
	      ("M-<right>" . sp-forward-barf-sexp)
	      ("C-<left>" . sp-backward-slurp-sexp)
	      ("M-<left>" . sp-backward-barf-sexp))
  :hydra (hydra-smartparens (:hint nil)
			    "
                               Smartparens
^^^^^^^^------------------------------------------------------------------------------
_d_: down                         _a_: Down                     _f_: forward
_e_: up                           _u_: Up                       _b_: backward
_k_: kill                         _q_: quit
"
			    ("d" sp-down-sexp)
			    ("e" sp-up-sexp)
			    ("u" sp-backward-up-sexp)
			    ("a" sp-backward-down-sexp)
			    ("f" sp-forward-sexp)
			    ("b" sp-backward-sexp)
			    ("k" sp-kill-sexp)
			    ("q" nil :color blue))
  )

(use-package projectile
  :ensure t
  :custom
  (projectile-enable-caching t)
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") nil)
  (projectile-mode t)
  )

(use-package helm-projectile
  :ensure t
  :bind (:map projectile-mode-map
	      ("C-c p" . hydra-helm-projectile/body))
  :config
  (helm-projectile-on)
  :hydra (hydra-helm-projectile (:color blue
					:hint nil)
				"
^Find^                            ^List^                          ^Grep^
^^^^^^^^-------------------------------------------------------------------------------
_f_: files                        _e_: recent files               _s s_: ag
_a_: other files                  _p_: proj                       _s g_: grep
_F_: files all prj                _b_: buff curr proj
_g_: files at point               _h_: helm interface
_d_: dir"
				("f" helm-projectile-find-file)
				("a" helm-projectile-find-other-file)
				("F" helm-projectile-find-file-in-known-projects)
				("g" helm-projectile-find-file-dwim)
				("d" helm-projectile-find-dir)

				("e" helm-projectile-recentf)
				("p" helm-projectile-switch-project)
				("b" helm-projectile-switch-to-buffer)
				("h" helm-projectile)

				("s s" helm-projectile-ag)
				("s g" helm-projectile-grep))
  )

(use-package helm-swoop
  :ensure t
  :commands (helm-swoop helm-swoop-back-to-last-point))

(use-package helm
  :ensure t
  :custom
  (helm-autoresize-mode t)
  (helm-mode-fuzzy-match t)
  :bind (("C-x c" . hydra-helm/body)
	 ("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x C-f" . helm-find-files)
	 ("C-s" . helm-swoop)
	 ("C-r" . helm-swoop-back-to-last-point)
	 :map helm-map
	 ([tab] . helm-execute-persistent-action))
  :hydra (hydra-helm (:color blue :columns 3)
		     "
                       C-x c Helm Commands
^^^^^^^^------------------------------------------------------------------"
		     ("/" Helm-find "find")
		     ("b" helm-resume "resume")
		     ("c" helm-colors "colors")

		     ("i" helm-semantic-or-imenu "imenu")
		     ("a" helm-apropos "apropos")
		     ("8" helm-ucs "ucs")

		     ("I" helm-imenu-in-all-buffers "imenu all buffers")
		     ("m" helm-man-woman "man")
		     ("p" helm-list-emacs-process "list emacs process")

		     ("s" helm-do-grep-ag "ag")
		     ("t" helm-bibtex "bibtex"))
  :config
  (helm-mode t)
  ;; (require 'helm-config)
  (global-set-key (kbd "C-x c") 'hydra-helm/body)
  )

(use-package dumb-jump
  :ensure t
  :init
  (defun dumb-jump-go/helm (p)
    "Load helm before calling treemancs-projectile."
    (interactive "P")
    (unless (featurep 'helm)
      (require 'helm))
    (dumb-jump-go)
    )
  :custom
  (dumb-jump-force-searcher 'ag)
  (dumb-jump-selector 'helm)
  :bind ("M-g" . hydra-dumb-jump/body)
  :hydra (hydra-dumb-jump (:color blue :columns 3)
			  "
                               Dumb Jump
^^^^^^^^----------------------------------------------------------------------"
			  ("j" dumb-jump-go/helm "Go")
			  ("o" dumb-jump-go-other-window "Other window")
			  ("e" dumb-jump-go-prefer-external "Go external")
			  ("b" dumb-jump-back "Back")
			  ("i" dumb-jump-go-prompt "Prompt")
			  ("l" dumb-jump-quick-look "Quick look")
			  ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
  )

(use-package treemacs
  :ensure t
  :init
  (defun treemacs-projectile/helm (p)
    "Load helm before calling treemancs-projectile."
    (interactive "P")
    (unless (featurep 'helm)
      (require 'helm))
    (treemacs-projectile)
    )
  :custom
  (treemacs-follow-after-init t)
  (treemacs-width 25)
  (treemacs-indentation 2)
  (treemacs-git-integration t)
  (treemacs-collapse-dirs 3)
  (treemacs-silent-refresh nil)
  (treemacs-change-root-without-asking nil)
  (treemacs-sorting 'alphabetic-desc)
  (treemacs-show-hidden-files t)
  (treemacs-never-persist nil)
  (treemacs-is-never-other-window nil)
  (treemacs-goto-tag-strategy 'refetch-index)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  :bind (("C-x a" . hydra-treemacs/body)
	 ("C-c a" . treemacs-helpful-hydra))
  :hydra (hydra-treemacs (:color blue :hint nil)
			 "
^Toggle^                            ^Windows^                          ^Find^
^ ^
^^^^^^^^-------------------------------------------------------------------------------
^ ^
_a_: Treemacs                       _s_: Select treemacs window        _f_: Find files
^ ^
_p_: Treemacs projectile            _d_: Delete other windows          _b_: Find bookmark
^ ^"
                         ("a" treemacs)
			 ("s" treemacs-select-window)
			 ("f" treemacs-find-file)
			 ("p" treemacs-projectile/helm)
			 ("d" treemacs-delete-other-windows)
			 ("b" treemacs-bookmark))
  
  )

(use-package treemacs-projectile
  :ensure t
  :commands treemacs-projectile
  :custom
  (treemacs-header-function #'treemacs-projectile-create-header)
  )

;; Python
;; (use-package anaconda-mode
;;   :ensure t
;;   :ensure company-anaconda
;;   :hook ((python-mode . anaconda-mode)
;; 	 (python-mode . anaconda-eldoc-mode))
;;   :config
;;   (make-local-variable 'company-backends)
;;   (setq company-backends nil)
;;   (add-to-list 'company-backends '(company-anaconda company-files))
;;   ;; make sure the company backends are set again when opening another python file.
;;   (defun my/anaconda-setup ()
;;     "Add company-anaconda to company-backends in python mode."
;;     (make-local-variable 'company-backends)
;;     (setq company-backends nil)
;;     (add-to-list 'company-backends '(company-anaconda company-files))
;;     )
;;   (add-hook 'python-mode-hook 'my/anaconda-setup)
;;   )

(use-package jedi
  :ensure t
  :custom (jedi:complete-on-dot t)
  :hook (python-mode . jedi:ac-setup)
  )

;; (use-package company-jedi
;;   :ensure t
;;   :init
;;   (defun my/python-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'my/python-mode-hook)
;;   )

;; (use-package jedi-core
;;   :ensure t
;;   :ensure company-jedi
;;   :hook (python-mode . jedi:setup)
;;   :config
;;   (message "Jedi loaded.")
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:use-shortcuts t)

;;   (make-local-variable 'company-backends)
;;   (setq company-backends nil)
;;   (add-to-list 'company-backends 'company-jedi)

;;   ;; make sure the company backends are set again when opening another python file.
;;   (defun my/jedi-setup ()
;;     "Add company-jedi to company-backends in python mode."
;;     (make-local-variable 'company-backends)
;;     (setq company-backends nil)
;;     (add-to-list 'company-backends '(company-jedi company-yasnippet company-files))
;;     )
;;   (add-hook 'python-mode-hook 'my/jedi-setup)
;;   )

(use-package py-autopep8
  :ensure t
  :hook (python-mode . py-autopep8-enable-on-save))

;; Emacs Ipython Notebook
(use-package ein
  :ensure t
  :commands ein:jupyter-server-start
  :custom (ein:completion-backend 'ein:use-company-backend)
  )

;; C++
(use-package irony
  :ensure t
  :ensure company-irony
  :hook ((c-mode . irony-mode)
	 (c++-mode . irony-mode)
	 (irony-mode . irony-cdb-autosetup-compile-options))
  :config
  (message "Irony loaded.")
  (make-local-variable 'company-backends)
  (setq company-backends nil)
  (add-to-list 'company-backends '(company-irony company-yasnippet company-files))

  (defun my/irony-mode ()
    "Add company-irony to company-backends in C and C++ mode."
    (make-local-variable 'company-backends)
    (setq company-backends nil)
    (add-to-list 'company-backends '(company-irony company-yasnippet company-files))
    )
  (add-hook 'c-mode-hook 'my/irony-mode)
  (add-hook 'c++-mode-hook 'my/irony-mode)

  )

;; (use-package rtags
;;   :ensure t
;;   :ensure helm-rtags
;;   :hook (((c-mode c++-mode) . rtags-start-process-unless-running)
;; 	 ((c-mode c++-mode) . rtags-diagnostics))
;;   :custom
;;   (rtags-completions-enabled t)
;;   (rtags-display-result-backend 'helm)
;;   )

;; Java
(use-package lsp-mode
  :ensure t
  :hook java-mode
  :config
  (setq lsp-eldoc-render-all nil
	lsp-highlight-symbol-at-point nil)
  )

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-sideline-mode lsp-ui-flycheck-enable)
  :config
  (setq lsp-ui-sideline-enable t
	lsp-ui-sideline-show-symbol t
	lsp-ui-sideline-show-hover t
	lsp-ui-sideline-show-code-actions t
	lsp-ui-sideline-update-mode 'point)
  )

(use-package lsp-java
  :ensure t
  :ensure company-lsp
  :hook (java-mode . lsp-java-enable)
  :config
  (lsp-ui-sideline-mode)
  (setq lsp-java--workspace-folders (list "~/javaproj"))
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-cache-candidates t)
  (lsp-ui-flycheck-enable t)
  (setq lsp-ui-sideline-enable t
	lsp-ui-sideline-show-symbol t
	lsp-ui-sideline-show-hover t
	lsp-ui-sideline-show-code-actions t
	lsp-ui-sideline-update-mode 'point)
  (make-local-variable 'company-backends)
  (setq company-backends nil)
  (add-to-list 'company-backends '(company-lsp company-yasnippet company-files))
  
  (add-hook 'java-mode-hook  (lambda ()
			       (lsp-ui-sideline-mode)
			       (lsp-ui-flycheck-enable t)
			       (make-local-variable 'company-backends)
			       (setq company-backends nil)
			       (add-to-list 'company-backends '(company-lsp company-yasnippet company-files))
  			       ))
  )

;; LaTeX
(use-package tex
  :defer t
  :ensure auctex
  :ensure helm-bibtex
  :config
  (setq TeX-auto-save t)
  (reftex-mode t)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (setq TeX-engine 'xetex)))
  (setq TeX-view-program-selection
	'((output-dvi "open")
	  (output-pdf "Preview.app")
	  (output-html "open")))
  (setq TeX-show-compilation t)
  (setq reftex-plug-into-AUCTeX t)
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
  (setq bibtex-completion-bibliography ; set up helm-bibtex
	'("~/RA/mybib.bib"
	  "~/RA/newadded.bib"))
  (require 'helm-bibtex)
  (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
  (helm-add-action-to-source "Insert Citation" 'helm-bibtex-insert-citation helm-source-bibtex 0) ; Set the default action to insert citation
  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  )

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . cdlatex-mode))

(use-package auctex-latexmk
  :ensure t
  :hook (LaTeX-mode . auctex-latexmk-setup)
  :custom (TeX-command-default "LatexMk")
  )

;; Matlab mode
(use-package matlab-mode
  :ensure t
  :commands matlab-shell
  )

(provide 'init)

;;; init.el ends here
