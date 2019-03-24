;;; Commentary:
;; init.el --- Emacs configuration

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
;; (package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/packages/use-package")
  (add-to-list 'load-path "~/.emacs.d/packages/bind-key")
  (require 'use-package))

;; (setq use-package-compute-statistics t)

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
 '(TeX-command-default "LatexMk" t)
 '(aggressive-indent-excluded-modes
   '(inf-ruby-mode makefile-mode makefile-gmake-mode python-mode text-mode yaml-mode java-mode) t)
 '(column-number-mode t)
 '(company-backends
   '((company-files company-keywords company-capf company-yasnippet)
     (company-abbrev company-dabbrev)) t)
 '(company-idle-delay 0 t)
 '(custom-safe-themes
   '("08ef1356470a9d3bf363ffab0705d90f8a492796e9db489936de4bde6a4fdb19" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" default))
 '(global-hl-line-mode t)
 '(helm-autoresize-mode t t)
 '(helm-mode-fuzzy-match t t)
 '(org-agenda-files '("~/notes/notes.org"))
 '(package-selected-packages
   '(gnuplot flycheck magit zenburn-theme org-ref company-jedi web-mode ob-ipython ess htmlize org-latex helm-ag dashboard matlab-mode auctex-latexmk cdlatex helm-bibtex auctex company-lsp lsp-java lsp-ui lsp-mode company-irony irony py-autopep8 treemacs-projectile treemacs dumb-jump helm-swoop helm-projectile projectile smartparens hydra aggressive-indent multiple-cursors expand-region hungry-delete undo-tree company yasnippet-snippets yasnippet ace-window which-key powerline zerodark-theme auto-package-update))
 '(projectile-enable-caching t)
 '(python-shell-interpreter "python3")
 '(scroll-bar-mode nil)
 '(treemacs-change-root-without-asking nil t)
 '(treemacs-collapse-dirs 3 t)
 '(treemacs-follow-after-init t t)
 '(treemacs-git-integration t t)
 '(treemacs-goto-tag-strategy 'refetch-index t)
 '(treemacs-header-function 'treemacs-projectile-create-header t)
 '(treemacs-indentation 2 t)
 '(treemacs-is-never-other-window nil t)
 '(treemacs-never-persist nil t)
 '(treemacs-show-hidden-files t t)
 '(treemacs-silent-refresh nil t)
 '(treemacs-sorting 'alphabetic-desc t)
 '(treemacs-width 25 t)
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
(setq
 mac-command-modifier 'meta
 mac-option-modifier 'none
 )
;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; Kill currnet line and copy current line
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

;; ;; English Font
;; (set-face-attribute
;;  'default nil :font "Monaco 12")
;; ;; Chinese Font
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;; 		    charset
;; 		    ;; (font-spec :family "Microsoft Yahei" :size 14)))
;; 		    (font-spec :family "WenQuanYi Micro Hei Mono" :size 14)))

(defun s-font()
  (interactive)
  ;; font config for org table showing.
  (set-frame-font "Consolas-13")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "WenQuanYi Micro Hei Mono" :size 14))))

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if window-system
                   (s-font))))
(if window-system
    (s-font))

;; (set-frame-font "monaco-12") ; set font to Monaco

;;; Scrolling.
;; Good speed and allow scrolling through large images (pixel-scroll).
;; Note: Scroll lags when point must be moved but increasing the number
;;       of lines that point moves in pixel-scroll.el ruins large image
;;       scrolling. So unfortunately I think we'll just have to live with
;;       this.
(pixel-scroll-mode)
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t)
;;   )

(use-package solarized-theme
  :ensure t
  :config
  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)
  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)
  ;; ;; Avoid all font-size changes
  ;; (setq solarized-height-minus-1 1.0)
  ;; (setq solarized-height-plus-1 1.0)
  ;; (setq solarized-height-plus-2 1.0)
  ;; (setq solarized-height-plus-3 1.0)
  ;; (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-light t)
  )
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; (load-theme 'doom-one t)
;;   (load-theme 'doom-solarized-light t)
;;   )

;; (use-package zerodark-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zerodark t)
;;   )

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-center-theme)
;;   (setq powerline-image-apple-rgb t)
;;   )

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
  :hook ((prog-mode text-mode) . which-key-mode)
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
  :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  :config
  (setq yas/root-directory "~/.emacs.d/snippets")
  (yas-load-directory yas/root-directory)
  (add-hook 'python-mode-hook (lambda ()
				(setq-local yas-indent-line 'fixed)))
  )

(use-package company
  :ensure t
  :hook ((c-mode c++-mode matlab-mode emacs-lisp-mode org-mode eshell-mode python-mode message-mode inferior-ess-mode R-mode) . company-mode)
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
  :hook (prog-mode . flycheck-mode)
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
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package hungry-delete
  :ensure t
  :hook ((emacs-lisp-mode . hungry-delete-mode)
	 (c-mode . hungry-delete-mode)
	 (c++-mode . hungry-delete-mode)
	 (python-mode . hungry-delete-mode)
	 (matlab-mode . hungry-delete-mode))
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

(use-package aggressive-indent
  :ensure t
  :custom
  (aggressive-indent-excluded-modes '(inf-ruby-mode makefile-mode makefile-gmake-mode python-mode
						    text-mode yaml-mode java-mode))
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
	 (c-mode . aggressive-indent-mode)
	 (c++-mode . aggressive-indent-mode))
  :config
  (electric-indent-local-mode -1)
  (defun turn-off-electric-indent-mode ()
    "Turn off electric indent mode in current buffer."
    (electric-indent-local-mode -1))
  (add-hook 'emacs-lisp-mode 'turn-off-electric-indent-mode)
  (add-hook 'c-mode 'turn-off-electric-indent-mode)
  (add-hook 'c++-mode 'turn-off-electric-indent-mode)
  )

(use-package use-package-hydra
  :load-path "~/.emacs.d/packages/use-package-hydra"
  :ensure hydra)

(use-package smartparens
  :ensure t
  :hook ((prog-mode . show-smartparens-mode)
	 (prog-mode . turn-on-smartparens-mode))
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

^Find^                            ^List^                          ^Grep^                          ^Compile^

^^^^^^^^-----------------------------------------------------------------------------------------------------------------

_f_: files                        _e_: recent files               _s s_: ag                       _c_: compile peoject

_a_: other files                  _p_: proj                       _s g_: grep

_F_: files all prj                _b_: buff curr proj

_g_: files at point               _h_: helm interface

_d_: dir
"
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
				("s g" helm-projectile-grep)

				("c" projectile-compile-project))
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
  (global-set-key (kbd "C-x c") 'hydra-helm/body)
  )

;; (use-package dumb-jump
;;   :ensure t
;;   :pin manual
;;   :init
;;   (defun dumb-jump-go/helm (p)
;;     "Load helm before calling 'dumb-jump-go', P."
;;     (interactive "P")
;;     (unless (featurep 'helm)
;;       (require 'helm))
;;     (dumb-jump-go)
;;     )
;;   :custom
;;   (dumb-jump-force-searcher 'ag)
;;   (dumb-jump-selector 'helm)
;;   :bind ("M-g" . hydra-dumb-jump/body)
;;   :hydra (hydra-dumb-jump (:color blue :columns 3)
;; 			  "
;;                                Dumb Jump
;; ^^^^^^^^----------------------------------------------------------------------"
;; 			  ("j" dumb-jump-go/helm "Go")
;; 			  ("o" dumb-jump-go-other-window "Other window")
;; 			  ("e" dumb-jump-go-prefer-external "Go external")
;; 			  ("b" dumb-jump-back "Back")
;; 			  ("i" dumb-jump-go-prompt "Prompt")
;; 			  ("l" dumb-jump-quick-look "Quick look")
;; 			  ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
;;   )

(use-package treemacs
  :ensure t
  :init
  (defun treemacs-projectile/helm (p)
    "Load helm before calling 'treemancs-projectile', P."
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

;; (use-package jedi
;;   :ensure t
;;   :custom (jedi:complete-on-dot t)
;;   :hook (python-mode . jedi:ac-setup)
;;   )

(use-package company-jedi
  :ensure t
  :defer t
  :init
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends '(company-jedi company-files)))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

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
  :init
  ;; (add-hook 'LaTeX-mode-hook 'flyspell-mode) ; Enable spell check in latex mode
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode) ; Enable auto-fill in latex mode
  (add-hook 'LaTeX-mode-hook 'reftex-mode)    ; Enable reftex mode in latex mode
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (setq TeX-engine 'xetex)
			       (setq TeX-command-extra-options "-shell-escape")))
  (reftex-mode t)
  ;; (setq TeX-source-correlate-mode t)

  ;; (setq TeX-view-program-selection
  ;; 	'((output-dvi "open")
  ;; 	  (output-pdf "open")
  ;; 	  (output-html "open")))
  
  (setq TeX-show-compilation t)
  (setq reftex-plug-into-AUCTeX t)
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
  (setq bibtex-completion-bibliography ; set up helm-bibtex
	'("~/RA/mybib.bib"
	  "~/RA/newadded.bib"
	  "~/paper/pub/thesis.bib"))
  (require 'helm-bibtex)
  (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
  (helm-add-action-to-source "Insert Citation" 'helm-bibtex-insert-citation helm-source-bibtex 0) ; Set the default action to insert citation
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

;; Gnuplot mode
(use-package gnuplot
  :ensure t
  :mode ("\\.gnuplot\\' \\.gp\\'" . gnuplot-mode)
  )

;; Org mode
(setq-default fill-column 80)
(use-package org
  :ensure t
  :ensure ob-ipython
  :ensure org-ref
  :mode ("\\.org\\'" . org-mode)
  :init
  (add-hook 'org-mode-hook 'auto-fill-mode)
  ;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  ;; (setq org-startup-with-inline-images t) ; Display inline images by default
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'add-pcomplete-to-capf) ; Enable org mode completion
  (add-hook 'org-mode-hook (lambda ()
			     (unless (featurep 'ox-md)
			       (require 'ox-md)) ; Enable exporting to markdown
			     (unless (featurep 'ox-beamer)
			       (require 'ox-beamer)) ; Enable exporting to beamers via LaTeX
                             (setq-local company-minimum-prefix-length 1)
			     (unless (featurep 'cdlatex)
			       (require 'cdlatex))
			     (unless (featurep 'ess-site)
			       (require 'ess-site))
			     ;; (org-defkey org-mode-map "`" 'cdlatex-math-symbol)
			     ;; (org-defkey org-mode-map (kbd "C-;") 'cdlatex-math-modify)
			     (org-defkey org-mode-map "\C-c{" 'org-cdlatex-environment-indent)

			     (setq org-image-actual-width '(400)) ; Prevent inline images being too big
			     (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images) ; Redisplay after babel executing
			     (setq org-export-coding-system 'utf-8)	       ; Ensure exporting with UTF-8
			     (add-to-list 'org-latex-packages-alist '("" "listings")) ; Use listings package to export code blocks
			     (add-to-list 'org-latex-packages-alist '("" "xcolor"))
			     (add-to-list 'org-latex-packages-alist '("" "xeCJK"))
			     (setq org-latex-listings 'listings)
			     ;; (add-to-list 'org-latex-packages-alist '("" "minted")) ; Use minted package to export code blocks
			     ;; (setq org-latex-listings 'minted)
			     (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
\\ctexset{section/format=\\Large\\bfseries}"
							       ("\\section{%s}" . "\\section*{%s}")
							       ("\\subsection{%s}" . "\\subsection*{%s}")
							       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
							       ("\\paragraph{%s}" . "\\paragraph*{%s}")
							       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
			     (add-to-list 'org-latex-classes '("ctexrep" "\\documentclass[11pt]{ctexrep}
\\ctexset{section/format=\\Large\\bfseries}"
							       ("\\part{%s}" . "\\part*{%s}")
							       ("\\chapter{%s}" . "\\chapter*{%s}")
							       ("\\section{%s}" . "\\section*{%s}")
							       ("\\subsection{%s}" . "\\subsection*{%s}")
							       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
			     (add-to-list 'org-latex-classes '("ctexbook" "\\documentclass[11pt]{ctexbook}"
							       ("\\part{%s}" . "\\part*{%s}")
							       ("\\chapter{%s}" . "\\chapter*{%s}")
							       ("\\section{%s}" . "\\section*{%s}")
							       ("\\subsection{%s}" . "\\subsection*{%s}")
							       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
			     (setq org-latex-compiler "xelatex")
			     (setq org-latex-pdf-process
				   '(;; "latexmk -pdflatex=xelatex -pdf -shell-escape %f"
				     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
				     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
				     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
				     ))
			     (setq org-latex-caption-above '(table)) ; Set the caption in exported pdf above
			     (org-babel-do-load-languages
			      'org-babel-load-languages
			      '((python . t)
				(ipython . t)
				(emacs-lisp . t)
				(C . t)
				(js . t)
				(ditaa . t)
				(dot . t)
				(org . t)
				(shell . t)
				(latex . t)
				(R . t)
				(gnuplot . t)
				))				      ; Babel stuff
			     (add-to-list 'org-latex-listings-langs '(ipython "Python"))
			     (setq org-src-fontify-natively t)
			     (setq org-preview-latex-default-process 'dvipng)
			     (setq org-src-window-setup 'current-window)
			     
			     ;; (setq org-confirm-babel-evaluate nil) ; Don’t ask before evaluating code blocks.
			     (setq org-export-use-babel nil) ; Stop Org from evaluating code blocks to speed exports
			     (setq org-babel-python-command "python3") ; Set the command to python3 instead of python

			     ;; (unless (featurep 'org-ref)
			     ;;   (require 'org-ref))
			     ;; (setq reftex-default-bibliography '("~/RA/mybib.bib"))
			     ;; (setq org-ref-default-bibliography '("~/RA/mybib.bib"))
			     ))
  (add-hook 'org-mode-hook (lambda ()
			     (electric-pair-local-mode t))) ; Enable emacs-native electric pair mode in org mode

  (defun org-capture/helm (p)
    "Load helm before calling 'org-capture', P."
    (interactive "P")
    (unless (featurep 'helm)
      (require 'helm))
    (org-capture))
  (define-key global-map (kbd "C-c 2") 'org-capture/helm) ; Org-capture

  (setq org-capture-templates
	'(("t" "Todo list item"
	   entry (file+headline "~/notes/tasks.org" "Tasks")
	   "* TODO %?\n %i\n %a")

	  ("j" "Journal entry"
	   entry (file+olp+datetree "~/notes/journal.org" "Journals")
	   "* %U %^{Title}\n %?")

	  ("b" "Tidbit: quote, zinger, one-liner or textlet"
	   entry (file+headline "~/notes/tidbits.org" "Tidbits")
	   "* %^{Name} captured %U\n %^{Tidbit type|quote|zinger|one-liner|textlet}\n Possible inspiration: %a %i\n %?")

	  ("n" "Notes"
	   entry (file "~/notes/notes.org" )
	   "* %?")
	  ))
  :config
  (setq org-startup-indented t)		; Indent the tree structure
  )

(use-package htmlize
  :ensure t
  :commands htmlize-buffer)		; Enable org exporting to html
(setq org-html-postamble nil)		; Don't include a footer with my contact and publishing information at the bottom of every exported HTML document

;; Eshell
(define-key global-map (kbd "C-c 1") 'eshell) ; Eshell mode
(add-hook 'shell-mode-hook (lambda ()
			     (setq-local company-minimum-prefix-length 1)
			     ))
(setq password-cache t)			; enable password caching
(setq password-cache-expiry 3600)	; Enable password caching for one hour

;; ESS and R
(use-package ess
  :ensure t
  :commands R
  )


(use-package web-mode
  :ensure t
  :mode "\\.html?\\'"
  :config
  (setq web-mode-engines-alist
	'(("django"    . "\\.html\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  )

;; mu4e
(use-package mu4e
  :load-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu4e"
  :bind ("M-m" . mu4e)
  :config
  (setq mail-user-agent 'mu4e-user-agent)	; Use mu4e as default mail agent
  (setq mu4e-maildir "~/mail")		; Mail folder set to ~/mail
  (setq mu4e-get-mail-command "offlineimap") ; Fetch mail by offlineimap
  (setq mu4e-update-interval 300)		; Fetch mail in 300 sec interval

  (setq mu4e-sent-folder   "/pkumailbox/Sent Items")
  (setq mu4e-drafts-folder "/pkumailbox/Drafts")
  (setq mu4e-trash-folder  "/pkumailbox/Trash")
  (setq mu4e-refile-folder  "/pkumailbox/Archive")
  (setq mu4e-attachment-dir  "~/Downloads")
  
  (setq mu4e-view-show-images t)
  (setq mu4e-compose-signature-auto-include nil)

  ;; Send emails
  (setq message-send-mail-function 'smtpmail-send-it
	smtpmail-stream-type 'ssl
	starttls-use-gnutls t)
  ;; Personal info
  (setq user-full-name "Shihao, Liu")
  (setq user-mail-address "liushihao@pku.edu.cn")
  ;; Smtpmail setup
  (setq smtpmail-smtp-server "mail.pku.edu.cn")
  (setq smtpmail-default-smtp-server "mail.pku.edu.cn")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-smtp-user "liushihao@pku.edu.cn")
  )


(provide 'init)

;;; init.el ends here
