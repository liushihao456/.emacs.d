;;; Commentary:
;;init.el --- Emacs configuration

;; INSTALL PACKAGES;;; init -- init Emacs
;;
;; Commentary:
;;   This is where Emacs is inited
;; --------------------------------------

;;; Code:
(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

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
 '(TeX-engine (quote xetex))
 '(TeX-view-program-selection
   (quote
    ((output-dvi "open")
     (output-pdf "Preview.app")
     (output-html "open"))))
 '(aggressive-indent-dont-electric-modes t)
 '(aggressive-indent-excluded-modes
   (quote
    (inf-ruby-mode makefile-mode makefile-gmake-mode python-mode text-mode yaml-mode java-mode)))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(aw-keys (quote (97 115 100 102 103 104 106 107 108)))
 '(column-number-mode t)
 '(company-backends
   (quote
    ((company-files company-keywords company-capf company-yasnippet)
     (company-abbrev company-dabbrev))))
 '(company-idle-delay 0)
 '(dumb-jump-force-searcher (quote ag))
 '(dumb-jump-selector (quote helm))
 '(global-aggressive-indent-mode t)
 '(global-company-mode t)
 '(global-hl-line-mode t)
 '(global-hungry-delete-mode t)
 '(global-linum-mode t)
 '(global-undo-tree-mode t)
 '(helm-autoresize-mode t)
 '(helm-mode t)
 '(helm-mode-fuzzy-match t)
 '(pdf-view-use-unicode-ligther nil)
 '(preview-TeX-style-dir "/Users/liushihao/.emacs.d/elpa/auctex-12.1.1/latex" t)
 '(projectile-enable-caching t)
 '(projectile-mode t nil (projectile))
 '(python-shell-interpreter "python3")
 '(scroll-bar-mode nil)
 '(show-smartparens-global-mode t)
 '(smartparens-global-mode t)
 '(truncate-lines t)
 '(which-key-mode t)
 '(which-key-popup-type (quote side-window))
 '(which-key-side-window-location (quote right))
 '(yas-global-mode t)
 '(yas-snippet-dirs
   (quote
    ("/Users/liushihao/.emacs.d/snippets" yasnippet-snippets-dir))))

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
(set-frame-font "Monaco-13") ; set font to Monaco
(load-theme 'zerodark t)
;; (load-theme 'zenburn t)
(powerline-center-theme) ; mode line theme
(add-hook 'after-init-hook 'toggle-frame-fullscreen) ; start emacs in fullscreen
(dashboard-setup-startup-hook) ; show spacemacs-style interface when startup
(setq dashboard-banner-logo-title "Emacs and the changing world.") ; change the title
(setq dashboard-items '((recents  . 10)
                        (projects . 10)
                        ))
(setq-default bidi-display-reordering nil) ; improve long-line performance
(global-set-key (kbd "M-o") 'ace-window) ; quickly switch between windows

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Smartparens                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra hydra-smartparens (:hint nil)
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
(define-key smartparens-mode-map (kbd "C-c s") 'hydra-smartparens/body)
(define-key smartparens-mode-map (kbd "C-k") 'sp-kill-hybrid-sexp)
(define-key smartparens-mode-map (kbd "M-k") 'sp-backward-kill-sexp)
(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-<right>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-<left>") 'sp-backward-barf-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Projectile                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(helm-projectile-on)

(define-key projectile-mode-map (kbd "C-c C-p") nil)
(defhydra hydra-helm-projectile (:color blue
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
(define-key projectile-mode-map (kbd "C-c p") 'hydra-helm-projectile/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Helm                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-r") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

(defhydra hydra-helm (:color blue :columns 3)
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
(global-set-key (kbd "C-x c") 'hydra-helm/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Dumb jump                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defhydra hydra-dumb-jump (:color blue :columns 3)
  "
                               Dumb Jump
^^^^^^^^----------------------------------------------------------------------"
  ("j" dumb-jump-go "Go")
  ("o" dumb-jump-go-other-window "Other window")
  ("e" dumb-jump-go-prefer-external "Go external")
  ("b" dumb-jump-back "Back")
  ("i" dumb-jump-go-prompt "Prompt")
  ("l" dumb-jump-quick-look "Quick look")
  ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
(global-set-key (kbd "M-g") 'hydra-dumb-jump/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Flycheck                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This sets $MANPATH, $PATH and exec-path from your shell
;; in order for flycheck to take effect
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; Enable flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Python mode                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'ein:notebook-multilang-mode-hook (lambda () (company-mode -1))) ;disable company mode in ein mode
;; (setq ein:completion-backend 'ein:use-ac-jedi-backend) ;enable jedi in ein

;; Use flake8 for flycheck in pyton mode
(add-hook 'python-mode-hook (lambda ()
                              (setq-local flycheck-python-flake8-executable "/usr/local/bin/flake8")))

;; enable company-jedi
(add-hook 'python-mode-hook (lambda ()
			      (make-local-variable 'company-backends)
			      (setq company-backends (copy-tree company-backends))
			      (setf (car company-backends)
			      	    (append '(company-jedi) (car company-backends)))
			      ))
(add-hook 'python-mode-hook 'jedi:setup)

;; Aggressive indent doesn't work for python mode, therefore
;; electric indent mode for python mode will be automatically enabled.

;; Set yas-indent-line to fixed for python mode
(add-hook 'python-mode-hook (lambda ()
			      (setq-local yas-indent-line 'fixed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               C and C++ mode                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up c-mode and cpp-mode company backends
(defun my/irony-mode ()
  "Add company-irony to company-backends in C and C++ mode."
  (make-local-variable 'company-backends)
  (setq company-backends (copy-tree company-backends))
  (setf (car company-backends)
	(append '(company-irony) (car company-backends)))
  )
(add-hook 'c-mode-hook 'my/irony-mode)
(add-hook 'c++-mode-hook 'my/irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Java mode                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/lsp-java-setup ()
  "Lsp java setup."
  (require 'lsp-java)
  (require 'lsp-ui-flycheck)
  (require 'lsp-ui-sideline)
  (require 'lsp-mode)
  (lsp-java-enable)
  (lsp-ui-sideline-mode)
  (setq lsp-java--workspace-folders (list "~/javaproj"
					  "~/Android"))
  (setq company-lsp-enable-snippet t
	company-lsp-cache-candidates t)
  (setq lsp-ui-sideline-enable t
	lsp-ui-sideline-show-symbol t
	lsp-ui-sideline-show-hover t
	lsp-ui-sideline-show-code-actions t
	lsp-ui-sideline-update-mode 'point)
  (make-local-variable 'company-backends)
  (push 'company-lsp company-backends)
  (lsp-ui-flycheck-enable t)
  (add-hook 'lsp-mode-hook (lambda ()
			     (setq lsp-inhibit-message t
				   lsp-eldoc-render-all nil
				   lsp-highlight-symbol-at-point nil)
			     ))
  )
(add-hook 'java-mode-hook  'my/lsp-java-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Latex mode                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-off-smartparens-mode) ; turn off smartparens since cdlatex mode has this feature
(add-hook 'LaTeX-mode-hook 'cdlatex-mode)

;; Turn off company mode in latex-mode
(add-hook 'LaTeX-mode-hook (lambda ()
			     (company-mode -1)
                             (setq TeX-show-compilation t)
			     (setq reftex-plug-into-AUCTeX t)
			     (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
                             
                             (setq bibtex-completion-bibliography ; set up helm-bibtex
				   '("~/RA/mybib.bib"
				     "~/RA/newadded.bib"))
			     (require 'helm-bibtex)
			     (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
			     (helm-add-action-to-source "Insert Citation" 'helm-bibtex-insert-citation helm-source-bibtex 0) ; Set the default action to insert citation
                             
			     (auctex-latexmk-setup)
			     (setq TeX-command-default "LatexMk") ; Set the default command to LatexMk
			     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Expand region                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-=") 'er/expand-region) ; set the expand region key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Multiple cursors                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c '") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c ;") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Narrowing and Widening                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you're windened, narrow to the region, if you're narrowed, widen.
;; Bound to C-c n

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
	((derived-mode-p 'org-mode)
	 (cond ((ignore-errors (org-edit-src-code))
		(delete-other-windows))
	       ((org-at-block-p)
		(org-narrow-to-block))
	       (t (org-narrow-to-subtree))))
	(t (narrow-to-defun))))
;; (define-key global-map "C-c n" #'narrow-or-widen-dwim)
(global-set-key (kbd "C-c n") 'narrow-or-widen-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Auto yasnippets                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c c") 'aya-create)
(global-set-key (kbd "C-c e") 'aya-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Magit                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'init)

;;; init.el ends here
