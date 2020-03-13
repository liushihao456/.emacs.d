;;; package --- Summary
;;; Commentary:
;;   init.el --- Emacs configuration
;;   This is where Emacs is inited
;; --------------------------------------

;;; Code:
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
                         ))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(eval-when-compile
  (add-to-list 'load-path "~/.config/emacs/packages/use-package")
  (add-to-list 'load-path "~/.config/emacs/packages/bind-key")
  (require 'use-package))
;; (setq use-package-compute-statistics t)

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package use-package-hydra
  :load-path "~/.config/emacs/packages/use-package-hydra"
  :ensure hydra)

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
 '(blink-cursor-mode nil)
 '(cmake-tab-width 4 t)
 '(column-number-mode t)
 '(custom-safe-themes
   '("a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "e39ff005e524c331b08d613109bff0b55fc21c64914c4a243faa70f330015389" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "08ef1356470a9d3bf363ffab0705d90f8a492796e9db489936de4bde6a4fdb19" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" default))
 '(dired-use-ls-dired nil)
 '(electric-pair-mode t)
 '(evil-disable-insert-state-bindings t t)
 '(evil-insert-state-cursor nil t)
 '(evil-operator-state-cursor nil t)
 '(evil-replace-state-cursor nil t)
 '(indent-tabs-mode nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(ivy parchment-theme ripgrep company-tabnine lsp-mode company-box lsp-java lsp-ui all-the-icons ag lsp-python-ms delight solarized-theme general evil-surround evil shell-pop spacemacs-theme dap-mode ob-ipython hydra projectile web-mode ess bibtex auctex magit multiple-cursors company yasnippet-snippets which-key flycheck doom-themes ccls zenburn-theme htmlize dashboard cdlatex yasnippet))
 '(projectile-completion-system 'ivy)
 '(projectile-enable-caching t)
 '(python-shell-interpreter "python3")
 '(scroll-bar-mode nil)
 '(shell-pop-full-span t t)
 '(shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell))) t)
 '(show-paren-mode t)
 '(split-width-threshold 150)
 '(tool-bar-mode nil)
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

(setq inhibit-startup-message t)        ; hide the startup message
(fset 'yes-or-no-p 'y-or-n-p)           ; change all prompts to y or n
(setq backup-directory-alist `(("." . "~/.config/emacs/backups")))
(setq ring-bell-function 'ignore)
(setq-default fill-column 80)
(setq comment-style 'indent)


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


(if (display-graphic-p)
    (progn
      (setq initial-frame-alist (quote ((fullscreen . maximized))))
      (setq
       mac-command-modifier 'meta
       mac-option-modifier 'none
       )

      (setq face-font-rescale-alist `(("STkaiti" . ,(/ 16.0 13))))
      (set-face-attribute 'default nil :font "Source Code Pro-13")
      (set-fontset-font t 'han      (font-spec :family "STkaiti"))
      (set-fontset-font t 'cjk-misc (font-spec :family "STkaiti"))
      )
)

(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (emacs-lisp-mode "Elisp" :major)
             (undo-tree-mode nil "Undo-Tree")))
  )

(use-package evil
  :ensure t
  :defer 1
  :custom
  (evil-disable-insert-state-bindings t)
  (evil-insert-state-cursor nil)
  (evil-replace-state-cursor nil)
  (evil-operator-state-cursor nil)
  :config
  (evil-set-initial-state 'dashboard-mode 'motion)
  (evil-set-initial-state 'use-package-statistics-mode 'motion)
  (evil-set-initial-state 'rst-mode 'motion)
  (evil-set-initial-state 'message-mode 'motion)
  (evil-set-initial-state 'flycheck-error-list-mode 'motion)
  (evil-set-initial-state 'fundamental-mode 'motion)
  (evil-set-initial-state 'TeX-output-mode 'motion)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'lsp-ui-imenu-mode 'emacs)
  (evil-mode 1)
  (use-package evil-surround
    :ensure t
    )
  (define-key evil-motion-state-map " " nil)
  (define-key evil-normal-state-map " " nil)
  (define-key evil-visual-state-map " " nil)
  (evil-define-key '(normal motion) 'global
    (kbd "SPC w") 'evil-window-map)
  (evil-define-key 'normal 'global
    (kbd "SPC TAB") 'evil-motion-state)
  (evil-define-key 'motion 'global
    (kbd "SPC TAB") 'evil-normal-state)
  (global-evil-surround-mode 1)
  )

(defun my/open-external-terminal ()
  "Open an external Terminal window under current directory."
  (interactive)
  (shell-command "open -a Terminal .")
  )

(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual)
   "TAB" 'indent-for-tab-command
   )
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "TAB" 'org-cycle)
  (general-define-key
   :states 'motion
   :keymaps 'help-mode-map
   "TAB" 'forward-button
   )
  (general-define-key
   :states '(normal motion)
   "=" 'end-of-defun
   "-" 'beginning-of-defun
   "_" 'mark-defun
   "C-u" 'evil-scroll-up
   )
  (general-define-key
   :states '(normal motion)
   :prefix "SPC"
   "q" 'save-buffers-kill-terminal
   ";" 'comment-line
   "s" 'save-buffer
   "e" 'eval-last-sexp
   "SPC" 'execute-extended-command
   "o t" 'my/open-external-terminal
   "d f" 'describe-function
   "d v" 'describe-variable
   "d k" 'describe-key
   "d m" 'describe-mode
   "d p" 'describe-package
   "d b" 'describe-bindings
   "d l" 'view-lossage
   "d r" 'info-emacs-manual
   "d i" 'info
   )
  (general-define-key
   :states '(normal motion)
   :prefix "SPC"
   "k" 'kill-buffer
   "D" 'dired
   "b" 'switch-to-buffer
   "B" 'ibuffer
   "P" 'list-processes
   )
  (general-define-key
   :states 'visual
   :prefix "SPC r"
   "s" 'copy-to-register
   "r" 'copy-rectangle-to-register
   )
  (general-define-key
   :states '(normal motion)
   :prefix "SPC r"
   "w" 'window-configuration-to-register
   "f" 'frameset-to-register
   "SPC" 'point-to-register
   "j" 'jump-to-register

   "i" 'insert-register

   "m" 'bookmark-set
   "M" 'bookmark-set-no-overwrite
   "b" 'bookmark-jump
   "l" 'list-bookmarks
   )
  )

(defun find-init-file ()
  "Find init.el file."
  (interactive)
  (find-file "~/.config/emacs/init.el")
  )

(general-define-key
 :states '(normal motion)
 :prefix "SPC f"
 "i" 'find-init-file
 "f" 'find-file
 "r" 'recentf-open-files
 )

(defun toggle-window-split ()
  "Toggle window split.  Works only when there are exactly two windows open.
If the windows are vertically split, turn them into positinos horizontally
split; vice versa."
  (interactive)
  (unless (= (count-windows) 2) (error "Can only toggle a frame split in two"))
  (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-is-2nd (not (and (<= (car this-win-edges)
                                        (car next-win-edges))
                                    (<= (cadr this-win-edges)
                                        (cadr next-win-edges)))))
         (splitter (if (= (car this-win-edges)
                          (car next-win-edges))
                       'split-window-horizontally
                     'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-is-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-is-2nd (other-window 1)))))
(general-define-key
 :states '(normal motion)
 :prefix "SPC w"
 "|" 'toggle-window-split)

(use-package ivy
  :ensure t
  :defer 1
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

(use-package projectile
  :ensure t
  :ensure ripgrep
  :defer 1.5
  :delight '(:eval (concat " Proj:" (projectile-project-name)))
  :config
  (projectile-mode t)
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  :general
  (:states '(normal motion)
           :prefix "SPC p"
           "f" 'projectile-find-file
           "a" 'projectile-find-other-file
           "F" 'projectile-find-file-in-known-projects
           "g" 'projectile-find-file-dwim
           "d" 'projectile-find-dir
           "D" 'projectile-dired
           "e" 'projectile-recentf
           "p" 'projectile-switch-project
           "b" 'projectile-switch-to-buffer
           "s s" 'projectile-ag
           "s r" 'projectile-ripgrep
           "s g" 'projectile-grep
           "c" 'projectile-compile-project
           "i" 'projectile-invalidate-cache
           "r" 'projectile-replace
           "R" 'projectile-replace-regexp
           )
  )

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  )

;; (use-package parchment-theme
;;   :ensure t
;;   :config (load-theme 'parchment t)
;;   )

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; (load-theme 'doom-one t)
;;   ;; (load-theme 'doom-one-light t)
;;   ;; (load-theme 'doom-solarized-light t)
;;   )

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-dark t)
;;   (setq solarized-use-variable-pitch nil)
;;   (setq solarized-distinct-fringe-background t)
;;   (setq solarized-high-contrast-mode-line t)
;;   (setq solarized-use-less-bold t)
;;   (setq solarized-use-more-italic t)
;;   (setq solarized-emphasize-indicators nil)
;;   (setq solarized-scale-org-headlines nil)
;;   )

;; (use-package spacemacs-theme
;;   :ensure t
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t)
;;   (setq spacemacs-theme-underline-parens t)
;;   )

(use-package flycheck
  :ensure t
  :general
  (:states '(normal motion)
           :prefix "SPC f"
           "p" 'flycheck-previous-error
           "n" 'flycheck-next-error
           "l" 'flycheck-list-errors
           "c" 'flycheck-buffer
           "v" 'flycheck-verify-setup
           "s" 'flycheck-select-checker
           "d" 'flycheck-disable-checker
           )
  :hook ((prog-mode . flycheck-mode)
         (ess-r-mode . (lambda () (flycheck-mode -1))))
  )

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)
                          ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil)
  )

;; Show key bindings below
(use-package which-key
  :ensure t
  :delight
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  )

(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
  :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  :config
  (setq yas/root-directory "~/.config/emacs/snippets/")
  (yas-load-directory yas/root-directory)
  )

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :pin manual
)

(use-package company
  :ensure t
  :hook ((prog-mode org-mode eshell-mode shell-mode inferior-python-mode inferior-ess-mode) . company-mode)
  :general
  (:keymaps 'company-active-map
            "C-n" 'company-select-next
            "C-p" 'company-select-previous
           )
  :config
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0)
  )

(use-package company-box
  :ensure t
  :delight
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-show-single-candidate t
        company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons)
  )

(use-package multiple-cursors
  :ensure t
  :general
  (:states '(insert emacs)
           "C-c m" 'hydra-multiple-cursors/body
           )
  :hydra (hydra-multiple-cursors (:hint nil)
                                 "

          Previous^^                  Next^^                 Miscellaneous         % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")

         ---------------------------------------------------------------------------------------------------

          [_p_]   Next                [_n_]   Next           [_l_] Edit lines        [_0_] Insert numbers

          [_P_]   Skip                [_N_]   Skip           [_a_] Mark all          [_A_] Insert letters

          [_M-p_] Unmark              [_M-n_] Unmark         [_s_] Search

          [Click] Cursor at point   [_q_]   Quit
         "
                                 ("l" mc/edit-lines :exit t)
                                 ("a" mc/mark-all-like-this :exit t)
                                 ("n" mc/mark-next-like-this)
                                 ("N" mc/skip-to-next-like-this)
                                 ("M-n" mc/unmark-next-like-this)
                                 ("p" mc/mark-previous-like-this)
                                 ("P" mc/skip-to-previous-like-this)
                                 ("M-p" mc/unmark-previous-like-this)
                                 ("s" mc/mark-all-in-region-regexp :exit t)
                                 ("0" mc/insert-numbers :exit t)
                                 ("A" mc/insert-letters :exit t)
                                 ("<mouse-1>" mc/add-cursor-on-click)
                                 ;; Help with click recognition in this hydra
                                 ("<down-mouse-1>" ignore)
                                 ("<drag-mouse-1>" ignore)
                                 ("q" nil))
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
  :general
  (:states '(normal motion)
           :prefix "SPC m"
           "g" 'magit-status)
  )

;; LaTeX
(use-package tex
  :defer t
  :ensure auctex
  :general
  (:states '(normal motion)
           :keymaps 'LaTeX-mode-map
           :prefix "SPC c"
           "s" 'LaTeX-section
           "e" 'LaTeX-environment
           "j" 'LaTeX-insert-item
           "m s" 'LaTeX-mark-section
           "m e" 'LaTeX-mark-environment
           "]" 'LaTeX-close-environment
           "." 'reftex-view-crossref
           "(" 'reftex-label
           ")" 'reftex-reference
           "/" 'reftex-index-selection-or-word
           "<" 'reftex-index
           "=" 'reftex-toc
           ">" 'reftex-display-index
           "[" 'reftex-citation
           "\\" 'reftex-index-phrase-selection-or-word
           "|" 'reftex-index-visit-phrases-buffer
           "?" 'cdlatex-command-help
           "{" 'cdlatex-environment
           "RET" 'TeX-insert-macro
           "TAB" 'TeX-goto-info-page
           "^" 'TeX-home-buffer
           "_" 'TeX-master-file-ask
           "`" 'TeX-next-error
           "a" 'TeX-command-run-all
           "c" 'TeX-command-master
           "b" 'TeX-command-buffer
           "d" 'TeX-save-document
           "f" 'TeX-font
           "k" 'TeX-kill-job
           "l" 'TeX-recenter-output-buffer
           "n" 'TeX-normal-mode
           "r" 'TeX-command-region
           "v" 'TeX-view
           "w" 'TeX-toggle-debug-bad-boxes
           "z" 'LaTeX-command-section
           "Z" 'LaTeX-command-run-all-section
           )
  (:states '(normal motion)
           :keymaps 'LaTeX-mode-map
           :prefix "SPC c q"
           "e" 'LaTeX-fill-environment
           "s" 'LaTeX-fill-section
           "r" 'LaTeX-fill-region
           "p" 'LaTeX-fill-paragraph
           )
  (:states '(normal motion)
           :keymaps 'LaTeX-mode-map
           :prefix "SPC c o"
           "f" 'TeX-fold-mode
           "RET" 'TeX-fold-macro
           "e" 'TeX-fold-env
           "b" 'TeX-fold-buffer
           "c" 'TeX-fold-comment
           "o" 'TeX-fold-dwim
           "p" 'TeX-fold-paragraph
           "r" 'TeX-fold-region
           "B" 'TeX-fold-clearout-buffer
           "i" 'TeX-fold-clearout-item
           "P" 'TeX-fold-clearout-paragraph
           "R" 'TeX-fold-clearout-region
           )
  (:states '(normal motion)
           :keymaps 'LaTeX-mode-map
           :prefix "SPC c p"
           "TAB" 'preview-goto-info-page
           "b" 'preview-buffer
           "d" 'preview-document
           "e" 'preview-environment
           "f" 'preview-cache-preamble
           "r" 'preview-region
           "s" 'preview-section
           "p" 'preview-at-point
           "B" 'preview-clearout-buffer
           "D" 'preview-clearout-document
           "E" 'preview-clearout-environment
           "P" 'preview-clearout-at-point
           "F" 'preview-cache-preamble-off
           "R" 'preview-clearout
           "S" 'preview-clearout-section
           )
  :init
  ;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-engine 'xetex)
  (TeX-command-extra-options "-shell-escape")
  (TeX-show-compilation t)
  (reftex-plug-into-AUCTeX t)
  )

(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  )

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . cdlatex-mode))

;; ;; Gnuplot mode
;; (use-package gnuplot
;;   :ensure t
;;   :mode (("\\.gnuplot\\'" . gnuplot-mode)
;;          ("\\.gp\\'" . gnuplot-mode))
;;   )

;; ESS and R
(use-package ess
  :ensure t
  :commands R
  :config
  (setq ess-eval-visibly 'nowait)	; Allow asynchronous executing
  )

;; Org mode
(use-package org
  :ensure t
  :ensure ob-ipython
  :mode ("\\.org\\'" . org-mode)
  :general
  (:states 'normal
           :keymaps 'org-mode-map
           :prefix "SPC"
           "c e" 'org-export-dispatch)
  (:states '(normal motion)
           :prefix "SPC"
           "o c" 'org-capture)
  :custom
  (org-capture-templates
   '(("t" "Todo list item"
      entry (file+headline "~/notes/tasks.org" "Tasks")
      "* TODO %?\n %i\n")
     ;; "* TODO %?\n %i\n %a")

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
  :init
  (add-hook 'org-mode-hook 'auto-fill-mode)
  ;; (setq org-startup-with-inline-images t) ; Display inline images by default
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'add-pcomplete-to-capf) ; Enable org mode completion
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local company-minimum-prefix-length 1)
                             ))
  :config
  (setq org-startup-indented t)		; Indent the tree structure

  (org-defkey org-mode-map "\C-c{" 'org-cdlatex-environment-indent)

  ;; ;; Continuous numbering of org mode equations
  ;; (defun org-renumber-environment (orig-func &rest args)
  ;;   (let ((results '())
  ;;         (counter -1)
  ;;         (numberp))
  ;;     (setq results (loop for (begin .  env) in
  ;;                         (org-element-map (org-element-parse-buffer) 'latex-environment
  ;;                                          (lambda (env)
  ;;                                            (cons
  ;;                                             (org-element-property :begin env)
  ;;                                             (org-element-property :value env))))
  ;;                         collect
  ;;                         (cond
  ;;                          ((and (string-match "\\\\begin{equation}" env)
  ;;                                (not (string-match "\\\\tag{" env)))
  ;;                           (incf counter)
  ;;                           (cons begin counter))
  ;;                          ((string-match "\\\\begin{align}" env)
  ;;                           (prog2
  ;;                               (incf counter)
  ;;                               (cons begin counter)
  ;;                             (with-temp-buffer
  ;;                               (insert env)
  ;;                               (goto-char (point-min))
  ;;                               ;; \\ is used for a new line. Each one leads to a number
  ;;                               (incf counter (count-matches "\\\\$"))
  ;;                               ;; unless there are nonumbers.
  ;;                               (goto-char (point-min))
  ;;                               (decf counter (count-matches "\\nonumber")))))
  ;;                          (t
  ;;                           (cons begin nil)))))

  ;;     (when (setq numberp (cdr (assoc (point) results)))
  ;;       (setf (car args)
  ;;             (concat
  ;;              (format "\\setcounter{equation}{%s}\n" numberp)
  ;;              (car args)))))
  ;;   (apply orig-func args))
  ;; (advice-add 'org-create-formula-image :around #'org-renumber-environment)

  (add-to-list 'image-type-file-name-regexps '("\\.eps\\'" . imagemagick))
  (add-to-list 'image-file-name-extensions "eps")
  (setq org-image-actual-width '(400)) ; Prevent inline images being too big
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images) ; Redisplay after babel executing
  (require 'ox-md)
  (require 'ox-beamer)
  (require 'ox-latex)
  (setq org-highlight-latex-and-related '(native))
  (setq org-export-coding-system 'utf-8)           ; Ensure exporting with UTF-8
  (add-to-list 'org-latex-packages-alist '("" "xeCJK"))
  (add-to-list 'org-latex-packages-alist '("" "listings")) ; Use listings package to export code blocks
  (setq org-latex-listings 'listings)
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
     (ipython . t)
     ))
  (add-to-list 'org-latex-listings-langs '(ipython "Python"))
  (setq org-src-fontify-natively t)
  ;; (setq org-preview-latex-default-process 'imagemagick)
  (setq org-format-latex-options '(:foreground auto :background "Transparent" :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                                               ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (setq org-src-window-setup 'current-window)
  (setq org-export-use-babel nil) ; Stop Org from evaluating code blocks
  (setq org-babel-python-command "python3") ; Set the command to python3 instead of python
  (setq org-confirm-babel-evaluate nil)   ; Don't prompt me to confirm everytime I want to evaluate a block
  )

(use-package htmlize
  :ensure t
  :commands htmlize-buffer		; Enable org exporting to html
  :config (setq org-html-postamble nil)		; Don't include a footer with my contact and publishing information at the bottom of every exported HTML document
  )

;; Shell
(use-package shell-pop
  :ensure t
  :defer t
  :general
  (:states '(normal motion)
           :prefix "SPC o"
           "s" 'shell-pop)
  :custom
  (shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  (shell-pop-full-span t)
  :config
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
  )
(setq password-cache t)			; Enable password caching
(setq password-cache-expiry 3600)	; Enable password caching for one hour

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.xml\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  )

;; mu4e
(use-package mu4e
  :load-path "/usr/local/Cellar/mu/1.2.0_1/share/emacs/site-lisp/mu/mu4e"
  :general
  (:states '(normal motion)
           :prefix "SPC m"
           "e" 'mu4e)
  (:keymaps 'mu4e-headers-mode-map
            "." 'hydra-mu4e-headers/body)
  :config
  (require 'org)
  (setq mail-user-agent 'mu4e-user-agent)	; Use mu4e as default mail agent
  (setq mu4e-maildir (expand-file-name "~/Maildir"))		; Mail folder set to ~/mail
  (setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc -a")
  (setq mu4e-update-interval 300)
  (setq mu4e-headers-auto-update t)
  (setq mu4e-compose-signature-auto-include nil)
  (setq mu4e-view-show-addresses 't)
  (setq mu4e-confirm-quit nil)
  (setq mu4e-view-show-images t)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  (add-hook 'mu4e-compose-mode-hook 'visual-line-mode)
  (add-hook 'mu4e-compose-mode-hook 'company-mode)
  ;; Dynamically setting the width of the columns so it takes up the whole width
  (add-hook 'mu4e-headers-mode-hook
            (defun my/mu4e-change-headers ()
              (interactive)
              (setq mu4e-headers-fields
                    `((:human-date . 12)
                      (:flags . 4)
                      (:from-or-to . 25)
                      (:subject . ,(- (window-body-width) 60))
                      (:size . 7)))))
  (add-to-list 'mu4e-view-actions
               '("browser view" . mu4e-action-view-in-browser) t)
  (add-hook 'mu4e-view-mode-hook
            (lambda()
              (local-set-key (kbd "TAB") 'org-next-link)
              (local-set-key (kbd "<backtab>") 'org-previous-link)
              (local-set-key (kbd "<return>") 'mu4e~view-browse-url-from-binding))
            )

  (setq mu4e-sent-folder   "/liushihao-pku/Sent Items")
  (setq mu4e-drafts-folder "/liushihao-pku/Drafts")
  (setq mu4e-trash-folder  "/liushihao-pku/Trash")
  (setq mu4e-refile-folder  "/liushihao-pku/Archive")
  (setq mu4e-attachment-dir  "~/Downloads")

  ;; Send emails
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq smtpmail-stream-type 'ssl)
  (setq starttls-use-gnutls t)
  ;; Personal info
  (setq user-full-name "Shihao Liu")
  (setq user-mail-address "liushihao@pku.edu.cn")
  ;; Smtpmail setup
  (setq smtpmail-smtp-server "mail.pku.edu.cn")
  (setq smtpmail-default-smtp-server "mail.pku.edu.cn")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-smtp-user "liushihao@pku.edu.cn")

  :hydra
  (hydra-mu4e-headers (:color blue :hint nil)
    "
  ^General^         ^Search^               _!_: read       _#_: deferred    ^Switches^

  -^^-----------------^^-----------------  _?_: unread     _%_: pattern    -^^------------------

  _n_: next          _s_: search           _r_: refile     _&_: custom      _O_: sorting

  _p_: prev          _S_: edit prev qry    _u_: unmk       _+_: flag        _P_: threading

  _]_: n unred       _/_: narrow search    _U_: unmk *     _-_: unflag      _Q_: full-search

  _[_: p unred       _b_: search bkmk      _d_: trash      _T_: thr         _V_: skip dups

  _y_: sw view       _B_: edit bkmk        _D_: delete     _t_: subthr      _W_: include-related

  _R_: reply         _{_: previous qry     _m_: move      -^^----------------^^------------------

  _C_: compose       _}_: next query       _a_: action     _|_: thru shl    _`_: update, reindex

  _F_: forward       _C-+_: show more      _A_: mk4actn    _H_: help        _;_: context-switch

  --------------   _C--_: show less      _*_: *thing     _q_: quit hdrs   _j_: jump2maildir
"

    ;; general
    ("n" mu4e-headers-next)
    ("p" mu4e-headers-previous)
    ("[" mu4e-select-next-unread)
    ("]" mu4e-select-previous-unread)
    ("y" mu4e-select-other-view)
    ("R" mu4e-compose-reply)
    ("C" mu4e-compose-new)
    ("F" mu4e-compose-forward)
    ;; ("o" my/org-capture-mu4e)                  ; differs from built-in

    ;; search
    ("s" mu4e-headers-search)
    ("S" mu4e-headers-search-edit)
    ("/" mu4e-headers-search-narrow)
    ("b" mu4e-headers-search-bookmark)
    ("B" mu4e-headers-search-bookmark-edit)
    ("{" mu4e-headers-query-prev)              ; differs from built-in
    ("}" mu4e-headers-query-next)              ; differs from built-in
    ("C-+" mu4e-headers-split-view-grow)
    ("C--" mu4e-headers-split-view-shrink)

    ;; mark stuff
    ("!" mu4e-headers-mark-for-read)
    ("?" mu4e-headers-mark-for-unread)
    ("r" mu4e-headers-mark-for-refile)
    ("u" mu4e-headers-mark-for-unmark)
    ("U" mu4e-mark-unmark-all)
    ("d" mu4e-headers-mark-for-trash)
    ("D" mu4e-headers-mark-for-delete)
    ("m" mu4e-headers-mark-for-move)
    ("a" mu4e-headers-action)                  ; not really a mark per-se
    ("A" mu4e-headers-mark-for-action)         ; differs from built-in
    ("*" mu4e-headers-mark-for-something)

    ("#" mu4e-mark-resolve-deferred-marks)
    ("%" mu4e-headers-mark-pattern)
    ("&" mu4e-headers-mark-custom)
    ("+" mu4e-headers-mark-for-flag)
    ("-" mu4e-headers-mark-for-unflag)
    ("t" mu4e-headers-mark-subthread)
    ("T" mu4e-headers-mark-thread)

    ;; miscellany
    ("q" mu4e~headers-quit-buffer)
    ("H" mu4e-display-manual)
    ("|" mu4e-view-pipe)                       ; does not seem built-in any longer

    ;; switches
    ("O" mu4e-headers-change-sorting)
    ("P" mu4e-headers-toggle-threading)
    ("Q" mu4e-headers-toggle-full-search)
    ("V" mu4e-headers-toggle-skip-duplicates)
    ("W" mu4e-headers-toggle-include-related)

    ;; more miscellany
    ("`" mu4e-update-mail-and-index)           ; differs from built-in
    (";" mu4e-context-switch)
    ("j" mu4e~headers-jump-to-maildir)

    ("." nil))
  )

(use-package xref
  :ensure t
  :general
  (:states '(normal motion)
           :prefix "SPC"
           "." 'xref-find-definitions
           "/" 'xref-find-references
           "C-." 'xref-find-apropos
           "," 'xref-pop-marker-stack)
  :config
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))
  )

(setq-default c-basic-offset 4)
(setq-default tab-width 4)

;; lsp mode
(use-package lsp-mode
  :ensure t
  ;; :pin melpa-stable
  :commands lsp
  :general
  (:states 'normal
           :keymaps 'lsp-mode-map
           :prefix "SPC l"
           "d" 'lsp-find-declaration
           "D" 'lsp-ui-peek-find-definitions
           "R" 'lsp-ui-peek-find-references
           ;; "i" 'lsp-ui-peek-find-implementation
           "t" 'lsp-find-type-definition
           "o" 'lsp-describe-thing-at-point
           "r" 'lsp-rename
           "f" 'lsp-format-buffer
           "m" 'lsp-ui-imenu
           "x" 'lsp-execute-code-action
           "M-s" 'lsp-describe-session
           "M-r" 'lsp-workspace-restart
           "S" 'lsp-shutdown-workspace
           "a" 'xref-find-apropos
           )
  :config
  (require 'lsp-mode)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-idle-delay 0.5)
  (setq lsp-enable-indentation nil)
  (setq lsp-before-save-edits nil)
  (setq lsp-signature-render-documentation nil)

  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  )

(use-package lsp-ui
  ;; :pin melpa-stable
  :ensure t
  :commands lsp-ui-mode
  :general
  (:keymaps 'lsp-ui-imenu-mode-map
            "b" 'lsp-ui-imenu--prev-kind
            "f" 'lsp-ui-imenu--next-kind
            "p" 'previous-line
            "n" 'next-line
            )
  :config
  (setq lsp-ui-sideline-show-hover t)
  )

;; Python
(use-package lsp-python-ms
  :ensure t
  :defer t
  :hook (python-mode . (lambda ()
                         (unless (featurep 'lsp-python-ms)
                           (require 'lsp-python-ms)
                           )
                         (lsp)
                         ))
  :custom
  (lsp-python-ms-executable "~/.config/emacs/.cache/lsp/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer")
  :config
  (setq lsp-python-ms-cache "Library")
  (defun my/format-buffer ()
    "Format buffer using yapf."
    (interactive)
    (let ((old-point (point)))
      (erase-buffer)
      (insert (shell-command-to-string (concat "yapf " (buffer-name))))
      (goto-char old-point)
      )
    )
  (general-define-key
   :states 'normal
   :keymaps 'python-mode-map
   :prefix "SPC l"
   "F" 'my/format-buffer
   )
  )

(use-package python
  :defer t
  :general
  (:states 'normal
           :keymaps 'python-mode-map
           :prefix "SPC"
           "c p" 'run-python
           "c c" 'python-shell-send-buffer
           "c r" 'python-shell-send-region
           "c l" 'python-shell-send-file
           "c s" 'python-shell-send-string
           "c f" 'python-shell-send-defun
           "c e" 'python-eldoc-at-point
           "c d" 'python-describe-at-point
           "c v" 'python-check
           "c z" 'python-shell-switch-to-shell
           "c <" 'python-indent-shift-left
           "c >" 'python-indent-shift-right
           )
  :config
  (require 'lsp-python-ms)
  )

;; Lsp c++
(use-package ccls
  :ensure t
  :defer t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda ()
           (unless (featurep 'ccls)
             (require 'ccls)
             )
           (lsp)
           (delete 'company-clang company-backends)
           (setq comment-start "/* "
                 comment-end " */")
           ))
  :config
  (defun my/cmake-project-setup-for-ccls ()
    "ccls typically indexes an entire project. In order for this
to work properly, ccls needs to be able to obtain the source file
list and their compilation command lines."
    (interactive)
    (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
      (shell-command
       (concat "cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES"
               "\nln -s Debug/compile_commands.json"))
      )
    )
  (general-define-key
   :states 'normal
   :keymaps '(c-mode-map c++-mode-map)
   :prefix "SPC l"
   "s" 'my/cmake-project-setup-for-ccls
   "l" 'ccls-code-lens-mode
   "R" 'ccls-reload
   )
  :custom
  (ccls-sem-highlight-method 'font-lock)
  (ccls-executable "~/.config/emacs/.cache/lsp/ccls/ccls")
  )

(use-package cmake-mode
  :load-path "/usr/local/Cellar/cmake/3.14.5/share/emacs/site-lisp/cmake"
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :custom
  (cmake-tab-width 4)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun company//sort-by-tabnine (candidates)                                           ;;
;;   (if (or (functionp company-backend)                                                  ;;
;;           (not (and (listp company-backend) (memq 'company-tabnine company-backend)))) ;;
;;       candidates                                                                       ;;
;;     (let ((candidates-table (make-hash-table :test #'equal))                           ;;
;;           candidates-1                                                                 ;;
;;           candidates-2)                                                                ;;
;;       (dolist (candidate candidates)                                                   ;;
;;         (if (eq (get-text-property 0 'company-backend candidate)                       ;;
;;                 'company-tabnine)                                                      ;;
;;             (unless (gethash candidate candidates-table)                               ;;
;;               (push candidate candidates-2))                                           ;;
;;           (push candidate candidates-1)                                                ;;
;;           (puthash candidate t candidates-table)))                                     ;;
;;       (setq candidates-1 (nreverse candidates-1))                                      ;;
;;       (setq candidates-2 (nreverse candidates-2))                                      ;;
;;       (nconc (seq-take candidates-1 2)                                                 ;;
;;              (seq-take candidates-2 2)                                                 ;;
;;              (seq-drop candidates-1 2)                                                 ;;
;;              (seq-drop candidates-2 2)))))                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lsp java
(use-package lsp-java
  :ensure t
  :defer t
  :hook (java-mode . (lambda ()
                       (require 'lsp-java)
                       (lsp)
                       (setq comment-start "/* "
                             comment-end " */")
                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       ;; (delete 'company-lsp company-backends)                                         ;;
                       ;; (add-to-list 'company-transformers 'company//sort-by-tabnine t)                ;;
                       ;; (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate)) ;;
                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       ))
  :config
  (setq lsp-java-save-action-organize-imports nil)
  (setq lsp-java-format-on-type-enabled nil)
  (setq lsp-java-autobuild-enabled nil)
  (setq lsp-java-code-generation-generate-comments t)
  (setq lsp-java-signature-help-enabled nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package company-tabnine                        ;;
;;   :ensure t                                         ;;
;;   :config                                           ;;
;;   (add-to-list 'company-backends #'company-tabnine) ;;
;;   )                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gud
(use-package gud
  :commands gud-gdb
  :general
  (:states '(normal motion)
           :keymaps 'prog-mode-map
           :prefix "SPC"
           "gud" 'hydra-gud/body)
  :hydra
  (hydra-gud (:hint nil :foreign-keys run)
    "
          Breakpoint^^               Run^^                 Repl^^              Stack^^          Instruction mode^^
         ----------------------------------------------------------------------------------------------------------
          [_b_] break                [_r_] run             [_p_] print         [_<_] up         [_M-s_] stepi

          [_d_] remove               [_s_] step                              [_>_] down       [_M-n_] nexti

          [_l_] list breaks          [_n_] next                              [_1_] stack current thread

          [_q_] quit                 [_c_] continue                          [_2_] stack all threads

                                   [_f_] finish
"
    ("b" gud-break)
    ("d" gud-remove)
    ("l" gud-listb)

    ("r" gud-run)
    ("s" gud-step)
    ("n" gud-next)
    ("c" gud-cont)

    ("p" gud-print)
    ("f" gud-finish)

    ("<" gud-up)
    (">" gud-down)
    ("1" gud-bt)
    ("2" gud-bt-all)

    ("M-s" gud-stepi)
    ("M-n" gud-nexti)

    ("q" nil)
    )
  )

(use-package gud-lldb
  :load-path "~/.config/emacs/packages/gud-lldb"
  :commands (lldb)
  )

;; (use-package dap-mode
;;   :ensure t
;;   :defer t
;;   :custom
;;   (dap-lldb-debug-program "/usr/local/Cellar/llvm/8.0.0_1/bin/lldb-vscode")
;;   :hook ((java-mode . (lambda ()
;;                         (dap-mode t)
;;                         (dap-ui-mode t)
;;                         ))
;;          ((c-mode c++-mode objc-mode) . (lambda ()
;;                                           (dap-mode t)
;;                                           (dap-ui-mode t)
;;                                           ))
;;          ;; (python-mode . (lambda ()
;;          ;;                   (dap-mode t)
;;          ;;                   (dap-ui-mode t)
;;          ;;                   (require 'dap-python)
;;          ;;                   ))
;;          )
;;   :init
;;   (defun my/window-visible (b-name)
;;     "Return whether B-NAME is visible."
;;     (-> (-compose 'buffer-name 'window-buffer)
;;      (-map (window-list))
;;      (-contains? b-name)))

;;   (defun my/show-debug-windows (session)
;;     "Show debug windows."
;;     (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
;;       (save-excursion
;;      ;; display locals
;;      (unless (my/window-visible dap-ui--locals-buffer)
;;           (dap-ui-locals))
;;      ;; display sessions
;;      (unless (my/window-visible dap-ui--sessions-buffer)
;;           (dap-ui-sessions))
;;      (unless (my/window-visible "*dap-ui-repl*")
;;           (dap-ui-repl))
;;      (unless (my/window-visible "*Breakpoints*")
;;           (dap-ui-breakpoints))
;;      )))
;;   (add-hook 'dap-stopped-hook 'my/show-debug-windows)
;;   (defun my/hide-debug-windows (session)
;;     "Hide debug windows when all debug sessions are dead."
;;     (unless (-filter 'dap--session-running (dap--get-sessions))
;;       (and (get-buffer dap-ui--sessions-buffer)
;;            (kill-buffer dap-ui--sessions-buffer))
;;       (and (get-buffer dap-ui--locals-buffer)
;;            (kill-buffer dap-ui--locals-buffer))
;;       (and (get-buffer "*Breakpoints*")
;;            (kill-buffer "*Breakpoints*"))
;;       (and (get-buffer "*dap-ui-repl*")
;;            (kill-buffer "*dap-ui-repl*"))
;;       (delete-other-windows)
;;       ))
;;   (add-hook 'dap-terminated-hook 'my/hide-debug-windows)

;;   :config
;;   (global-set-key (kbd "C-c 3") 'dap-debug)
;;   (global-set-key (kbd "C-c 4") 'dap-hydra)
;;   (require 'dap-lldb)
;;   ;; (require 'dap-gdb-lldb)
;;   (require 'dap-java)
;;   )

;; (setq gc-cons-threshold (* 800 1000))

(provide 'init)
;;; init.el ends here
