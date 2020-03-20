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
 '(column-number-mode t)
 '(custom-safe-themes
   '("a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "e39ff005e524c331b08d613109bff0b55fc21c64914c4a243faa70f330015389" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "08ef1356470a9d3bf363ffab0705d90f8a492796e9db489936de4bde6a4fdb19" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" default))
 '(dired-use-ls-dired nil)
 '(electric-pair-mode t)
 '(evil-disable-insert-state-bindings t)
 '(evil-insert-state-cursor nil t)
 '(evil-operator-state-cursor nil t)
 '(evil-replace-state-cursor nil t)
 '(indent-tabs-mode nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(ess benchmark-init gnuplot-mode ivy parchment-theme ripgrep lsp-mode lsp-java lsp-ui delight solarized-theme general evil spacemacs-theme hydra web-mode auctex magit company yasnippet-snippets which-key flycheck doom-themes zenburn-theme cdlatex yasnippet))
 '(python-shell-interpreter "python3")
 '(scroll-bar-mode nil)
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
 '(default ((t (:inherit nil :extend nil :stipple nil :background "unspecified-bg" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(company-scrollbar-bg ((t (:background "color-242"))))
 '(company-scrollbar-fg ((t (:background "color-240"))))
 '(company-tooltip ((t (:background "color-244"))))
 '(company-tooltip-annotation ((t (:foreground "color-83"))))
 '(company-tooltip-common ((t (:foreground "brightwhite" :slant italic :weight bold))))
 '(company-tooltip-selection ((t (:background "color-242" :weight bold))))
 '(error ((t (:foreground "color-28" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "color-76"))))
 '(font-lock-comment-face ((t (:foreground "cyan" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "brightblue"))))
 '(font-lock-function-name-face ((t (:foreground "color-27"))))
 '(font-lock-keyword-face ((t (:foreground "yellow" :weight semi-bold))))
 '(font-lock-string-face ((t (:foreground "color-78"))))
 '(hi-green ((t (:background "color-30" :foreground "black"))))
 '(hi-pink ((t (:background "color-24" :foreground "black"))))
 '(hi-yellow ((t (:background "cyan" :foreground "black"))))
 '(highlight ((t (:background "color-234"))))
 '(isearch-fail ((t (:background "color-125"))))
 '(ivy-minibuffer-match-face-1 ((t (:background "color-28"))))
 '(ivy-minibuffer-match-face-2 ((t (:background "color-29" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:background "color-30" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:background "color-31" :weight bold))))
 '(lazy-highlight ((t (:background "red"))))
 '(lsp-ui-doc-background ((t (:background "brightblack"))))
 '(magit-section-highlight ((t (:extend t :background "color-239"))))
 '(match ((t (:background "color-102"))))
 '(minibuffer-prompt ((t (:foreground "color-171"))))
 '(mode-line ((t (:background "color-236" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "color-236" :foreground "brightcyan" :box (:line-width -1 :color "grey75") :weight light))))
 '(popup-tip-face ((t (:background "color-238"))))
 '(region ((t (:extend t :background "color-237"))))
 '(show-paren-match ((t (:underline "brightyellow" :weight bold))))
 '(warning ((t (:foreground "color-22" :weight bold))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "color-28")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Basic customizations                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)        ; hide the startup message
(fset 'yes-or-no-p 'y-or-n-p)           ; change all prompts to y or n
(setq backup-directory-alist `(("." . "~/.config/emacs/backups")))
(setq ring-bell-function 'ignore)
(setq-default fill-column 80)
(setq comment-style 'indent)
;; (global-hl-line-mode t)


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
      ;; (setq
      ;;  mac-command-modifier 'meta
      ;;  mac-option-modifier 'none
      ;;  )

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
  ;; :defer 1
  :custom
  (evil-disable-insert-state-bindings t)
  (evil-insert-state-cursor nil)
  (evil-replace-state-cursor nil)
  (evil-operator-state-cursor nil)
  (evil-symbol-word-search t)
  :config
  (evil-set-initial-state 'recentf-dialog-mode 'motion)
  (evil-set-initial-state 'use-package-statistics-mode 'motion)
  (evil-set-initial-state 'rst-mode 'motion)
  (evil-set-initial-state 'message-mode 'motion)
  (evil-set-initial-state 'flycheck-error-list-mode 'motion)
  (evil-set-initial-state 'fundamental-mode 'motion)
  (evil-set-initial-state 'TeX-output-mode 'motion)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'lsp-ui-imenu-mode 'emacs)
  (evil-set-initial-state 'ripgrep-search-mode 'emacs)
  (evil-mode 1)
  (define-key evil-motion-state-map " " nil)
  (define-key evil-normal-state-map " " nil)
  (define-key evil-visual-state-map " " nil)
  (evil-define-key '(normal motion) 'global
    (kbd "SPC w") 'evil-window-map)
  (evil-define-key 'normal 'global
    (kbd "SPC TAB") 'evil-motion-state)
  (evil-define-key 'motion 'global
    (kbd "SPC TAB") 'evil-normal-state)
  )

(with-current-buffer "*Messages*"
  (evil-motion-state))
(recentf-mode t)
(setq initial-buffer-choice 'recentf-open-files)

(defun my/open-external-terminal ()
  "Open an external Terminal window under current directory."
  (interactive)
  (shell-command "open -a Terminal .")
  )

(defun find-init-file ()
  "Find init.el file."
  (interactive)
  (find-file "~/.config/emacs/init.el")
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

(use-package general
  :ensure t
  :ensure ripgrep
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

   "SPC R" 'query-replace
   "SPC S" 'ripgrep-regexp
   "SPC O" 'occur
   "SPC [" 'highlight-symbol-at-point
   "SPC ]" 'unhighlight-regexp
   "SPC q" 'save-buffers-kill-terminal
   "SPC ;" 'comment-line
   "SPC s" 'save-buffer
   "SPC e" 'eval-last-sexp
   "SPC i" 'imenu
   "SPC k" 'kill-buffer
   "SPC D" 'dired
   "SPC b" 'switch-to-buffer
   "SPC B" 'ibuffer
   "SPC P" 'list-processes
   "SPC SPC" 'execute-extended-command

   "SPC w |" 'toggle-window-split

   "SPC p f" 'project-find-file
   "SPC p s" 'project-search
   "SPC p r" 'project-find-regexp
   "SPC p q" 'project-query-replace-regexp
   "SPC `" 'fileloop-continue

   "SPC f i" 'find-init-file
   "SPC f f" 'find-file
   "SPC f r" 'recentf-open-files

   "SPC t t" 'todo-show
   "SPC t j" 'todo-jump-to-category
   "SPC t i" 'todo-insert-item

   "SPC r w" 'window-configuration-to-register
   "SPC r f" 'frameset-to-register
   "SPC r SPC" 'point-to-register
   "SPC r j" 'jump-to-register
   "SPC r i" 'insert-register
   "SPC r m" 'bookmark-set
   "SPC r M" 'bookmark-set-no-overwrite
   "SPC r b" 'bookmark-jump
   "SPC r l" 'list-bookmarks

   "SPC o t" 'my/open-external-terminal
   "SPC d f" 'describe-function
   "SPC d F" 'describe-face
   "SPC d v" 'describe-variable
   "SPC d k" 'describe-key
   "SPC d m" 'describe-mode
   "SPC d p" 'describe-package
   "SPC d b" 'describe-bindings
   "SPC d l" 'view-lossage
   "SPC d r" 'info-emacs-manual
   "SPC d i" 'info
   )
  (general-define-key
   :states 'visual
   :prefix "SPC r"
   "s" 'copy-to-register
   "r" 'copy-rectangle-to-register
   )
  )

(defun show-bookmark-list ()
  "Show bookmark list after calling 'list-bookmars'."
  (switch-to-buffer "*Bookmark List*"))
(advice-add #'list-bookmarks :after #'show-bookmark-list)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t)
;;   )

;; (if (display-graphic-p)
;;     (use-package spacemacs-theme
;;       :ensure t
;;       :defer t
;;       :init
;;       (load-theme 'spacemacs-dark t)
;;       (setq spacemacs-theme-underline-parens t)
;;       )
;;   )

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
  :hook ((prog-mode . flycheck-mode))
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
  :hook ((prog-mode LaTeX-mode org-mode eshell-mode shell-mode inferior-python-mode) . company-mode)
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
  (add-hook 'LaTeX-mode-hook 'lsp)
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

;; (use-package cdlatex
;;   :ensure t
;;   :hook (LaTeX-mode . cdlatex-mode))

;; Gnuplot mode
(use-package gnuplot-mode
  :ensure t
  :mode (("\\.gnuplot\\'" . gnuplot-mode)
         ("\\.gp\\'" . gnuplot-mode))
  )

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
     ))
  (setq org-src-fontify-natively t)
  ;; (setq org-preview-latex-default-process 'imagemagick)
  (setq org-format-latex-options '(:foreground auto :background "Transparent" :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                                               ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (setq org-src-window-setup 'current-window)
  (setq org-export-use-babel nil) ; Stop Org from evaluating code blocks
  (setq org-babel-python-command "python3") ; Set the command to python3 instead of python
  (setq org-confirm-babel-evaluate nil)   ; Don't prompt me to confirm everytime I want to evaluate a block
  )

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
           "d" 'lsp-describe-thing-at-point
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
  (setq lsp-idle-delay 0.5)
  (setq lsp-enable-indentation nil)
  (setq lsp-before-save-edits nil)
  (setq lsp-signature-render-documentation nil)
  ;; (setq lsp-clients-texlab-executable "~/.config/emacs/.cache/lsp/texlab/target/release/texlab")
  ;; (setq lsp-log-io t)
  
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
  :load-path "~/.config/emacs/packages/lsp-python-ms"
  :defer t
  :hook (python-mode . (lambda ()
                         (lsp)
                         ))
  :custom
  (lsp-python-ms-executable "~/.config/emacs/.cache/lsp/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer")
  :config
  ;; (setq lsp-python-ms-cache "Library")
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
  :load-path "~/.config/emacs/packages/ccls"
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
    (let ((default-directory (cdr (project-current))))
      (shell-command
       (concat "\ncmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES"
               "\nln -s Debug/compile_commands.json")))
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
  :load-path "/usr/local/Cellar/cmake/3.15.4/share/emacs/site-lisp/cmake"
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (setq cmake-tab-width 4)
  )

;; Lsp java
(use-package lsp-java
  :ensure t
  :defer t
  :hook (java-mode . (lambda ()
                       (require 'lsp-java)
                       (lsp)
                       (setq comment-start "/* "
                             comment-end " */")
                       ))
  :config
  (setq lsp-java-save-action-organize-imports nil)
  (setq lsp-java-format-on-type-enabled nil)
  (setq lsp-java-autobuild-enabled nil)
  (setq lsp-java-code-generation-generate-comments t)
  (setq lsp-java-signature-help-enabled nil)
  )

(use-package gud-lldb
  :load-path "~/.config/emacs/packages/gud-lldb"
  :commands (lldb)
  )

;; (setq gc-cons-threshold (* 800 1000))

(provide 'init)
;;; init.el ends here
