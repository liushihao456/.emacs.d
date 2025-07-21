;;; init-misc.el --- Miscellaneous configurations	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Miscellaneous configurations
;; --------------------------------------

;;; Code:

(require 'init-macros)

;; Kill currnet line and copy current line
(define-advice kill-region (:around (fn &rest _) slick-cut)
  (if mark-active
      (funcall fn (region-beginning) (region-end))
    (funcall fn (line-beginning-position) (line-beginning-position 2))))

(define-advice kill-ring-save (:around (fn &rest _) slick-copy)
  (if mark-active
      (funcall fn (region-beginning) (region-end))
    (message "Copied line")
    (funcall fn (line-beginning-position) (line-beginning-position 2))))

(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my/delete-word (- arg)))

(global-set-key (kbd "M-d") 'my/delete-word)
(global-set-key (kbd "M-DEL") 'my/backward-delete-word)

(defun my/scroll-half-page-advice (fn &rest _)
  "Wrapper around scroll command to scroll only half the page.
FN is the original command."
  (interactive)
  (funcall fn (/ (window-body-height) 2)))

(advice-add #'scroll-up-command :around #'my/scroll-half-page-advice)
(advice-add #'scroll-down-command :around #'my/scroll-half-page-advice)

(defmacro shell-command-silent (cmd)
  "Call `shell-command' with CMD but inhibit the message."
  `(let ((inhibit-message t))
     (shell-command ,cmd)))

(defun my/open-external-terminal-project-root (arg)
  "Open an external Terminal or cmd.exe window at project root.
With prefix ARG, open at the current buffer file's location."
  (interactive "P")
  (require 'project)
  (let ((default-directory (if (and (not arg) (project-current))
                               (project-root (project-current))
                             default-directory)))
    (cond
     ;; Windows
     ((memq system-type '(ms-dos windows-nt cygwin))
      (let ((proc (start-process "cmd" nil "cmd.exe"
                                 "/C" "start" "cmd.exe" "/K"
                                 "C:\\Program Files (x86)\\Microsoft Visual Studio\\2022\\BuildTools\\VC\\Auxiliary\\Build\\vcvars64.bat")))
        (set-process-query-on-exit-flag proc nil)))
     ;; MacOS
     ((eq system-type 'darwin)
      (shell-command-silent "open -a Terminal ."))
     ;; Others
     (t (message "Error: unknown operating system type.")))))
(global-set-key (kbd "C-c t") 'my/open-external-terminal-project-root)

(defun my/open-external-file-explorer-project-root (arg)
  "Open external file explorer at project root.
With prefix ARG, open at the current buffer file's location."
  (interactive "P")
  (require 'project)
  (let ((default-directory (if (and (not arg) (project-current))
                               (project-root (project-current))
                             default-directory))
        cmd)
    (cond
     ;; Windows
     ((memq system-type '(cygwin windows-nt ms-dos))
      (setq cmd "explorer ."))
     ;; MacOS
     ((eq system-type 'darwin)
      (setq cmd "open ."))
     ;; Others
     (t (message "Error: unknown operating system type.")))
    (when cmd
      (shell-command-silent cmd))))
(global-set-key (kbd "C-c f e") 'my/open-external-file-explorer-project-root)

(defun switch-to-other-buffer ()
  "Switch to most recently selected buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

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

;; Hide the startup message
(setq inhibit-startup-message t)
;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; Directory for backup files
(setq backup-directory-alist `(("." . "~/.config/emacs/backups")))
;; Disable ring bell
(setq ring-bell-function 'ignore)
;; Set fill column to 80
(setq-default fill-column 80)
;; Show time cost in start-up
(add-hook 'after-init-hook (lambda () (message "Emacs started in %s" (emacs-init-time))))
;; Avoid line break in help-mode
(add-hook 'help-mode-hook 'visual-line-mode)
;; When in GUI, pixel scroll
(when (display-graphic-p)
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode)))

;; Exclude files for recentf
(recentf-mode)
(setq recentf-max-saved-items 100)
(setq recentf-exclude '("bookmarks\\'" ".*-autoloads.el\\'"))

;; From dashboard doc: if a command line argument is provided, Dashboard won't
;; be displayed.
(use-package dashboard
  :ensure t
  :demand t
  :config
  (setq dashboard-items '((recents . 5) (bookmarks . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (unless (display-graphic-p)
    (setq dashboard-startup-banner 2))
  (define-key dashboard-mode-map (kbd "n") 'widget-forward)
  (define-key dashboard-mode-map (kbd "p") 'widget-backward)
  (dashboard-setup-startup-hook))

(electric-pair-mode t)
(show-paren-mode t)
(setq help-window-select t)
(setq-default indent-tabs-mode nil)
(setq split-width-threshold 160)
(setq-default tab-width 4)
(setq-default truncate-lines t)
(blink-cursor-mode -1)
(column-number-mode t)
(setq scroll-margin 4)
(setq scroll-conservatively 101)

;; Global key bindings
(global-set-key (kbd "C-c |") 'toggle-window-split)
(global-set-key (kbd "C-c f f") 'find-file-at-point)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p p") 'project-switch-project)
(global-set-key (kbd "C-c p r") 'project-find-regexp)
(global-set-key (kbd "C-c p q") 'project-query-replace-regexp)
(global-set-key (kbd "C-c `") 'fileloop-continue)
(global-set-key (kbd "C-x v") nil)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "C-x p") 'list-processes)
(global-set-key (kbd "C-h l") 'find-library)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-x ;") 'comment-line)
(global-set-key (kbd "C-h F") 'describe-face)
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-'") 'switch-to-other-buffer)
(global-set-key (kbd "C-M-'") 'kill-current-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-`") 'save-buffer)
(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "M-a") 'backward-paragraph)

;; Expand region
(use-package expand-region
  :ensure t
  :bind ("C-\\" . er/expand-region))

;; Anzu - show match counts in mode line
(use-package anzu
  :ensure t
  :init
  (global-anzu-mode t)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key (kbd "M-s r") 'anzu-query-replace-at-cursor))

;; ;; Xref
;; (use-package xref
;;   (setq xref-prompt-for-identifier '(not xref-find-definitions
;;                                          xref-find-definitions-other-window
;;                                          xref-find-definitions-other-frame
;;                                          xref-find-references)))

;; Which-key: show key bindings below
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode))

;; Remove duplicate lines
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

;; Versatile tab key
(defun my/tab-jump-over-pair-key-filter (cmd)
  "Return CMD if there's any closing pair after point at current line.
This function is useful as a `:filter' to a conditional key
definition."
  (if (looking-at-p "\s*[])}>\"'`]") cmd))

(defun my/tab-jump-over-pair ()
  "Jump over an opening or enclosing pair character."
  (interactive)
  (save-match-data
    (re-search-forward "\s*[])}>\"'`]" (point-at-eol) t)))

(define-key prog-mode-map (kbd "TAB")
  '(menu-item "" my/tab-jump-over-pair :filter my/tab-jump-over-pair-key-filter))
(define-key prog-mode-map [tab]
  '(menu-item "" my/tab-jump-over-pair :filter my/tab-jump-over-pair-key-filter))
(use-package cc-mode
  :config
  (define-key c-mode-base-map (kbd "TAB")
    '(menu-item "" my/tab-jump-over-pair :filter my/tab-jump-over-pair-key-filter)))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; (if (eq system-type 'darwin)
;;     (setq mac-command-modifier 'meta
;;           mac-option-modifier 'none))

;; nerd-svg-icons
(use-package nerd-svg-icons-dired
  :load-path "packages/nerd-svg-icons"
  :hook (dired-mode . nerd-svg-icons-dired-mode))

(use-package nerd-svg-icons-ibuffer
  :load-path "packages/nerd-svg-icons"
  :hook (ibuffer . nerd-svg-icons-ibuffer-mode))

;; Ibuffer
(use-package ibuffer-project
  :ensure t
  :hook (ibuffer . (lambda ()
                     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :config
  (require 'nerd-svg-icons)
  (setq ibuffer-project-root-functions
        `((ibuffer-project-project-root . ,(nerd-svg-icons-icon-str "oct-repo" :face 'nerd-svg-icons-lorange))
          (file-remote-p . ,(nerd-svg-icons-icon-str "cod-terminal" :face 'nerd-svg-icons-lorange)))))

;; Deadgrep
(use-package deadgrep
  :ensure t
  :bind ("C-c s" . deadgrep))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (advice-add #'wgrep-change-to-wgrep-mode :after #'evil-normal-state)
  (advice-add #'wgrep-finish-edit :after #'evil-motion-state)
  (advice-add #'wgrep-abort-changes :after #'evil-motion-state))

(use-package wgrep-deadgrep
  :ensure t
  :hook (deadgrep-finished . wgrep-deadgrep-setup))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  :config
  (defun yas-try-key-from-dot (_start-point)
    "As `yas-key-syntaxes' element, look for dot key.
It enables expanding `foo.' to `foo->'."
    (skip-chars-backward "\."))
  (add-to-list 'yas-key-syntaxes 'yas-try-key-from-dot)

  (setq yas-triggers-in-field t)
  (yas-reload-all))

(provide 'init-misc)

;;; init-misc.el ends here
