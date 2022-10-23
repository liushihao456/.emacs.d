;;; init-misc.el --- Miscellaneous configurations	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Miscellaneous configurations
;; --------------------------------------

;;; Code:

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

(when (display-graphic-p)
  ;;; I prefer cmd key for meta
  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier 'none)
  )

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
;; When in GUI, set fonts
(when (display-graphic-p)
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode)))

;; Exclude files for recentf
(recentf-mode)
(setq recentf-max-saved-items 100)
(setq recentf-exclude '("bookmarks\\'" ".*-autoloads.el\\'"))

;; ;; Open recent files list at Emacs start up
;; (unless (> (length command-line-args) 1)
;;   ;; Check if Emacs is called with a file name in command line args.
;;   (setq initial-buffer-choice 'recentf-open-files))

;; From dashboard doc: if a command line argument is provided, assume a filename
;; and skip displaying Dashboard.
(with-eval-after-load 'dashboard
  (setq dashboard-items '((recents . 5) (bookmarks . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (unless (display-graphic-p)
    (setq dashboard-startup-banner 2))
  (define-key dashboard-mode-map (kbd "n") 'widget-forward)
  (define-key dashboard-mode-map (kbd "p") 'widget-backward))
(dashboard-setup-startup-hook)

(with-eval-after-load 'dired (setq dired-use-ls-dired nil))
(electric-pair-mode t)
(show-paren-mode t)
(setq help-window-select t)
(setq-default indent-tabs-mode nil)
(setq split-width-threshold 100)
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
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-x v") nil)
(global-set-key (kbd "C-c v") vc-prefix-map)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "C-x p") 'list-processes)
(global-set-key (kbd "C-h l") 'find-library)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-x ;") 'comment-line)
(global-set-key (kbd "C-h F") 'describe-face)
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-'") 'switch-to-other-buffer)
(global-set-key (kbd "C-M-'") 'kill-this-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-`") 'save-buffer)
(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "M-a") 'backward-paragraph)
;; Magit
(global-set-key (kbd "C-c g") 'magit-status)
;; Expand region
(global-set-key (kbd "C-\\") 'er/expand-region)

;; Anzu - show match counts in mode line
(global-anzu-mode t)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key (kbd "M-s r") 'anzu-query-replace-at-cursor)
;; Occur
(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)
;; Automatically pop to some buffers
(add-hook 'occur-hook (lambda () (pop-to-buffer (get-buffer "*Occur*"))))
(add-hook 'bookmark-bmenu-mode-hook (lambda () (switch-to-buffer "*Bookmark List*")))
(add-hook 'process-menu-mode-hook (lambda () (pop-to-buffer "*Process List*")))
;; Xref
(with-eval-after-load 'xref
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references)))
;; Which-key: show key bindings below
(which-key-mode)

;; C indentation style
(setq c-basic-offset 4)
(c-set-offset 'arglist-intro '+)
(c-set-offset 'arglist-close '0)

;; Markdown mode
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode))

;; Treemacs
(global-set-key (kbd "C-c e e") 'treemacs-select-window)
(add-hook 'treemacs-mode-hook (lambda () (setq-local scroll-margin 0)))
(with-eval-after-load 'treemacs
  (defun my/treemacs-kill-buffer ()
    "Kill treemacs buffer even when not in treemacs window."
    (interactive)
    (let ((visibility (treemacs-current-visibility)))
      (if (eq visibility 'none)
          (treemacs-log-failure "No treemacs buffer alive.")
        (if (eq visibility 'visible)
            (progn
              (unless treemacs--in-this-buffer (treemacs--select-visible-window))
              (treemacs-kill-buffer))
          (kill-buffer (treemacs-get-local-buffer))
          (run-hooks treemacs-kill-hook))
        (treemacs-log "Killed treemacs buffer."))))
  (global-set-key (kbd "C-c e k") 'my/treemacs-kill-buffer)

  (defun my/treemacs-quit ()
    "Quit treemacs window even when not in it."
    (interactive)
    (let ((visibility (treemacs-current-visibility)))
      (if (not (eq visibility 'visible))
          (treemacs-log-failure "Treemacs window not visible.")
        (unless treemacs--in-this-buffer (treemacs--select-visible-window))
        (treemacs-quit))))
  (global-set-key (kbd "C-c e q") 'my/treemacs-quit)

  (treemacs-follow-mode)
  (treemacs-project-follow-mode)
  (setq treemacs--project-follow-delay 0.1)
  (setq treemacs-file-follow-delay 0.1)
  (setq treemacs-project-follow-cleanup t)
  (setq treemacs-follow-after-init t)
  ;; (setq treemacs-width 30)
  (setq treemacs-indentation 1)
  (setq treemacs-is-never-other-window t)

  (defadvice treemacs-visit-node-default (after treemacs-extra-wide-toggle-off activate)
    "Restore Treemacs buffer if it's in extr-wide state."
    (if (get 'treemacs-extra-wide-toggle :toggle-on)
        (with-selected-window (treemacs-get-local-window)
          (treemacs--set-width treemacs-width)
          (put 'treemacs-extra-wide-toggle :toggle-on nil)
          (treemacs-log "Switched to normal width display"))))
  (defadvice treemacs-quit (after treemacs-quit-reset-extra-wide activate)
    (put 'treemacs-extra-wide-toggle :toggle-on nil)
    (treemacs-log "Switched to normal width display"))

  (defun my/treemacs-ignore-file-predicate (file _)
    (or (string= file ".gitignore")
        (string-suffix-p ".pyc" file)
        (string= file "__pycache__")
        (string-prefix-p ".cache" file)))
  (push #'my/treemacs-ignore-file-predicate treemacs-ignored-file-predicates)

  ;; icon-tools-treemacs-icons displays svg icons in GUI and nerd font icons in TUI.
  (icon-tools-treemacs-icons-config))

;; Icon-tools-dired
(add-hook 'dired-mode-hook 'icon-tools-dired-mode)

;; Telega
(with-eval-after-load 'telega
  (setq telega-proxies
        (list '(:server "127.0.0.1" :port 7890 :enable t
                        :type (:@type "proxyTypeSocks5"))))
  (setq telega-use-docker nil))

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
  "Return CMD if there's any opening or closing pair after point at current line.
This function is useful as a `:filter' to a conditional key definition."
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

;; Diff-hl mode
(setq diff-hl-command-prefix (kbd "C-c v"))
(with-eval-after-load 'diff-hl
  (defun my/diff-hl-define-bitmaps (&rest _)
    (define-fringe-bitmap 'diff-hl-bmp-middle [#b00011000] nil nil '(center repeated))
    (define-fringe-bitmap 'diff-hl-bmp-delete [#b11110000
                                               #b11100000
                                               #b11000000
                                               #b10000000] nil nil 'top))

  (advice-add #'diff-hl-define-bitmaps :override #'my/diff-hl-define-bitmaps)
  (defun my/diff-hl-type-face-fn (type _pos)
    (intern (format "diff-hl-%s" type)))
  (defun my/diff-hl-type-at-pos-fn (type _pos)
    (if (eq type 'delete)
        'diff-hl-bmp-delete
      'diff-hl-bmp-middle))
  (advice-add #'diff-hl-fringe-bmp-from-pos  :override #'my/diff-hl-type-at-pos-fn)
  (advice-add #'diff-hl-fringe-bmp-from-type :override #'my/diff-hl-type-at-pos-fn)
  (setq diff-hl-draw-borders nil)
  (with-eval-after-load 'flycheck
    (setq flycheck-indication-mode 'right-fringe)
    ;; Let the arrow point left
    (when (fboundp 'define-fringe-bitmap) ;; #ifdef HAVE_WINDOW_SYSTEM
      (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
        flycheck-fringe-bitmap-double-left-arrow)
      (define-fringe-bitmap
        'flycheck-fringe-bitmap-double-arrow-hi-res
        flycheck-fringe-bitmap-double-left-arrow-hi-res
        nil 16))))

(global-diff-hl-mode)
(unless (display-graphic-p) (diff-hl-margin-mode))
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'vc-dir-mode-hook 'diff-hl-dir-mode)

;; Rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(provide 'init-misc)

;;; init-misc.el ends here
