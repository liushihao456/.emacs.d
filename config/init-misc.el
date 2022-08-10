;;; init-misc.el --- Miscellaneous configurations	-*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

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

(defun scroll-half-page-down (&rest _)
  "Wrapper around scroll-down-command: scroll down half the page."
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up (&rest _)
  "Wrapper around scroll-up-command: scroll up half the page."
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(advice-add 'scroll-up-command :around 'scroll-half-page-up)
(advice-add 'scroll-down-command :around 'scroll-half-page-down)

(defun my/open-external-terminal-project-root ()
  "Open an external Terminal window under current directory."
  (interactive)
  (require 'project)
  (if (project-root (project-current))
      (let ((default-directory (project-root (project-current))))
        (shell-command "open -a Terminal ."))
    (shell-command "open -a Terminal .")))

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
  (setq initial-frame-alist '((top . 1) (left . 1) (width . 100) (fullscreen . fullheight)))
  (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-21"))
  (setq face-font-rescale-alist `(("STkaiti" . ,(/ 20.0 21))))
  (set-fontset-font t 'han      (font-spec :family "STkaiti"))
  (set-fontset-font t 'cjk-misc (font-spec :family "STkaiti"))
  )

;; Open recent files list at Emacs start up
(defun my/command-line-args-has-file-p ()
  "Check if Emacs is called with a file name in command line args."
  (> (length command-line-args) 1))
(recentf-mode t)
;; (unless (my/command-line-args-has-file-p)
;;   (setq initial-buffer-choice 'recentf-open-files))

(dashboard-setup-startup-hook)
(with-eval-after-load 'dashboard
  (setq dashboard-items '((recents . 5) (bookmarks . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-startup-banner 2)
  (define-key dashboard-mode-map (kbd "n") 'widget-forward)
  (define-key dashboard-mode-map (kbd "p") 'widget-backward))

(with-eval-after-load 'dired (setq dired-use-ls-dired nil))
(electric-pair-mode t)
(setq enable-recursive-minibuffers t)
(setq help-window-select t)
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(when (functionp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(show-paren-mode t)
;; (define-advice show-paren-function (:around (fn) fix)
;;   "Highlight enclosing parens."
;;   (cond ((looking-at-p "\\s(") (funcall fn))
;;         (t (save-excursion
;;              (ignore-errors (backward-up-list))
;;              (funcall fn)))))
(setq split-width-threshold 100)
(setq-default tab-width 4)
(tool-bar-mode -1)
(setq-default truncate-lines t)
(blink-cursor-mode -1)
(setq c-basic-offset 4)
(c-set-offset 'arglist-intro '+)
(c-set-offset 'arglist-close '0)
(column-number-mode t)
(setq scroll-margin 4)
(setq scroll-conservatively 101)

;; Global key bindings
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-c |") 'toggle-window-split)
(global-set-key (kbd "C-c t") 'my/open-external-terminal-project-root)
(global-set-key (kbd "C-c f r") 'recentf-open-files)
(global-set-key (kbd "C-c f f") 'find-file-at-point)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p p") 'project-switch-project)
;; (global-set-key (kbd "C-c p s") 'project-search)
(global-set-key (kbd "C-c p r") 'project-find-regexp)
(global-set-key (kbd "C-c p q") 'project-query-replace-regexp)
(global-set-key (kbd "C-c `") 'fileloop-continue)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "C-x p") 'list-processes)
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
;; Writeroom mode
;; (global-set-key (kbd "M-`") 'writeroom-mode)
;; Cheat.sh
(global-set-key (kbd "C-c M-s") 'cheat-sh-search)

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
;; Marginalia mode
(marginalia-mode t)
;; Which-key: show key bindings below
(which-key-mode)
(which-key-setup-side-window-bottom)

;; C indentation style
(setq c-default-style
      '((java-mode . "java") (other . "awk")))

;; Markdown mode
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode))

;; Treemacs
(global-set-key (kbd "M-z") 'treemacs)
(with-eval-after-load 'treemacs
  ;; Fixes compatibility with emacs trunk, where an additional item was added to the project-current list.
  (defun my/treemacs--current-builtin-project-function (&rest _)
    "Find the current project.el project."
    (declare (side-effect-free t))
    (-some-> (project-current) (project-root) (file-truename) (treemacs-canonical-path)))
  ;; (advice-add 'treemacs--current-builtin-project-function :around #'my/treemacs--current-builtin-project-function)
  (setq treemacs--find-user-project-functions (list #'my/treemacs--current-builtin-project-function))

  (treemacs-follow-mode)
  (treemacs-tag-follow-mode)
  (treemacs-project-follow-mode)
  (setq treemacs-tag-follow-delay 0.1)
  (setq treemacs-project-follow-cleanup t)
  (setq treemacs-follow-after-init t)
  (setq treemacs-width 30)

  (if (display-graphic-p)
    (progn
      (require 'doom-themes)
      (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
      (doom-themes-treemacs-config))
    (treemacs-nerd-config)))

;; ;; Copilot
;; (add-hook 'prog-mode-hook 'copilot-mode)

;; Telega
(with-eval-after-load 'telega
  (setq telega-proxies
        (list '(:server "127.0.0.1" :port 7890 :enable t
                        :type (:@type "proxyTypeSocks5"))))
  (setq telega-use-docker nil))

(provide 'init-misc)

;;; init-misc.el ends here
