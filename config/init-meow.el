;;; init-meow.el --- Configuration for meow.	-*- lexical-binding: t -*-

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
;; Configuration for meow - Yet another modal editing on Emacs.
;; --------------------------------------

;;; Code:

;; Embrace -- pair manipulation
(add-hook 'org-mode-hook 'embrace-org-mode-hook)
(add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
;; The builtin hooks above are autoloaded. We need to require embrace in our
;; custom hooks, otherwise embrace won't be loaded as embrace-add-pair is not
;; autoloaded.
(defun embrace-markdown-mode-hook ()
  "Embrace markdown mode hook."
  (require 'embrace)
  (dolist (lst '((?* "*" . "*")
                 (?\ "\\" . "\\")
                 (?$ "$" . "$")
                 (?/ "/" . "/")))
    (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
(add-hook 'markdown-mode-hook 'embrace-markdown-mode-hook)

;; Meow -- modal editing
(require 'meow)
(defun meow-setup ()
  "Setup meow."
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (add-hook 'meow-mode-hook (lambda () (setq delete-active-region t)))

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("b" . switch-to-buffer)
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . avy-goto-char-2)
   '("O" . er/expand-region)
   '("p" . meow-yank)
   ;; '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-reverse)
   '("v" . scroll-up-command)
   '("V" . meow-visit)
   '("'" . scroll-down-command)
   '("/" . isearch-forward)
   '("?" . query-replace)
   '("-" . meow-pop-selection)
   '(";" . my/comment-dwim)
   '(":" . comment-kill)
   '("S" . embrace-commander)
   '("<escape>" . ignore)))

(meow-setup)
(meow-global-mode 1)
(global-set-key (kbd "C-h C-f") nil)
(global-set-key (kbd "C-h C-t") nil)
(global-set-key (kbd "C-h C-p") nil)
(global-set-key (kbd "C-h C-d") nil)
(global-set-key (kbd "C-h C-a") nil)
(global-set-key (kbd "C-h C-c") nil)
(global-set-key (kbd "C-h C-e") nil)
(global-set-key (kbd "C-h C-n") nil)
(global-set-key (kbd "C-h C-o") nil)
(global-set-key (kbd "C-h C-s") nil)
(global-set-key (kbd "C-h C-w") nil)
(global-set-key (kbd "C-h C-m") nil)
(global-set-key (kbd "C-x C-r") nil)
(setq meow-cursor-type-insert 'box)
(setq meow-keypad-leader-dispatch "C-c")
(setq meow-keypad-ctrl-meta-prefix ?M)
(push '(diff-mode . motion) meow-mode-state-list)

;; Use jk to escape from insert state to normal state
(defvar meow-two-char-escape-sequence "jk")
(defvar meow-two-char-escape-delay 0.5)
(defun meow--two-char-exit-insert-state (s)
  "Exit meow insert state when pressing consecutive two keys.

S is string of the two-key sequence."
  (when (meow-insert-mode-p)
    (let ((modified (buffer-modified-p))
          (undo-list buffer-undo-list))
      (insert (elt s 0))
      (let* ((second-char (elt s 1))
             (event
              (if defining-kbd-macro
                  (read-event nil nil)
              (read-event nil nil meow-two-char-escape-delay))))
        (when event
          (if (and (characterp event) (= event second-char))
              (progn
                (backward-delete-char 1)
                (set-buffer-modified-p modified)
                (setq buffer-undo-list undo-list)
                (meow-insert-exit))
            (push event unread-command-events)))))))
(defun meow-two-char-exit-insert-state ()
  "Exit meow insert state when pressing consecutive two keys."
  (interactive)
  (meow--two-char-exit-insert-state meow-two-char-escape-sequence))
(define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
  #'meow-two-char-exit-insert-state)

;; Isearch integration
(defun meow--post-isearch-function ()
  "Isearch integration with meow."
  (unless isearch-mode-end-hook-quit
    (when isearch-success
      (let ((beg (car isearch-match-data))
	        (end (cadr isearch-match-data))
            text)
	    (thread-first
	      (meow--make-selection '(select . visit)
	    		                beg
	    		                (if isearch-forward end isearch-other-end))
          (meow--select (not isearch-forward)))
        (setq text (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
        (meow--push-search text)
        (meow--ensure-visible)
        (meow--highlight-regexp-in-buffer text)))))
(add-hook 'isearch-mode-end-hook 'meow--post-isearch-function)
;; Seamless wrap search when needed
(defadvice isearch-search (after isearch-no-fail activate)
  "Seamless wrap search when needed."
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; Show search indicator in mode line instead of making overlays at the end of
;; line.
;;
;; Before:
;;
;; `meow-search' calls `meow--remove-search-indicator' then `meow--show-indicator'.
;;
;; `pre-command-hook' calls `meow--remove-search-highlight' unless the command
;; is `meow-search'
;;
;; After:
;;
;; `meow--remove-search-highlight' has an after advice that sets up mode line
;; indicator if the command is one of `meow-search', `meow-mark-word' or
;; `meow-mark-symbol', and the indicator is not yet setup, and removes the mode
;; line indicator if the command is not one of `meow-search', `meow-mark-word'
;; or `meow-mark-symbol', and the indicator is active.
;;
;; `meow--show-indicator' has an override advice that updates the search index
;; and count information.
(require 'anzu)
(defface meow/search-mode-line-indicator-face
  '((t (:inherit anzu-mode-line)))
  "Face for meow search mode line indicator."
  :group 'meow)
(defvar meow/search-current-position 0)
(defvar meow/search-total-matched 0)
(defvar meow/search-indicator-active nil)
(defconst meow/search-indicator-mode-line-format '(:eval (meow/search-update-mode-line)))
(defun meow/search-update-mode-line ()
  "Meow search mode line indicator segment."
  (propertize (format "(%d/%d)" meow/search-current-position meow/search-total-matched)
              'face 'meow/search-mode-line-indicator-face))

(defun meow--show-indicator-advice (pos idx cnt)
  "Advice to show the search indicator in mode line.
POS, IDX, CNT are arguments to the original function."
  (setq meow/search-current-position idx)
  (setq meow/search-total-matched cnt)
  (force-mode-line-update))
(advice-add #'meow--show-indicator :override #'meow--show-indicator-advice)

(defun meow/search-setup-mode-line-indicator ()
  "Setup meow search mode line indicator."
  (if (bound-and-true-p mini-modeline-mode)
      (setq mini-modeline-r-format (cons meow/search-indicator-mode-line-format mini-modeline-r-format))
    (setq mode-line-format (cons meow/search-indicator-mode-line-format mode-line-format)))
  (setq meow/search-indicator-active t))

(defun meow/search-reset-mode-line-indicator ()
  "Reset meow search mode line indicator."
  (if (bound-and-true-p mini-modeline-mode)
      (setq mini-modeline-r-format (delete meow/search-indicator-mode-line-format mini-modeline-r-format))
    (setq mode-line-format (delete meow/search-indicator-mode-line-format mode-line-format)))
  (setq meow/search-indicator-active nil))

(defun meow--highlight-pre-command-after-advice ()
  "After-advice for `meow--highlight-pre-command'."
  (when (and (memq this-command
                   '(meow-search meow-mark-word meow-mark-symbol isearch-exit))
             (not meow/search-indicator-active))
    (meow/search-setup-mode-line-indicator))
  (when (and
         (not (memq this-command
                    '(meow-search meow-mark-word meow-mark-symbol isearch-exit)))
         meow/search-indicator-active)
    (meow/search-reset-mode-line-indicator)))
(advice-add #'meow--highlight-pre-command :after #'meow--highlight-pre-command-after-advice)

;; Custom comment function
(defun my/comment-dwim (arg)
  "If region active, comment the region, else comment the line.

ARG only takes effect when region is not active.

With positive prefix, apply to N lines including current one.

With negative prefix, apply to -N lines above.  Also, further
consecutive invocations of this command will inherit the negative
argument."
  (interactive "p")
  (if mark-active
      (comment-dwim nil)
    (comment-line arg)))

;; Delete whitespaces of blank line when exiting insert mode.
(defun my/meow-escape-advice (&rest _)
  "Delete whitespaces of blank line when exiting insert mode."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (line-string (buffer-substring-no-properties bol eol)))
    (when (string-blank-p line-string)
      (delete-horizontal-space))))
(advice-add #'meow-insert-exit :before #'my/meow-escape-advice)

;; Indent current line if it's empty after entering insert state
(defun my/meow-indent-after-enter-insert (state &rest _)
  "Indent current line if it's empty after entering insert STATE."
  (when (eq state 'insert)
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (line-string (buffer-substring-no-properties bol eol)))
      (when (string-blank-p line-string)
        (indent-according-to-mode)))))
(advice-add #'meow--switch-state :after #'my/meow-indent-after-enter-insert)

(provide 'init-meow)

;;; init-meow.el ends here
