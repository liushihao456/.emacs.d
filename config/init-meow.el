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

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
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
   '("O" . meow-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
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
   '("-" . meow-pop-selection)
   '(";" . my/comment-dwim)
   '(":" . comment-kill)
   '("<escape>" . ignore)))

(require 'meow)
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
(global-set-key (kbd "C-x C-r") nil)
(setq meow-cursor-type-insert 'box)
(setq meow-keypad-leader-dispatch "C-c")

;; Use jk to escape from insert state to normal state
(setq meow-two-char-escape-sequence "jk")
(setq meow-two-char-escape-delay 0.5)
(defun meow--two-char-exit-insert-state (s)
  (when (meow-insert-mode-p)
    (let ((modified (buffer-modified-p)))
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
                (meow--execute-kbd-macro "<escape>"))
            (push event unread-command-events)))))))
(defun meow-two-char-exit-insert-state ()
  (interactive)
  (meow--two-char-exit-insert-state meow-two-char-escape-sequence))
(define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
  #'meow-two-char-exit-insert-state)

;; Isearch integration
(defun meow--post-isearch-function ()
  (unless isearch-mode-end-hook-quit
    (when isearch-success
      (let ((beg (car isearch-match-data))
	    (end (cadr isearch-match-data)))
	(thread-first
	  (meow--make-selection '(select . visit)
				beg
				(if isearch-forward end isearch-other-end))
	  (meow--select (not isearch-forward)))))))
(add-hook 'isearch-mode-end-hook 'meow--post-isearch-function)
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; Custom comment function
(defun my/comment-dwim (arg)
  "If region active, comment the region, else comment the line."
  (interactive "p")
  (if mark-active
      (comment-dwim nil)
    (comment-line arg)))

(provide 'init-meow)

;;; init-meow.el ends here
