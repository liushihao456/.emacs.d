;;; init-meow.el --- Configuration for meow.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for meow - Yet another modal editing on Emacs.
;; --------------------------------------

;;; Code:

(require 'evil)

(with-eval-after-load 'evil
  (setq evil-insert-state-cursor 'box)
  (setq evil-want-C-u-scroll t)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

  ;; Mode specific keymaps
  (add-to-list 'evil-motion-state-modes 'special-mode)
  (add-to-list 'evil-motion-state-modes 'dashboard-mode)
  (add-to-list 'evil-motion-state-modes 'symbols-outline-mode)
  (add-to-list 'evil-motion-state-modes 'deadgrep-mode)
  (with-eval-after-load 'treemacs
    (evil-make-overriding-map treemacs-mode-map))
  (with-eval-after-load 'dashboard
    (evil-make-overriding-map dashboard-mode-map))
  (with-eval-after-load 'symbols-outline
    (evil-make-overriding-map symbols-outline-mode-map))
  (with-eval-after-load 'deadgrep
    (evil-make-overriding-map deadgrep-mode-map))

  ;; Quit minibuffer with ESC
  (defun evil-minibuffer-quit ()
    "Keyboard escape quit in minibuffer."
    (interactive)
    (if (fboundp 'minibuffer-keyboard-quit)
        (call-interactively #'minibuffer-keyboard-quit)
      (call-interactively #'abort-recursive-edit)))
  (defun evil--minibuffer-setup ()
    (local-set-key (kbd "<escape>") #'evil-minibuffer-quit))
  (add-hook 'minibuffer-setup-hook #'evil--minibuffer-setup))

(evil-mode)
(require 'evil-anzu)

(require 'evil-keypad)
;; (evil--setup-which-key t)
(setq evil-keypad-leader-dispatch "C-c")
(evil-define-key 'normal global-map (kbd "SPC") 'evil-keypad-state)
(evil-define-key 'motion global-map (kbd "SPC") 'evil-keypad-state)

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
(global-set-key (kbd "C-c b") 'switch-to-buffer)
(global-set-key (kbd "C-c B") 'ibuffer)
(global-set-key (kbd "C-c ;") 'comment-line)
(global-set-key (kbd "C-c /") 'evil-keypad-describe-key)

(provide 'init-evil)

;;; init-evil.el ends here

