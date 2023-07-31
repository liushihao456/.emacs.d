;;; init-meow.el --- Configuration for meow.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for meow - Yet another modal editing on Emacs.
;; --------------------------------------

;;; Code:

(with-eval-after-load 'evil
  (setq evil-insert-state-cursor 'box)
  (define-key evil-normal-state-map (kbd "gr") 'xref-find-references)
  (define-key evil-normal-state-map (kbd "[f") 'beginning-of-defun)
  (define-key evil-normal-state-map (kbd "]f") 'end-of-defun)

  ;; Mode specific keymaps
  (add-to-list 'evil-emacs-state-modes 'diff-mode)
  (add-to-list 'evil-motion-state-modes 'special-mode)
  (add-to-list 'evil-motion-state-modes 'dashboard-mode)
  (add-to-list 'evil-motion-state-modes 'symbols-outline-mode)
  (add-to-list 'evil-motion-state-modes 'deadgrep-mode)
  (with-eval-after-load 'treemacs
    (evil-make-overriding-map treemacs-mode-map 'motion))
  (with-eval-after-load 'dashboard
    (evil-make-overriding-map dashboard-mode-map 'motion))
  (with-eval-after-load 'symbols-outline
    (evil-make-overriding-map symbols-outline-mode-map 'motion))
  (with-eval-after-load 'deadgrep
    (evil-make-overriding-map deadgrep-mode-map 'motion))
  (with-eval-after-load 'help-mode
    (evil-make-overriding-map help-mode-map 'motion))
  (with-eval-after-load 'flycheck
    (evil-make-overriding-map flycheck-error-list-mode-map 'motion))
  (with-eval-after-load 'xref
    (evil-make-overriding-map xref--xref-buffer-mode-map 'motion))
  (with-eval-after-load 'org
    (evil-make-overriding-map org-mode-map 'normal))

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

(require 'evil)
(evil-mode)
(require 'evil-anzu)
(global-evil-surround-mode)
(evil-commentary-mode)

(require 'evil-keypad)
(evil--setup-which-key t)
(setq evil-keypad-leader-dispatch "C-c")
(evil-define-key 'normal global-map (kbd "SPC") 'evil-keypad-state)
(evil-define-key 'motion global-map (kbd "SPC") 'evil-keypad-state)
(define-key evil-normal-state-map (kbd "SPC") 'evil-keypad-state)
(define-key evil-motion-state-map (kbd "SPC") 'evil-keypad-state)
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

;; Tree-sitter text objects
(define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
(define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
(define-key evil-outer-text-objects-map "F" (evil-textobj-tree-sitter-get-textobj "call.outer"))
(define-key evil-inner-text-objects-map "F" (evil-textobj-tree-sitter-get-textobj "call.inner"))
(define-key evil-outer-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "class.outer"))
(define-key evil-inner-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "class.inner"))
(define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
(define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "comment.inner"))
(define-key evil-outer-text-objects-map "v" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
(define-key evil-inner-text-objects-map "v" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
(define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
(define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))

(provide 'init-evil)

;;; init-evil.el ends here

