;;; init-meow.el --- Configuration for meow.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for meow - Yet another modal editing on Emacs.
;; --------------------------------------

;;; Code:

(use-package evil
  :ensure t
  :custom
  (evil-kill-on-visual-paste nil)
  (evil-symbol-word-search t)
  (evil-want-C-u-scroll t)
  :init
  (setq evil-disable-insert-state-bindings t) ; has to be set before loading Evil
  (evil-mode)
  :config
  (setq evil-insert-state-cursor 'box)
  (define-key evil-normal-state-map (kbd "gr") 'xref-find-references)
  (define-key evil-normal-state-map (kbd "[f") 'beginning-of-defun)
  (define-key evil-normal-state-map (kbd "]f") 'end-of-defun)

  (evil-define-text-object evil-defun-textobj (count &optional _beg _end type)
    "Text object to select the top-level Lisp form or function definition at point."
    (cl-destructuring-bind (beg . end)
        (bounds-of-thing-at-point 'defun)
      (evil-range beg end type)))
  (define-key evil-outer-text-objects-map "f" 'evil-defun-textobj)
  (define-key evil-inner-text-objects-map "f" 'evil-defun-textobj)

  ;; Mode specific keymaps
  (add-to-list 'evil-emacs-state-modes 'diff-mode)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-to-list 'evil-motion-state-modes 'special-mode)
  (add-to-list 'evil-motion-state-modes 'dashboard-mode)
  (add-to-list 'evil-motion-state-modes 'symbols-outline-mode)
  (add-to-list 'evil-motion-state-modes 'deadgrep-mode)
  (with-current-buffer (messages-buffer) (evil-motion-state))
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
  (with-eval-after-load 'grep
    (evil-make-overriding-map grep-mode-map 'motion))
  (with-eval-after-load 'dired
    (evil-make-overriding-map dired-mode-map 'motion))
  (with-eval-after-load 'compile
    (evil-make-overriding-map compilation-mode-map 'motion))

  ;; Quit minibuffer with ESC
  (defun evil-minibuffer-quit ()
    "Keyboard escape quit in minibuffer."
    (interactive)
    (if (fboundp 'minibuffer-keyboard-quit)
        (call-interactively #'minibuffer-keyboard-quit)
      (call-interactively #'abort-recursive-edit)))
  (defun evil--minibuffer-setup ()
    (local-set-key (kbd "<escape>") #'evil-minibuffer-quit))
  (add-hook 'minibuffer-setup-hook #'evil--minibuffer-setup)

  ;; Replace current word or selection using vim style for evil mode
  (defun evil-replace-symbol-at-point(arg)
    (interactive "P")
    (unless (use-region-p)
      (let ((symbol (thing-at-point (if arg 'word 'symbol))))
        (minibuffer-with-setup-hook
            (lambda () (backward-char 3))
          (evil-ex (concat "%s/" symbol "/" symbol "/gc"))))))
  (global-set-key (kbd "C-c r") 'evil-replace-symbol-at-point))

(use-package evil-anzu
  :ensure t
  :demand t
  :after evil)

(use-package evil-surround
  :ensure t
  :demand t
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-commentary
  :ensure t
  :demand t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-keypad
  :load-path "packages/evil-keypad"
  :demand t
  :after evil
  :config
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
  (global-set-key (kbd "C-c /") 'evil-keypad-describe-key))

(provide 'init-evil)

;;; init-evil.el ends here

