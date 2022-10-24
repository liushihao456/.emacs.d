;;; init-fold.el --- Code/text folding	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Folding configuration.
;; --------------------------------------

;;; Code:

(require 'init-macros)

(with-eval-after-load 'origami
  (define-key origami-mode-map (kbd "C-c o o") 'origami-forward-toggle-node)
  (define-key origami-mode-map (kbd "M-RET") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c o s") 'origami-show-only-node)
  (define-key origami-mode-map (kbd "C-c o u") 'origami-undo)
  (define-key origami-mode-map (kbd "C-c o r") 'origami-redo)
  (define-key origami-mode-map (kbd "C-c o R") 'origami-reset)
  (define-key origami-mode-map (kbd "C-c o a") 'origami-open-all-nodes)
  (define-key origami-mode-map (kbd "C-c o t") 'origami-toggle-all-nodes)

  (def-transient-commands origami-mode-map "C-c o"
    ("n" . origami-next-fold)
    ("p" . origami-previous-fold)
    ("o" . origami-forward-toggle-node)
    ("a" . origami-open-all-nodes)
    ("t" . origami-toggle-all-nodes)
    ("u" . origami-undo)
    ("r" . origami-redo)
    ("R" . origami-reset)
    ("s" . origami-show-only-node)
    ("TAB" . origami-recursively-toggle-node))

  (defun my/origami-recenter-a (&rest _)
    "Recenter point to the middle of screen."
    (recenter))
  (advice-add #'origami-show-only-node :after #'my/origami-recenter-a))

(add-hook 'prog-mode-hook 'origami-mode)

(provide 'init-fold)

;;; init-fold.el ends here
