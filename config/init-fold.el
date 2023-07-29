;;; init-fold.el --- Code/text folding	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Folding configuration.
;; --------------------------------------

;;; Code:

(require 'init-macros)

(with-eval-after-load 'origami
  (define-key origami-mode-map (kbd "M-RET") 'origami-recursively-toggle-node)

  (def-transient-commands origami-mode-map "C-c o"
    ("n" . origami-next-fold)
    ("p" . origami-previous-fold)
    ("o" . origami-forward-toggle-node)
    ("a" . origami-open-all-nodes)
    ("c" . origami-close-all-nodes)
    ("t" . origami-toggle-all-nodes)
    ("u" . origami-undo)
    ("r" . origami-redo)
    ("R" . origami-reset)
    ("s" . origami-show-only-node)
    ("TAB" . origami-recursively-toggle-node))

  (defun my/origami-recenter-a (&rest _)
    "Recenter point to the middle of screen."
    (recenter))
  (advice-add #'origami-show-only-node :after #'my/origami-recenter-a)

  (defun origami-recursively-toggle-node-a (orig-fn &rest args)
    "Advice around `origami-recursively-toggle-node' that makes it behave more intuitively."
    (let ((last-command 'origami-recursively-toggle-node))
      (apply orig-fn args)))
  (advice-add #'origami-recursively-toggle-node
              :around #'origami-recursively-toggle-node-a))

(add-hook 'prog-mode-hook 'origami-mode)

(provide 'init-fold)

;;; init-fold.el ends here
