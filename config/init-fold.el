;;; init-fold.el --- Code/text folding	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Folding configuration.
;; --------------------------------------

;;; Code:

(require 'init-macros)

(use-package origami
  :ensure t
  :hook (prog-mode . origami-mode)
  :config
  (define-key origami-mode-map (kbd "M-RET") 'origami-recursively-toggle-node)

  ;; `->' and `-last-item' below requires dash
  (use-package dash
    :ensure t
    :demand t)
  (defun origami-parent-fold (buffer point)
    "Move point to the beginning of the parent fold of the fold the point is
currently in."
    (interactive (list (current-buffer) (point)))
    (-when-let (tree (origami-get-fold-tree buffer))
      (-when-let (path (origami-fold-find-path-containing tree point))
        (push-mark)
        (-when-let (c (-> (origami-fold-parent path)
                          origami-fold-beg))
          (goto-char c)))))

  (defun origami-child-fold (buffer point)
    "Move point to the beginning of the first child fold of the fold the point is
currently in."
    (interactive (list (current-buffer) (point)))
    (-when-let (tree (origami-get-fold-tree buffer))
      (-when-let (path (origami-fold-find-path-containing tree point))
        (push-mark)
        (-when-let (c (-> (-last-item path)
                          (origami-fold-children)
                          (-first-item)
                          origami-fold-beg))
          (goto-char c)))))

  (def-transient-commands origami-mode-map "C-c o"
                          ("n" . origami-forward-fold-same-level)
                          ("p" . origami-backward-fold-same-level)
                          ("o" . origami-forward-toggle-node)
                          ("a" . origami-open-all-nodes)
                          ("c" . origami-close-all-nodes)
                          ("t" . origami-toggle-all-nodes)
                          ("u" . origami-parent-fold)
                          ("d" . origami-child-fold)
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

(provide 'init-fold)

;;; init-fold.el ends here
