;;; init-fold.el --- Code/text folding	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Folding configuration.
;; --------------------------------------

;;; Code:

(with-eval-after-load 'origami
  (define-key origami-mode-map (kbd "C-c o o") 'origami-forward-toggle-node)
  (define-key origami-mode-map (kbd "M-RET") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c o s") 'origami-show-only-node)
  (define-key origami-mode-map (kbd "C-c o u") 'origami-undo)
  (define-key origami-mode-map (kbd "C-c o r") 'origami-redo)
  (define-key origami-mode-map (kbd "C-c o M-r") 'origami-reset)
  (define-key origami-mode-map (kbd "C-c o a") 'origami-open-all-nodes)
  (define-key origami-mode-map (kbd "C-c o t") 'origami-toggle-all-nodes)

  (defun transient/origami-next-fold (buffer point)
    "Navigate to the next origami fold, enabling pressing single
key for subsequent movements."
    (interactive (list (current-buffer) (point)))
    (let ((echo-keystrokes nil))
      (origami-next-fold buffer point)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map [?n] 'origami-next-fold)
         (define-key map [?p] 'origami-previous-fold)
         (define-key map [?o] 'origami-forward-toggle-node)
         map)
       t)))
  (defun transient/origami-previous-fold (buffer point)
    "Navigate to the previous origami fold, enabling pressing single
key for subsequent movements."
    (interactive (list (current-buffer) (point)))
    (let ((echo-keystrokes nil))
      (origami-previous-fold buffer point)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map [?n] 'origami-next-fold)
         (define-key map [?p] 'origami-previous-fold)
         (define-key map [?o] 'origami-forward-toggle-node)
         map)
       t)))
  (define-key origami-mode-map (kbd "C-c o p") 'transient/origami-previous-fold)
  (define-key origami-mode-map (kbd "C-c o n") 'transient/origami-next-fold)

  (defun my/origami-recenter-a (&rest _)
    "Recenter point to the middle of screen."
    (recenter-top-bottom))
  (advice-add #'origami-show-only-node :after #'my/origami-recenter-a)
  )

(add-hook 'prog-mode-hook 'origami-mode)

(provide 'init-fold)

;;; init-fold.el ends here
