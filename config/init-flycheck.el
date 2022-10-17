;;; init-flycheck.el --- Configurations for flycheck	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for flycheck
;; --------------------------------------

;;; Code:

(add-hook 'prog-mode-hook 'flycheck-mode)
(with-eval-after-load 'flycheck
  (defun transient/flycheck-next-error ()
    "Navigate to the next flycheck error, enabling pressing single key for
subsequent movements."
    (interactive)
    (let ((echo-keystrokes nil))
      (flycheck-next-error)
      (message "Goto flycheck error: [n]ext [p]revious")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map [?n] 'flycheck-next-error)
         (define-key map [?p] 'flycheck-previous-error)
         map)
       t)))
  (defun transient/flycheck-previous-error ()
    "Navigate to the previous flycheck error, enabling pressing single key for
subsequent movements."
    (interactive)
    (let ((echo-keystrokes nil))
      (flycheck-previous-error)
      (message "Goto flycheck error: [n]ext [p]revious")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map [?n] 'flycheck-next-error)
         (define-key map [?p] 'flycheck-previous-error)
         map)
       t)))
  (define-key flycheck-mode-map (kbd "C-c f p") 'transient/flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "C-c f n") 'transient/flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-c f l") 'flycheck-list-errors)

  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (add-to-list 'display-buffer-alist
               `(,flycheck-error-list-buffer
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.33)))
  (defun flycheck-list-errors-a ()
    "Switch to flycheck error list buffer after creating it."
    (select-window (get-buffer-window flycheck-error-list-buffer)))
  (advice-add #'flycheck-list-errors :after #'flycheck-list-errors-a)
  (defun flycheck-goto-error-a ()
    "Close flycheck error list window after going to error."
    (when (eq this-command 'flycheck-error-list-goto-error)
      (quit-window nil (get-buffer-window flycheck-error-list-buffer))))
  (advice-add #'flycheck-error-list-goto-error :after #'flycheck-goto-error-a)
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when-let (w (get-buffer-window flycheck-error-list-buffer))
                (set-window-parameter w 'no-other-window t))))

  (set-face-background 'flycheck-warning 'unspecified)
  (set-face-background 'flycheck-error 'unspecified)
  (set-face-background 'flycheck-info 'unspecified))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
