;;; init-treemacs.el --- Treemacs configuration	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Treemacs configuration.
;; --------------------------------------

;;; Code:

(global-set-key (kbd "C-c e") 'treemacs-select-window)
(add-hook 'treemacs-mode-hook (lambda () (setq-local scroll-margin 0)))
(with-eval-after-load 'treemacs
  (treemacs-follow-mode)
  (treemacs-project-follow-mode)
  (setq treemacs--project-follow-delay 0.1)
  (setq treemacs-file-follow-delay 0.1)
  (setq treemacs-project-follow-cleanup t)
  (setq treemacs-follow-after-init t)
  ;; (setq treemacs-width 30)
  (setq treemacs-indentation 1)
  (setq treemacs-is-never-other-window t)

  (defadvice treemacs-visit-node-default (after treemacs-extra-wide-toggle-off activate)
    "Restore Treemacs buffer if it's in extr-wide state."
    (if (get 'treemacs-extra-wide-toggle :toggle-on)
        (with-selected-window (treemacs-get-local-window)
          (treemacs--set-width treemacs-width)
          (put 'treemacs-extra-wide-toggle :toggle-on nil)
          (treemacs-log "Switched to normal width display"))))
  (defadvice treemacs-quit (after treemacs-quit-reset-extra-wide activate)
    (put 'treemacs-extra-wide-toggle :toggle-on nil)
    (treemacs-log "Switched to normal width display"))

  (defun treemacs-visit-node-other-window (&optional arg)
    "Open current node in other window.
If the node is already opened in some other window then that
window will be selected instead.  Stay in the current window with
a single prefix argument ARG, or close the treemacs window with a
double prefix argument."
    (interactive "P")
    (treemacs--execute-button-action
     :file-action (find-file (treemacs-safe-button-get btn :path))
     :dir-action (dired (treemacs-safe-button-get btn :path))
     :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
     :tag-action (treemacs--goto-tag btn)
     :window-arg arg
     :ensure-window-split t
     :window  (progn (other-window 1) (get-buffer-window))
     :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))
  (setq treemacs-default-visit-action #'treemacs-visit-node-other-window)

  (defun my/treemacs-ignore-file-predicate (file _)
    (or (string= file ".gitignore")
        (string-suffix-p ".pyc" file)
        (string= file "__pycache__")
        (string-prefix-p ".cache" file)))
  (push #'my/treemacs-ignore-file-predicate treemacs-ignored-file-predicates)

  ;; icon-tools-treemacs-icons displays svg icons in GUI and nerd font icons in TUI.
  (icon-tools-treemacs-icons-config))

(provide 'init-treemacs)

;;; init-treemacs.el ends here
