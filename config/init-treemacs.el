;;; init-treemacs.el --- Treemacs configuration	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Treemacs configuration.
;; --------------------------------------

;;; Code:

(global-set-key (kbd "C-c e e") 'treemacs-select-window)
(add-hook 'treemacs-mode-hook (lambda () (setq-local scroll-margin 0)))
(with-eval-after-load 'treemacs
  (defun my/treemacs-kill-buffer ()
    "Kill treemacs buffer even when not in treemacs window."
    (interactive)
    (let ((visibility (treemacs-current-visibility)))
      (if (eq visibility 'none)
          (treemacs-log-failure "No treemacs buffer alive.")
        (if (eq visibility 'visible)
            (progn
              (unless treemacs--in-this-buffer (treemacs--select-visible-window))
              (treemacs-kill-buffer))
          (kill-buffer (treemacs-get-local-buffer))
          (run-hooks treemacs-kill-hook))
        (treemacs-log "Killed treemacs buffer."))))
  (global-set-key (kbd "C-c e k") 'my/treemacs-kill-buffer)

  (defun my/treemacs-quit ()
    "Quit treemacs window even when not in it."
    (interactive)
    (let ((visibility (treemacs-current-visibility)))
      (if (not (eq visibility 'visible))
          (treemacs-log-failure "Treemacs window not visible.")
        (unless treemacs--in-this-buffer (treemacs--select-visible-window))
        (treemacs-quit))))
  (global-set-key (kbd "C-c e q") 'my/treemacs-quit)

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
