;;; init-wgrep.el --- Configurations for wgrep	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for wgrep.
;; --------------------------------------

;;; Code:

;; Deadgrep
(global-set-key (kbd "C-c s") 'deadgrep)

(with-eval-after-load 'wgrep
  (setq wgrep-auto-save-buffer t))

;; wgrep-deadgrep
;; C-c C-p to enable editing in the grep result buffer
;; C-c C-e to apply the changes
(add-hook 'deadgrep-finished-hook 'wgrep-deadgrep-setup)
;; (define-key deadgrep-mode-map "C-c C-s" 'wgrep-save-all-buffers)

(provide 'init-wgrep)

;;; init-wgrep.el ends here
