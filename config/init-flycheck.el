;;; init-flycheck.el --- Configurations for flycheck	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for flycheck
;; --------------------------------------

;;; Code:

(require 'init-macros)

(use-package flymake
  :ensure t
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-mode-line-lighter "")
  (def-transient-commands flymake-mode-map "C-c f"
    ("n" . flymake-goto-next-error)
    ("p" . flymake-goto-prev-error))

  (define-key flymake-mode-map (kbd "C-c f l") 'flymake-show-buffer-diagnostics)
  (define-advice flymake-show-buffer-diagnostics (:after ())
    (select-window (get-buffer-window (flymake--diagnostics-buffer-name))))
  (define-advice flymake-goto-diagnostic (:after (pos))
    (quit-window nil (get-buffer-window (flymake--diagnostics-buffer-name))))

  (if (display-graphic-p)
      (progn
        (setq flymake-fringe-indicator-position 'right-fringe)
        (define-fringe-bitmap 'flymake-double-left-arrow
          [#b00011011
           #b00110110
           #b01101100
           #b11011000
           #b01101100
           #b00110110
           #b00011011])
        (setq flymake-error-bitmap '(flymake-double-left-arrow compilation-error))
        (setq flymake-warning-bitmap '(flymake-double-left-arrow compilation-warning))
        (setq flymake-note-bitmap '(flymake-double-left-arrow compilation-info)))
    (setq flymake-margin-indicator-position 'right-margin)
    (setq flymake-margin-indicators-string '((error "«" compilation-error)
                                             (warning "«" compilation-warning)
                                             (note "«" compilation-info)))))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
