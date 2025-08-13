;;; init-flymake.el --- Configurations for flymake	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for flymake
;; --------------------------------------

;;; Code:

(require 'init-macros)

(use-package flymake
  :ensure t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-margin-indicators-string '((error "«" compilation-error)
                                      (warning "«" compilation-warning)
                                      (note "«" compilation-info)))
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
    (setq flymake-margin-indicator-position 'right-margin)))

(use-package sideline-flymake
  :vc (:url "https://github.com/emacs-sideline/sideline-flymake")
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'line)
  (setq sideline-backends-right '(sideline-flymake)))

(provide 'init-flymake)

;;; init-flymake.el ends here
