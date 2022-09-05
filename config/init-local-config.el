;; This file is added to git but without tracking changes. It is meant to be
;; setup per machine locally in order to add extra decoupled configurations.

(defvar local-config-theme 'base16-dracula)

(defvar local-config-font "Operator Mono")
(defvar local-config-font-size "21")

(setq initial-frame-alist '((top . 1) (left . 1) (width . 120) (fullscreen . fullheight)))

(provide 'init-local-config)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
