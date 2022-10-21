;;; init-lsp.el --- Configurations of LSP	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for Language Server Protocal support.
;; --------------------------------------

;;; Code:

(setq read-process-output-max (* 1024 1024))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c l k") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c l t") 'lsp-find-type-definition)
  (define-key lsp-mode-map (kbd "C-c l e") 'lsp-find-declaration)
  (define-key lsp-mode-map (kbd "C-c l o") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c l r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c l f") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c l m") 'lsp-organize-imports)
  (define-key lsp-mode-map (kbd "C-c l x") 'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c l M-s") 'lsp-describe-session)
  (define-key lsp-mode-map (kbd "C-c l M-r") 'lsp-workspace-restart)
  (define-key lsp-mode-map (kbd "C-c l S") 'lsp-shutdown-workspace)
  (define-key lsp-mode-map (kbd "C-c l a") 'xref-find-apropos)
  (define-key lsp-mode-map (kbd "C-c l i") 'consult-lsp-symbols)
  (define-key lsp-mode-map (kbd "C-c l d") 'consult-lsp-diagnostics)

  (setq lsp-before-save-edits nil)
  (setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-verbosity=off"))
  ;; (setq lsp-completion-show-detail nil)
  ;; (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-semantic-tokens-enable t)
  (setq lsp-headerline-breadcrumb-enable t)
  ;; (setq lsp-idle-delay 0.5)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-enable-imenu nil)

  ;; (setq lsp-log-io t)
  )

(with-eval-after-load 'lsp-ui
  (define-key lsp-mode-ui-map [remap xref-find-definitions] 'lsp-ui-peek-find-references)
  (define-key lsp-mode-ui-map [remap xref-find-references] 'lsp-ui-peek-find-references)
  (define-key lsp-mode-ui-map [remap lsp-describe-thing-at-point] 'lsp-ui-doc-glance)
  (setq lsp-ui-sideline-enable nil))

(provide 'init-lsp)

;;; init-lsp.el ends here
