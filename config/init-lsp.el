;;; init-lsp.el --- Configurations of LSP	-*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Configurations for Language Server Protocal support.
;; --------------------------------------

;;; Code:

(setq read-process-output-max (* 1024 1024))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c l d") 'lsp-describe-thing-at-point)
  ;; (define-key lsp-mode-map (kbd "C-c l D") 'lsp-ui-peek-find-definitions)
  ;; (define-key lsp-mode-map (kbd "C-c l R") 'lsp-ui-peek-find-references)
  (define-key lsp-mode-map (kbd "C-c l t") 'lsp-find-type-definition)
  (define-key lsp-mode-map (kbd "C-c l o") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c l r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c l f") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c l m") 'lsp-organize-imports)
  ;; (define-key lsp-mode-map (kbd "C-c l m") 'lsp-ui-imenu)
  (define-key lsp-mode-map (kbd "C-c l x") 'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c l M-s") 'lsp-describe-session)
  (define-key lsp-mode-map (kbd "C-c l M-r") 'lsp-workspace-restart)
  (define-key lsp-mode-map (kbd "C-c l S") 'lsp-shutdown-workspace)
  (define-key lsp-mode-map (kbd "C-c l a") 'xref-find-apropos)

  (setq lsp-before-save-edits nil)
  (setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-verbosity=off"))
  (setq lsp-completion-show-detail nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-idle-delay 0.5)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-update-mode 'line)

  ;; (setq lsp-log-io t)
  )

(with-eval-after-load 'lsp-ui
  (defun my/lsp-ui-doc--make-request (fun &rest args)
    "Wrapper around lsp-ui-doc--make-request that prevents showing doc when
typing or company is active."
    (if (and (not company-pseudo-tooltip-overlay)
             (not (eq this-command 'self-insert-command)))
        (funcall fun)
      (lsp-ui-doc--hide-frame)))
  (advice-add 'lsp-ui-doc--make-request :around #'my/lsp-ui-doc--make-request)

  (set-face-background 'lsp-ui-sideline-code-action "unspecified-bg")
  (set-face-background 'lsp-ui-sideline-symbol "unspecified-bg"))

(provide 'init-lsp)

;;; init-lsp.el ends here
