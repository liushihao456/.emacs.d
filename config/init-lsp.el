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
  (define-key lsp-mode-map (kbd "C-c l k") 'lsp-describe-thing-at-point)
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
  (define-key lsp-mode-map (kbd "C-c l i") 'consult-lsp-symbols)
  (define-key lsp-mode-map (kbd "C-c l d") 'consult-lsp-diagnostics)

  (setq lsp-before-save-edits nil)
  (setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-verbosity=off"))
  (setq lsp-completion-show-detail nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-idle-delay 0.5)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-signature-render-documentation nil)
  ;; (setq lsp-ui-sideline-show-hover t)
  ;; (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-update-mode 'line)
  (setq lsp-enable-imenu nil)

  ;; (setq lsp-log-io t)
  )

(with-eval-after-load 'lsp-ui
  (defun my/lsp-ui-doc--make-request (fun &rest _)
    "Wrapper around lsp-ui-doc--make-request that prevents showing doc when
typing or company is active."
    (if (and (not company-pseudo-tooltip-overlay)
             (not (eq this-command 'self-insert-command)))
        (funcall fun)
      (lsp-ui-doc--hide-frame)))
  (advice-add 'lsp-ui-doc--make-request :around #'my/lsp-ui-doc--make-request)

  (set-face-background 'lsp-ui-doc-background "#272A36")
  (set-face-background 'lsp-ui-sideline-code-action 'unspecified)
  (set-face-background 'lsp-ui-sideline-symbol 'unspecified))

(add-hook 'emacs-lisp-mode-hook 'lsp-ui-mode)

;; Show eldoc in posframe ---------------------------------------------------- ;

(when (display-graphic-p)
  (defvar eldoc-posframe-buffer "*eldoc-posframe-buffer*"
    "The posframe buffer name use by eldoc-posframe.")

  (defvar eldoc-posframe-hide-posframe-hooks
    '(pre-command-hook post-command-hook focus-out-hook)
    "The hooks which should trigger automatic removal of the posframe.")

  (defvar eldoc-posframe-delay 0.2
    "Delay seconds to display `eldoc'.")

  (defvar-local eldoc-posframe--timer nil)

  (defun eldoc-posframe-hide-posframe ()
    "Hide messages currently being shown if any."
    (when eldoc-posframe--timer
      (cancel-timer eldoc-posframe--timer))

    (posframe-hide eldoc-posframe-buffer)
    (dolist (hook eldoc-posframe-hide-posframe-hooks)
      (remove-hook hook #'eldoc-posframe-hide-posframe t)))

  (defun eldoc-posframe-show-posframe (str &rest args)
    "Display STR with ARGS."
    (require 'posframe)
    (when eldoc-posframe--timer
      (cancel-timer eldoc-posframe--timer))

    (posframe-hide eldoc-posframe-buffer)
    (dolist (hook eldoc-posframe-hide-posframe-hooks)
      (add-hook hook #'eldoc-posframe-hide-posframe nil t))

    (setq eldoc-posframe--timer
          (run-with-idle-timer
           eldoc-posframe-delay nil
           (lambda ()
             (when str
               (posframe-show
                eldoc-posframe-buffer
                :string (apply #'format str args)
                :postion (point)
                :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
                :left-fringe 8
                :right-fringe 8
                :internal-border-width 1
                :internal-border-color (face-attribute 'font-lock-comment-face :foreground)
                :background-color (face-background 'tooltip)))))))

  (setq eldoc-message-function #'eldoc-posframe-show-posframe))

(provide 'init-lsp)

;;; init-lsp.el ends here
