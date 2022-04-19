;;; init-company.el --- Configuration for company mode.	-*- lexical-binding: t -*-

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
;; Configuration for company mode.
;; --------------------------------------

;;; Code:

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'LaTeX-mode-hook 'company-mode)
(add-hook 'org-mode-hook 'company-mode)
(add-hook 'inferior-python-mode-hook 'company-mode)
(with-eval-after-load 'company
  (setq company-backends '(company-capf company-files))
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;; (add-hook 'company-mode-hook 'company-box-mode)
  (add-hook 'company-mode-hook 'company-tip-mode)
  ;; (company-tip-mode t)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq-default company-frontends
        '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (setq company-idle-delay 0.1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)

  ;; Better sorting and filtering
  ;; Disable it because it gave worse sorting in lsp typescript completions
  (company-prescient-mode t)

  ;; Yasnippet integration
  (require 'yasnippet)
  (global-set-key (kbd "C-]") 'company-yasnippet)
  (defun my/smarter-yas-expand-next-field-complete ()
    "Try to `yas-expand' and `yas-next-field' at current cursor position.

If failed try to complete the common part with `company-complete-common'"
    (interactive)
    (if yas-minor-mode
        (let ((old-point (point))
              (old-tick (buffer-chars-modified-tick)))
          (yas-expand)
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (ignore-errors (yas-next-field))
            (when (and (eq old-point (point))
                       (eq old-tick (buffer-chars-modified-tick)))
              (company-complete-common))))
      (company-complete-common)))
  (define-key company-active-map [tab] 'my/smarter-yas-expand-next-field-complete)
  (define-key company-active-map (kbd "TAB") 'my/smarter-yas-expand-next-field-complete)

  (defun company-backend-with-yas (backend)
    "Add `yasnippet' to company backend."
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (defun my/company-enable-yas (&rest _)
    "Enable `yasnippet' in `company'."
    (setq company-backends (mapcar #'company-backend-with-yas company-backends)))
  ;; Enable in current backends
  (my/company-enable-yas)
  ;; Enable in `lsp-mode'
  (with-eval-after-load 'lsp-mode
    (advice-add #'lsp--auto-configure :after #'my/company-enable-yas))

  (defun my/company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
    "Enable yasnippet but disable it inline."
    (if (eq command 'prefix)
        (when-let ((prefix (funcall fun 'prefix)))
          (unless (memq (char-before (- (point) (length prefix)))
                        '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?` ?/))
            prefix))
      (progn
        (when (and arg
                   (not (get-text-property 0 'yas-annotation-patch arg)))
          (let* ((name (get-text-property 0 'yas-annotation arg))
                 (snip (format "-> %s (Snippet)" name))
                 (len (length arg)))
            (put-text-property 0 len 'yas-annotation snip arg)
            (put-text-property 0 len 'yas-annotation-patch t arg)))
        (funcall fun command arg))))
  (advice-add #'company-yasnippet :around #'my/company-yasnippet-disable-inline)

  ;; Copilot integration
  (require 'copilot)

  (defun my/yas-copilot-advice (fun &rest _)
    "Call copilot first when yasnippet is available."
    (if (bound-and-true-p copilot-mode)
        (or (copilot-accept-completion)
            (funcall fun))
      (funcall fun)))
  (advice-add #'yas-expand :around #'my/yas-copilot-advice)

  (defun my/copilot-tab-company ()
    (interactive)
    (or (copilot-accept-completion)
        (my/smarter-yas-expand-next-field-complete)))

  (delq 'company-preview-if-just-one-frontend company-frontends)
  (define-key company-active-map [tab] 'my/copilot-tab-company)
  (define-key company-active-map (kbd "TAB") 'my/copilot-tab-company)
  )

(provide 'init-company)

;;; init-company.el ends here
