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
  ;; (require 'company-tip)
  (company-tip-mode t)
  (setq company-dabbrev-downcase nil)
  (setq-default company-frontends
        '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)

  ;; Better sorting and filtering
  (company-prescient-mode t)

  ;; Yasnippet integration
  (require 'yasnippet)
  (global-set-key (kbd "C-]") 'company-yasnippet)
  (defun company-backend-with-yas (backend)
    "Add `yasnippet' to company backend."
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (defun my-company-enbale-yas (&rest _)
    "Enable `yasnippet' in `company'."
    (setq company-backends (mapcar #'company-backend-with-yas company-backends)))
  ;; Enable in current backends
  (my-company-enbale-yas)
  ;; Enable in `lsp-mode'
  (with-eval-after-load 'lsp-mode
    (advice-add #'lsp--auto-configure :after #'my-company-enbale-yas))

  (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
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
  (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline))

(provide 'init-company)

;;; init-company.el ends here
