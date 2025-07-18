;;; init-company.el --- Configuration for company mode.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for company mode.
;; --------------------------------------

;;; Code:

(setq initial-major-mode #'fundamental-mode) 

(use-package company
  :ensure t
  :hook ((prog-mode LaTeX-mode org-mode inferior-python-mode) . company-mode)
  :config
  (setq company-backends '(company-capf))
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq completion-ignore-case t)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)

  ;; Enable yas tab jumping when company candidates are active
  (add-hook 'company-mode-hook
              (lambda ()
                ;; This hook is added by company when `company-mode' is enabled. It disables
                ;; `yas-next-field-or-maybe-expand' with TAB key even if TAB has been unset in
                ;; `company-active-map'
                (remove-hook 'yas-keymap-disable-hook 'company--active-p t)))

  ;; Yasnippet integration
  (use-package yasnippet
    :defer t
    :config
    (global-set-key (kbd "C-]") 'company-yasnippet)

    (define-key company-active-map [tab] nil)
    (define-key company-active-map (kbd "TAB") nil)

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
    (use-package lsp-mode
      :defer t
      :config
      (advice-add #'lsp--auto-configure :after #'my/company-enable-yas))))

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-scrollbar nil))

(use-package company-tip
  :load-path "packages/company-tip"
  :if (not window-system)
  :commands company-tip-mode
  :hook (company-mode . company-tip-mode))

(use-package company-flx
  :ensure t
  :hook (company-mode . company-flx-mode))

(provide 'init-company)

;;; init-company.el ends here
