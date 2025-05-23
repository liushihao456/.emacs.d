;;; init-company.el --- Configuration for company mode.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for company mode.
;; --------------------------------------

;;; Code:

(with-eval-after-load 'company
  (setq company-backends '(company-capf))
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  
  ;; Yasnippet integration
  (require 'yasnippet)
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
  (with-eval-after-load 'lsp-mode
    (advice-add #'lsp--auto-configure :after #'my/company-enable-yas))

  ;; (defun my/company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
  ;;   "Enable yasnippet but disable it inline."
  ;;   (if (eq command 'prefix)
  ;;       (when-let ((prefix (funcall fun 'prefix)))
  ;;         (unless (memq (char-before (- (point) (length prefix)))
  ;;                       '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?` ?/))
  ;;           prefix))
  ;;     (progn
  ;;       (when (and arg
  ;;                  (not (get-text-property 0 'yas-annotation-patch arg)))
  ;;         (let* ((name (get-text-property 0 'yas-annotation arg))
  ;;                (snip (format "-> %s (Snippet)" name))
  ;;                (len (length arg)))
  ;;           (put-text-property 0 len 'yas-annotation snip arg)
  ;;           (put-text-property 0 len 'yas-annotation-patch t arg)))
  ;;       (funcall fun command arg))))
  ;; (advice-add #'company-yasnippet :around #'my/company-yasnippet-disable-inline)
  )

(add-hook 'company-mode-hook
          (lambda ()
            ;; This hook is added by company when `company-mode' is enabled. It disables
            ;; `yas-next-field-or-maybe-expand' with TAB key even if TAB has been unset in
            ;; `company-active-map'
            (remove-hook 'yas-keymap-disable-hook 'company--active-p t)))

(with-eval-after-load 'company-box
  (setq company-box-scrollbar nil))

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'LaTeX-mode-hook 'company-mode)
(add-hook 'org-mode-hook 'company-mode)
(add-hook 'inferior-python-mode-hook 'company-mode)

(add-hook 'company-mode-hook 'company-flx-mode)
(if (display-graphic-p)
    (add-hook 'company-mode-hook 'company-box-mode)
  (add-hook 'company-mode-hook 'company-tip-mode))

(provide 'init-company)

;;; init-company.el ends here
