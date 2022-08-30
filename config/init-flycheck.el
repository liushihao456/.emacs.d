;;; init-flycheck.el --- Configurations for flycheck	-*- lexical-binding: t -*-

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
;; Configurations for flycheck
;; --------------------------------------

;;; Code:

(add-hook 'prog-mode-hook 'flycheck-mode)
(with-eval-after-load 'flycheck
  (defun transient/flycheck-next-error ()
    "Navigate to the next flycheck error, enabling pressing single key for
subsequent movements."
    (interactive)
    (let ((echo-keystrokes nil))
      (flycheck-next-error)
      (message "Goto flycheck error: [n]ext [p]revious")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map [?n] 'flycheck-next-error)
         (define-key map [?p] 'flycheck-previous-error)
         map)
       t)))
  (defun transient/flycheck-previous-error ()
    "Navigate to the previous flycheck error, enabling pressing single key for
subsequent movements."
    (interactive)
    (let ((echo-keystrokes nil))
      (flycheck-previous-error)
      (message "Goto flycheck error: [n]ext [p]revious")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map [?n] 'flycheck-next-error)
         (define-key map [?p] 'flycheck-previous-error)
         map)
       t)))
  (define-key flycheck-mode-map (kbd "C-c f p") 'transient/flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "C-c f n") 'transient/flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-c f l") 'flycheck-list-errors)

  (setq-default flycheck-emacs-lisp-load-path 'inherit)

  (set-face-background 'flycheck-warning "unspecified-bg")
  (set-face-background 'flycheck-error "unspecified-bg")
  (set-face-background 'flycheck-info "unspecified-bg"))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
