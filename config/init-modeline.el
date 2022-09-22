;;; init-modeline.el --- Configurations for modeline	-*- lexical-binding: t -*-

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
;; Configurations for modeline.
;; --------------------------------------

;;; Code:

;; ;; Powerline
;; ;; In terminal version of emacs, transparency must be set to 0 for the
;; ;; separators' color to match
;; (require 'powerline)
;; (powerline-center-theme)

;; Delight modeline lighters
(delight '((eldoc-mode nil "eldoc")
           (emacs-lisp-mode "Elisp" :major)
           (which-key-mode nil "which-key")
           (abbrev-mode nil "abbrev")
           (visual-line-mode nil "simple")
           (lsp-mode nil "lsp-mode")
           (company-mode nil "company")
           (company-box-mode nil "company-box")
           (yas-minor-mode nil "yasnippet")
           (auto-revert-mode nil "autorevert")
           (hi-lock-mode nil "hi-lock")
           (auto-fill-function nil "simple")
           (emmet-mode nil "emmet-mode")
           (anzu-mode nil "anzu")
           (tree-sitter-mode nil "tree-sitter")
           (flycheck-mode nil "flycheck")
           (copilot-mode nil "copilot")))
;; cc-mode.el assumes that `mode-name’ is always a string (which was true in
;; Emacs 22 and earlier), while delight.el makes use of the fact that
;; `mode-name’ can (since Emacs 23) contain any mode-line construct. The two are
;; therefore incompatible. See https://www.emacswiki.org/emacs/DelightedModes.
(advice-add 'c-update-modeline :override #'ignore)

;; Tidy mode line
(defun tidy-modeline--fill (reserve)
  "Return empty space leaving RESERVE space on the right.
Adapted from powerline.el."
  (let ((real-reserve (if (and window-system (eq 'right (get-scroll-bar-mode)))
                          (- reserve 3)
                        reserve)))
    (propertize
     " "
     'display `((space :align-to (- (+ right right-fringe right-margin) ,real-reserve))))))


(defun tidy-modeline--fill-center (reserve)
  "Return empty space leaving RESERVE space on the right in order to center a string.
Adapted from powerline.el."
  (propertize
   " "
   'display `((space :align-to (- (+ center (0.5 . right-margin)) ,reserve
                                  (0.5 . left-margin))))))

(defun buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))

(defun my/buffer-file-icon-mode-line ()
  "Render icon for current buffer file in the mode line."
  (if-let ((buffer-file buffer-file-name)
           (file (file-name-nondirectory buffer-file-name)))
      (cond ((string-match-p "\\/$" file)
             (icon-tools-icon-for-dir file 'icon-tools-lblue))
            (t
             (icon-tools-icon-for-file file 'icon-tools-lblue)))
    (icon-tools-icon-for-mode major-mode)))

(defun my/vc-mode-line ()
  "Render version control information in the mode line."
  (cond (vc-mode (format "[%s]" (substring vc-mode 1)))
        (t nil)))

(defun my/flycheck-mode-line ()
  "Render flycheck information in the mode line."
  (if (boundp 'flycheck-last-status-change)
      (pcase flycheck-last-status-change
        (`not-checked nil)
        (`no-checker (propertize " -" 'face 'warning))
        (`running (propertize " ?" 'face 'success))
        (`errored (propertize " !" 'face 'error))
        (`finished
         (when-let ((error-counts (flycheck-count-errors flycheck-current-errors)))
           (let ((no-errors (cdr (assq 'error error-counts)))
                 (no-warnings (cdr (assq 'warning error-counts))))
             (format "[%s/%s]"
                     (propertize (number-to-string (or no-errors 0)) 'face 'error)
                     (propertize (number-to-string (or no-warnings 0)) 'face 'warning)))))
             ;; (concat
             ;;    (when no-errors
             ;;      (propertize
             ;;       (format "%s%s " (icon-tools-icon-str "ban") no-errors)
             ;;       'face 'error))
             ;;    (when no-warnings
             ;;      (propertize
             ;;       (format "%s%s" (icon-tools-icon-str "warning") no-warnings)
             ;;       'face 'warning))))))
        (`interrupted " -")
        (`suspicious '(propertize " ?" 'face 'warning)))
    nil))

(defun my/row-col-mode-line ()
  "Render row and col information in the mode line."
  (let* ((row (format-mode-line (list (propertize "%l" 'help-echo "Line number"))))
         (col (format-mode-line (list (propertize "%c" 'help-echo "Column number"))))
         (row-col (format "[%s : %s] " row col))
         (encoding (format "[%s]" (buffer-encoding-abbrev)))
         (row-col-length (length row-col))
         (encoding-length (+ row-col-length 1 (length encoding))))
    (list
     (tidy-modeline--fill encoding-length)
     encoding
     (tidy-modeline--fill row-col-length)
     row-col
     )))

(setq-default mode-line-format
              (list "%e"
                    '(:eval (cond (buffer-read-only " %*")
                                  ((buffer-modified-p) " *")
                                  (t " -")))
                    '(:eval (propertize " [%P]" 'help-echo "Position in buffer"))
                    " "
                    ;; '(:eval (my/buffer-file-icon-mode-line))
                    ;; " "
                    '(:eval (propertize "%12b" 'face 'mode-line-buffer-id 'help-echo default-directory))
                    " "
                    ;; '(:eval (my/vc-mode-line))
                    ;; " "
                    '(:eval (my/flycheck-mode-line))
                    " "
                    '(:eval (let* ((modes (-remove #'(lambda (x) (or (equal x "(") (equal x ")"))) mode-line-modes)))
                              (list (tidy-modeline--fill-center (/ (length modes) 2)) modes)))
                    '(:eval (my/row-col-mode-line))))


(provide 'init-modeline)

;;; init-modeline.el ends here
