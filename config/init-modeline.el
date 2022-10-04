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

(defun my/buffer-encoding-abbrev ()
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
  (format "[%s : %s]"
          (format-mode-line (list (propertize "%l" 'help-echo "Line number")))
          (format-mode-line (list (propertize "%c" 'help-echo "Column number")))))

(defun my/mode-line-render (left middle right)
  "Return a string of `window-total-width' length containing LEFT,
MIDDLE, and RIGHT aligned respectively."
  (let* ((total-width (window-total-width))
         (half-middle-width (/ (length middle) 2 ))
         (total-space (- total-width 3 (length left) (length middle) (length right)))
         (space1 (- (/ total-width 2) 1 (length left) half-middle-width))
         (space1 (max 0 space1))
         (space2 (- total-space space1))
         (space2 (max 0 space2)))
    (format " %s%s %s%s%s "
            left
            (make-string space1 ?\s)
            middle
            (make-string space2 ?\s)
            right)))

(defvar my/mode-line-left-segment
  (list "%e"
        '(:eval (cond (buffer-read-only "%*")
                      ((buffer-modified-p) "*")
                      (t "-")))
        " "
        '(:eval (propertize "[%P]"
                            'help-echo "Position in buffer"))
        " "
        ;; '(:eval (my/buffer-file-icon-mode-line))
        ;; " "
        '(:eval (propertize "%12b"
                            'face 'mode-line-buffer-id
                            'help-echo default-directory))))

(defvar my/mode-line-middle-segment
  (list '(:eval (list
                 (-remove
                  (lambda (x) (or (equal x "(") (equal x ")")))
                  mode-line-modes)))))

(defvar my/mode-line-right-segment
  (list '(:eval (my/flycheck-mode-line))
        " "
        '(:eval (my/row-col-mode-line))))

(setq-default mode-line-format
              '((:eval
                 (replace-regexp-in-string ; escape ``%''
                  "%" "%%"
                  (my/mode-line-render
                   (format-mode-line my/mode-line-left-segment)
                   (format-mode-line my/mode-line-middle-segment)
                   (format-mode-line my/mode-line-right-segment))))))

(provide 'init-modeline)

;;; init-modeline.el ends here
