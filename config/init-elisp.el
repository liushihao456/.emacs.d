;;; init-elisp.el --- Configurations for Elisp	-*- lexical-binding: t -*-

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
;; Configurations for Elisp
;; --------------------------------------

;;; Code:

;; Copied from doom emacs ---------------------------------------------------- ;

(defvar +emacs-lisp--face nil)

(defun +emacs-lisp-highlight-vars-and-faces (end)
  "Match defined variables and functions.
Functions are differentiated into special forms, built-in functions and
library/userland functions"
  (catch 'matcher
    (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
      (let ((ppss (save-excursion (syntax-ppss))))
        (cond ((nth 3 ppss)  ; strings
               (search-forward "\"" end t))
              ((nth 4 ppss)  ; comments
               (forward-line +1))
              ((let ((symbol (intern-soft (match-string-no-properties 0))))
                 (and (cond ((null symbol) nil)
                            ((eq symbol t) nil)
                            ((keywordp symbol) nil)
                            ((special-variable-p symbol)
                             (setq +emacs-lisp--face 'font-lock-variable-name-face))
                            ((and (fboundp symbol)
                                  (eq (char-before (match-beginning 0)) ?\()
                                  (not (memq (char-before (1- (match-beginning 0)))
                                             (list ?\' ?\`))))
                             (let ((unaliased (indirect-function symbol)))
                               (unless (or (macrop unaliased)
                                           (special-form-p unaliased))
                                 (let (unadvised)
                                   (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                   (setq unaliased (indirect-function unadvised)))))
                                   unaliased)
                                 (setq +emacs-lisp--face
                                       (if (subrp unaliased)
                                           'font-lock-constant-face
                                         'font-lock-function-name-face))))))
                      (throw 'matcher t)))))))
    nil))

(with-eval-after-load 'elisp-mode
  ;; highlight defined, special variables & functions
  (font-lock-add-keywords
   'emacs-lisp-mode
   '((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face))))

;; Highlight quoted symbols in elisp
(add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)

;; Display variable value next to documentation in eldoc.
(defun +emacs-lisp-append-value-to-eldoc-a (fn sym)
  "Display variable value next to documentation in eldoc."
  (when-let (ret (funcall fn sym))
    (if (boundp sym)
        (concat ret " "
                (let* ((truncated " [...]")
                       (print-escape-newlines t)
                       (str (symbol-value sym))
                       (str (prin1-to-string str))
                       (limit (- (frame-width) (length ret) (length truncated) 1)))
                  (format (format "%%0.%ds%%s" (max limit 0))
                          (propertize str 'face 'warning)
                          (if (< (length str) limit) "" truncated))))
      ret)))
(advice-add #'elisp-get-var-docstring :around #'+emacs-lisp-append-value-to-eldoc-a)

(provide 'init-elisp)

;;; init-elisp.el ends here
