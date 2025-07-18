;;; init-elisp.el --- Configurations for Elisp	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for Elisp
;; --------------------------------------

;;; Code:

;; Copied from doom emacs: add extra highlight + better indentation ---------- ;

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

(use-package highlight-quoted
  :ensure t
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; Display variable value next to documentation in eldoc --------------------- ;

(defun +emacs-lisp-append-value-to-eldoc-a (fn sym)
  "Display variable value next to documentation in eldoc."
  (when-let* ((ret (funcall fn sym)))
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

;; Prettify symbols mode ----------------------------------------------------- ;

;; Disable it because it breaks indentation for lines after the lambda
;; (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(provide 'init-elisp)

;;; init-elisp.el ends here
