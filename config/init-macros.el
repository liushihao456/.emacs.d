;;; init-macros.el --- Macro definitions	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file includes some macro definitions..
;; --------------------------------------

;;; Code:

(defmacro def-transient-keymap (&rest key-commands)
  `(set-transient-map
    (let ((map (make-sparse-keymap)))
      ,@(mapcar (lambda (kc) `(define-key map (kbd ,(car kc)) ',(cdr kc)))
                key-commands)
      map)
    t))

(defmacro def-transient-commands (keymap prefix-key &rest key-commands)
  `(progn
     ,@(mapcar (lambda (kc)
                 `(defun ,(intern (format "transient/%s" (cdr kc))) ()
                    (interactive)
                    (let ((echo-keystrokes nil))
                      (call-interactively #',(cdr kc))
                      (def-transient-keymap ,@key-commands))))
               key-commands)
     ,@(mapcar (lambda (kc)
                 `(define-key ,keymap (kbd (format "%s %s" ,prefix-key ,(car kc)))
                    ',(intern (format "transient/%s" (cdr kc)))))
               key-commands)))

(provide 'init-macros)

;;; init-macros.el ends here
