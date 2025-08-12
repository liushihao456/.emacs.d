;;; init-modeline.el --- Configurations for modeline	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for modeline.
;; --------------------------------------

;;; Code:

;; Delight modeline lighters
(use-package delight
  :ensure t
  :init
  (delight '((eldoc-mode nil "eldoc")
             (emacs-lisp-mode "Elisp" :major)
             (which-key-mode nil "which-key")
             (abbrev-mode nil "abbrev")
             (visual-line-mode nil "simple")
             (company-mode nil "company")
             (company-box-mode nil "company-box")
             (yas-minor-mode nil "yasnippet")
             (auto-revert-mode nil "autorevert")
             (hi-lock-mode nil "hi-lock")
             (auto-fill-function nil "simple")
             (emmet-mode nil "emmet-mode")
             (anzu-mode nil "anzu")
             (isearch-mode nil "isearch")
             (tree-sitter-mode nil "tree-sitter")
             (mini-modeline-mode nil "mini-modeline")
             (hs-minor-mode nil "hideshow")
             (lsp-lens-mode nil "lsp-lens")
             (evil-commentary-mode nil "evil-commentary")
             (org-indent-mode nil "org-indent")
             (copilot-mode nil "copilot")
             (sideline-mode nil "sideline"))))

;; cc-mode.el assumes that `mode-name’ is always a string (which was true in
;; Emacs 22 and earlier), while delight.el makes use of the fact that
;; `mode-name’ can (since Emacs 23) contain any mode-line construct. The two are
;; therefore incompatible. See https://www.emacswiki.org/emacs/DelightedModes.
(advice-add 'c-update-modeline :override #'ignore)

;; (setcar mode-line-position
;;         '(:eval (format "%3d%%" (/ (window-start) 0.01 (point-max)))))

(defun my/buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))

(defun my/buffer-file-icon-mode-line ()
  "Render icon for current buffer file in the mode line."
  (if-let* ((buffer-file buffer-file-name)
            (file (file-name-nondirectory buffer-file-name)))
      (with-selected-window (minibuffer-window)
        (cond ((string-match-p "\\/$" file)
               (nerd-svg-icons-icon-for-dir file))
              (t
               (nerd-svg-icons-icon-for-file file))))
    (let ((mode major-mode))
      (with-selected-window (minibuffer-window)
        (nerd-svg-icons-icon-for-mode mode)))))

(defun my/vc-mode-line ()
  "Render version control information in the mode line."
  (cond (vc-mode (format "[%s]" (substring vc-mode 1)))
        (t nil)))

(defun my/evil-mode-line ()
  "Render version control information in the mode line."
  (cond (evil-mode (evil-generate-mode-line-tag evil-state))
        (t nil)))

(defun my/row-col-mode-line ()
  "Render row and col information in the mode line."
  "[%3l:%2c]")

;; (defun my/mode-line-render (left middle right)
;;   "Return a string of `window-total-width' length containing LEFT,
;; MIDDLE, and RIGHT aligned respectively."
;;   (let* ((additional-items (cl-subseq
;;                            mode-line-format 0 (1- (length mode-line-format))))
;;          (additional-itmes-width (length
;;                                  (format-mode-line additional-items)))
;;          (total-width (window-total-width))
;;          (half-middle-width (/ (length middle) 2 ))
;;          (total-space (- total-width
;;                          2
;;                          additional-itmes-width
;;                          (length left)
;;                          (length middle)
;;                          (length right)))
;;          (space1 (- (/ total-width 2)
;;                     additional-itmes-width
;;                     (length left)
;;                     half-middle-width))
;;          (space1 (max 0 space1))
;;          (space2 (- total-space space1))
;;          (space2 (max 0 space2)))
;;     (format " %s%s%s%s%s "
;;             left
;;             (make-string space1 ?\s)
;;             middle
;;             (make-string space2 ?\s)
;;             right)))

;; (defvar my/mode-line-left-segment
;;   (list "%e"
;;         '(:eval (cond (buffer-read-only "%*")
;;                       ((buffer-modified-p) "*")
;;                       (t "-")))
;;         " "
;;         "["
;;         mode-line-percent-position
;;         "]"
;;         " "
;;         ;; '(:eval (my/buffer-file-icon-mode-line))
;;         ;; " "
;;         '(:eval (propertize "%b"
;;                             'face 'mode-line-buffer-id
;;                             'help-echo default-directory))
;;         " "))

;; (defvar my/mode-line-middle-segment
;;   (list '(:eval (list
;;                  (-remove
;;                   (lambda (x) (or (equal x "(") (equal x ")")))
;;                   mode-line-modes)))))

;; (defvar my/mode-line-right-segment
;;   (list " "
;;         '(:eval (my/row-col-mode-line))))

;; (setq-default mode-line-format
;;               '((:eval
;;                  (replace-regexp-in-string ; escape ``%''
;;                   "%" "%%"
;;                   (my/mode-line-render
;;                    (format-mode-line my/mode-line-left-segment)
;;                    (format-mode-line my/mode-line-middle-segment)
;;                    (format-mode-line my/mode-line-right-segment))))))

;; Combine mode line and minibuffer ------------------------------------------ ;

;; The emacs-mini-modeline package requires dash
(use-package dash
  :ensure t)

(use-package mini-modeline
  :load-path "packages/emacs-mini-modeline"
  :commands mini-modeline-mode
  :init
  (mini-modeline-mode)
  :config
  (setq mini-modeline-l-format
        (list "%e"
              " "
              '(:eval (cond (buffer-read-only "%*")
                            ((buffer-modified-p) "*")
                            (t "-")))
              " "
              "["
              mode-line-percent-position
              "]"
              " "
              '(:eval (my/buffer-file-icon-mode-line))
              " "
              '(:eval (propertize (if (eq major-mode 'treemacs-mode) "Treemacs" "%b")
                                  'face 'mode-line-buffer-id))
              " "))
  (setq mini-modeline-r-format
        (list " "
              '(:eval (list (-remove #'(lambda (x) (or (equal x "(") (equal x ")"))) mode-line-modes)))
              '(:eval (my/evil-mode-line))
              " "
              '(:eval (my/row-col-mode-line))
              " "))
  (setq mini-modeline-right-padding 1))

(provide 'init-modeline)

;;; init-modeline.el ends here
