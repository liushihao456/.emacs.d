;;; init-diffhl.el --- Diff-hl configuration	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Diff-hl configuration.
;; --------------------------------------

;;; Code:

(require 'init-macros)

(setq diff-hl-command-prefix (kbd "C-c v"))
(with-eval-after-load 'diff-hl
  (set-face-background 'diff-hl-change (face-background 'default))
  (set-face-background 'diff-hl-insert (face-background 'default))
  (set-face-background 'diff-hl-delete (face-background 'default))
  (defun my/diff-hl-define-bitmaps (&rest _)
    (define-fringe-bitmap 'diff-hl-bmp-middle [#b00011000] nil nil '(center repeated))
    (define-fringe-bitmap 'diff-hl-bmp-delete [#b11110000
                                               #b11100000
                                               #b11000000
                                               #b10000000] nil nil 'top))

  (advice-add #'diff-hl-define-bitmaps :override #'my/diff-hl-define-bitmaps)
  (defun my/diff-hl-type-face-fn (type _pos)
    (intern (format "diff-hl-%s" type)))
  (defun my/diff-hl-type-at-pos-fn (type _pos)
    (if (eq type 'delete)
        'diff-hl-bmp-delete
      'diff-hl-bmp-middle))
  (advice-add #'diff-hl-fringe-bmp-from-pos  :override #'my/diff-hl-type-at-pos-fn)
  (advice-add #'diff-hl-fringe-bmp-from-type :override #'my/diff-hl-type-at-pos-fn)
  (setq diff-hl-draw-borders nil)
  (with-eval-after-load 'flycheck
    (setq flycheck-indication-mode 'right-fringe)
    ;; Let the arrow point left
    (when (fboundp 'define-fringe-bitmap) ;; #ifdef HAVE_WINDOW_SYSTEM
      (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
        flycheck-fringe-bitmap-double-left-arrow)
      (define-fringe-bitmap
        'flycheck-fringe-bitmap-double-arrow-hi-res
        flycheck-fringe-bitmap-double-left-arrow-hi-res
        nil 16)))

  (def-transient-commands diff-hl-mode-map diff-hl-command-prefix
    ("n" . diff-hl-next-hunk)
    ("p" . diff-hl-previous-hunk)
    ("r" . diff-hl-revert-hunk)))

(global-diff-hl-mode)
(unless (display-graphic-p) (diff-hl-margin-mode))
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'vc-dir-mode-hook 'diff-hl-dir-mode)

(provide 'init-diffhl)

;;; init-diffhl.el ends here
