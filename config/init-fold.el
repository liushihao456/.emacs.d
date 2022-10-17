;;; init-fold.el --- Code/text folding	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Folding configuration.
;; --------------------------------------

;;; Code:

(defun hs-hide-leafs-recursive (minp maxp)
  "Hide blocks below point that do not contain further blocks in
    region (MINP MAXP)."
  (when (hs-find-block-beginning)
    (setq minp (1+ (point)))
    (funcall hs-forward-sexp-func 1)
    (setq maxp (1- (point))))
  (unless hs-allow-nesting
    (hs-discard-overlays minp maxp))
  (goto-char minp)
  (let ((leaf t))
    (while (progn
             (forward-comment (buffer-size))
             (and (< (point) maxp)
                  (re-search-forward hs-block-start-regexp maxp t)))
      (setq pos (match-beginning hs-block-start-mdata-select))
      (if (hs-hide-leafs-recursive minp maxp)
          (save-excursion
            (goto-char pos)
            (hs-hide-block-at-point t)))
      (setq leaf nil))
    (goto-char maxp)
    leaf))

(defun hs-hide-leafs ()
  "Hide all blocks in the buffer that do not contain subordinate
    blocks.  The hook `hs-hide-hook' is run; see `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (message "Hiding blocks ...")
     (save-excursion
       (goto-char (point-min))
       (hs-hide-leafs-recursive (point-min) (point-max)))
     (message "Hiding blocks ... done"))
   (run-hooks 'hs-hide-hook)))

(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map (kbd "C-c o l") 'hs-hide-level)
  (define-key hs-minor-mode-map (kbd "C-c o o") 'hs-toggle-hiding)
  (define-key hs-minor-mode-map (kbd "C-c o a") 'hs-show-all)
  (define-key hs-minor-mode-map (kbd "C-c o f") 'hs-hide-leafs))

(add-hook 'prog-mode-hook 'hs-minor-mode)

(provide 'init-fold)

;;; init-fold.el ends here
