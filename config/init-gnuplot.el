;;; init-gnuplot.el --- Configurations for gnuplot	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for gnuplot
;; --------------------------------------

;;; Code:

(add-to-list 'auto-mode-alist '("\\.gnuplot\\'" . gnuplot-mode))
(add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
(with-eval-after-load 'gnuplot-mode
  (defun gnuplot-epslatex-generate-eps ()
    "Generate eps output in an epslatex terminal in gnuplot using dvips."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "set \\(?:terminal\\|term\\) epslatex[^\n]+standalone" nil t)
        (when (search-forward-regexp "set output \"\\([^\"]+\\)\.tex\"")
          (let ((output-file (match-string 1))
                (buffer (and (if (get-buffer "*shell-output*") (kill-buffer "*shell-output*") t)
                             (get-buffer-create "*shell-output*"))))
            (gnuplot-run-buffer)
            (pop-to-buffer buffer)
            (help-mode)
            (setq-local buffer-read-only nil)
            (erase-buffer)
            (shell-command
             (concat "latex " output-file ".tex && dvips -E "
                     output-file ".dvi -o " output-file ".eps")
             t)
            (setq-local buffer-read-only t))))))
  (define-key gnuplot-mode-map (kbd "C-c C-e") 'gnuplot-epslatex-generate-eps))

(provide 'init-gnuplot)

;;; init-gnuplot.el ends here
