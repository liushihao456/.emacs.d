;;; init-theme.el --- Configurations for theme	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for theme.
;; --------------------------------------

;;; Code:

(with-eval-after-load 'base16-theme
  (setq base16-theme-256-color-source 'base16-shell))

(with-eval-after-load 'solarized
  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)
  ;; Use less bolding
  (setq solarized-use-less-bold t)
  ;; Use more italics
  (setq solarized-use-more-italic t)
  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  (setq solarized-emphasize-indicators nil)
  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil))

(defun load-base16-theme (&optional theme)
  "Load base16 theme.

If called with the optional arg THEME, then THEME is loaded.
THEME could be a string or a symbol.

Otherwise tries to load the theme in ``~/.vimrc_background''
which base16-shell produces."
  (cond
   (theme
      (cond ((symbolp theme)
             (load-theme theme t))
            ((stringp theme)
             (load-theme (intern theme) t)))
      (message "Loaded theme %s as set in init-theme.el." theme))
   ((file-exists-p "~/.vimrc_background")
        (with-temp-buffer
          (insert-file-contents "~/.vimrc_background")
          (save-match-data
            (let ((theme1 (buffer-substring-no-properties
                           (search-forward "colorscheme ") (point-at-eol))))
              (message "Loaded theme %s from .vimrc_background file." theme1)
              (load-theme (intern theme1) t)))))
   (t
    (message "No theme is specified and no .vimrc_background file found."))))

(if (and (display-graphic-p) local-config-theme)
    (progn
      (message "Loaded theme %s from init-local-config.el" local-config-theme)
      (load-theme local-config-theme t))
  (load-base16-theme))

;; (setq frame-background-mode 'dark)
(unless (display-graphic-p)
  (set-face-background 'default 'unspecified))

(when (display-graphic-p)
  (with-eval-after-load 'modus-themes
    (set-face-background 'modus-themes-fringe-red 'unspecified)
    (set-face-background 'modus-themes-fringe-green 'unspecified)
    (set-face-background 'modus-themes-fringe-yellow 'unspecified)
    (set-face-background 'modus-themes-fringe-blue 'unspecified)
    (set-face-background 'modus-themes-fringe-magenta 'unspecified)
    (set-face-background 'modus-themes-fringe-cyan 'unspecified))
  (set-face-background 'fringe 'unspecified))

(provide 'init-theme)

;;; init-theme.el ends here
