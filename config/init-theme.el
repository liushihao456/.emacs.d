;;; init-theme.el --- Configurations for theme	-*- lexical-binding: t -*-

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

(defun load-local-config-theme ()
  "Load the theme specified in .emacs.d/local-config.el.

Return t if successfully loaded the theme."
  (let ((local-config-file
         (file-name-concat user-emacs-directory "local-config.el"))
        (loaded))
    (when (file-exists-p local-config-file)
      (require 'local-config local-config-file)
      (when theme-to-load
        (load-theme theme-to-load t)
        (setq loaded t)))
    (unless loaded
      (message "No theme loaded from .emacs.d/local-config.el file."))
    loaded))

(unless (and (display-graphic-p) (load-local-config-theme))
    (load-base16-theme))

(setq frame-background-mode 'dark)
(unless (display-graphic-p)
  (set-face-background 'default 'unspecified))

(provide 'init-theme)

;;; init-theme.el ends here
