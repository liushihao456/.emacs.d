;;; zenburn-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2018 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Package-Version: 20200305.737
;; Version: 2.7-snapshot

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Zenburn for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme terminal-zenburn "The Zenburn color theme for terminal")

(defgroup terminal-zenburn-theme nil
  "Terminal-Zenburn theme."
  :group 'faces
  :prefix "terminal-zenburn-"
  :link '(url-link :tag "GitHub" "http://github.com/bbatsov/terminal-zenburn-emacs")
  :tag "Terminal-Zenburn theme")

;;;###autoload
(defcustom terminal-zenburn-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'terminal-zenburn-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defvar terminal-zenburn-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar terminal-zenburn-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled.")

(defvar terminal-zenburn-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled.")

(defcustom terminal-zenburn-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'terminal-zenburn-theme
  :package-version '(terminal-zenburn . "2.6"))

(defcustom terminal-zenburn-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'terminal-zenburn-theme
  :package-version '(terminal-zenburn . "2.6"))

(defcustom terminal-zenburn-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'terminal-zenburn-theme
  :package-version '(terminal-zenburn . "2.6"))

(defcustom terminal-zenburn-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'terminal-zenburn-theme
  :package-version '(terminal-zenburn . "2.6"))

(defcustom terminal-zenburn-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'terminal-zenburn-theme
  :package-version '(terminal-zenburn . "2.6"))

;;; Color Palette

(defvar terminal-zenburn-default-colors-alist
  '(("terminal-zenburn-fg-1"     . "#656555")
    ("terminal-zenburn-fg-05"    . "#989890")
    ("terminal-zenburn-fg"       . "#DCDCCC")
    ("terminal-zenburn-fg+1"     . "#FFFFEF")
    ("terminal-zenburn-fg+2"     . "#FFFFFD")
    ("terminal-zenburn-bg-2"     . "#000000")
    ("terminal-zenburn-bg-1"     . "#2B2B2B")
    ("terminal-zenburn-bg-08"    . "#303030")
    ("terminal-zenburn-bg-05"    . "#383838")
    ("terminal-zenburn-bg"       . "#3F3F3F")
    ("terminal-zenburn-bg+05"    . "#494949")
    ("terminal-zenburn-bg+1"     . "#4F4F4F")
    ("terminal-zenburn-bg+2"     . "#5F5F5F")
    ("terminal-zenburn-bg+3"     . "#6F6F6F")
    ("terminal-zenburn-red-6"    . "#6C3333")
    ("terminal-zenburn-red-5"    . "#7C4343")
    ("terminal-zenburn-red-4"    . "#8C5353")
    ("terminal-zenburn-red-3"    . "#9C6363")
    ("terminal-zenburn-red-2"    . "#AC7373")
    ("terminal-zenburn-red-1"    . "#BC8383")
    ("terminal-zenburn-red"      . "#CC9393")
    ("terminal-zenburn-red+1"    . "#DCA3A3")
    ("terminal-zenburn-red+2"    . "#ECB3B3")
    ("terminal-zenburn-orange"   . "#DFAF8F")
    ("terminal-zenburn-yellow-2" . "#D0BF8F")
    ("terminal-zenburn-yellow-1" . "#E0CF9F")
    ("terminal-zenburn-yellow"   . "#F0DFAF")
    ("terminal-zenburn-green-5"  . "#2F4F2F")
    ("terminal-zenburn-green-4"  . "#3F5F3F")
    ("terminal-zenburn-green-3"  . "#4F6F4F")
    ("terminal-zenburn-green-2"  . "#5F7F5F")
    ("terminal-zenburn-green-1"  . "#6F8F6F")
    ("terminal-zenburn-green"    . "#7F9F7F")
    ("terminal-zenburn-green+1"  . "#8FB28F")
    ("terminal-zenburn-green+2"  . "#9FC59F")
    ("terminal-zenburn-green+3"  . "#AFD8AF")
    ("terminal-zenburn-green+4"  . "#BFEBBF")
    ("terminal-zenburn-cyan"     . "#93E0E3")
    ("terminal-zenburn-blue+3"   . "#BDE0F3")
    ("terminal-zenburn-blue+2"   . "#ACE0E3")
    ("terminal-zenburn-blue+1"   . "#94BFF3")
    ("terminal-zenburn-blue"     . "#8CD0D3")
    ("terminal-zenburn-blue-1"   . "#7CB8BB")
    ("terminal-zenburn-blue-2"   . "#6CA0A3")
    ("terminal-zenburn-blue-3"   . "#5C888B")
    ("terminal-zenburn-blue-4"   . "#4C7073")
    ("terminal-zenburn-blue-5"   . "#366060")
    ("terminal-zenburn-magenta"  . "#DC8CC3"))
  "List of Terminal-Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro terminal-zenburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `terminal-zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append terminal-zenburn-default-colors-alist
                           terminal-zenburn-override-colors-alist))
         (z-variable-pitch (if terminal-zenburn-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(terminal-zenburn-with-color-variables
  (custom-theme-set-faces
   'terminal-zenburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,terminal-zenburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,terminal-zenburn-yellow-2 :underline t :weight normal))))
   ;; `(default ((t (:foreground ,terminal-zenburn-fg))))
   ;; `(default ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg))))
   `(cursor ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-fg+1))))
   `(widget-field ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg+3))))
   `(escape-glyph ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(fringe ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg+1))))
   `(header-line ((t (:foreground ,terminal-zenburn-yellow
                                  :background ,terminal-zenburn-bg-1
                                  :box (:line-width -1 :style released-button)
                                  :extend t))))
   `(highlight ((t (:background ,terminal-zenburn-bg-05))))
   `(success ((t (:foreground ,terminal-zenburn-green :weight bold))))
   `(warning ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(tooltip ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,terminal-zenburn-green))))
   `(compilation-error-face ((t (:foreground ,terminal-zenburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,terminal-zenburn-fg))))
   `(compilation-info-face ((t (:foreground ,terminal-zenburn-blue))))
   `(compilation-info ((t (:foreground ,terminal-zenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,terminal-zenburn-green))))
   `(compilation-line-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(compilation-line-number ((t (:foreground ,terminal-zenburn-yellow))))
   `(compilation-message-face ((t (:foreground ,terminal-zenburn-blue))))
   `(compilation-warning-face ((t (:foreground ,terminal-zenburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,terminal-zenburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,terminal-zenburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,terminal-zenburn-fg-1))))
;;;;; customize
   `(custom-variable-tag ((t (:foreground ,terminal-zenburn-blue :weight bold))))
   `(custom-group-tag ((t (:foreground ,terminal-zenburn-blue :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,terminal-zenburn-green+4))))
;;;;; display-fill-column-indicator
     `(fill-column-indicator ((,class :foreground ,terminal-zenburn-bg-05 :weight semilight)))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,terminal-zenburn-fg))))
   `(grep-error-face ((t (:foreground ,terminal-zenburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,terminal-zenburn-blue))))
   `(grep-match-face ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(match ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,terminal-zenburn-cyan    :foreground ,terminal-zenburn-bg-1))))
   `(hi-green   ((t (:background ,terminal-zenburn-green+4 :foreground ,terminal-zenburn-bg-1))))
   `(hi-pink    ((t (:background ,terminal-zenburn-magenta :foreground ,terminal-zenburn-bg-1))))
   `(hi-yellow  ((t (:background ,terminal-zenburn-yellow  :foreground ,terminal-zenburn-bg-1))))
   `(hi-blue-b  ((t (:foreground ,terminal-zenburn-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,terminal-zenburn-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,terminal-zenburn-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,terminal-zenburn-yellow-2 :weight bold :background ,terminal-zenburn-bg+2))))
   `(isearch-fail ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-red-4))))
   `(lazy-highlight ((t (:foreground ,terminal-zenburn-yellow-2 :weight bold :background ,terminal-zenburn-bg-05))))

   `(menu ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,terminal-zenburn-yellow))))
   `(mode-line
     ((,class (:foreground ,terminal-zenburn-green+1
                           :background ,terminal-zenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,terminal-zenburn-green-2
                      :background ,terminal-zenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,terminal-zenburn-bg-1 :extend t))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,terminal-zenburn-bg+2))))
   `(trailing-whitespace ((t (:background ,terminal-zenburn-red))))
   `(vertical-border ((t (:foreground ,terminal-zenburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,terminal-zenburn-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,terminal-zenburn-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,terminal-zenburn-green-2))))
   `(font-lock-constant-face ((t (:foreground ,terminal-zenburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,terminal-zenburn-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,terminal-zenburn-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,terminal-zenburn-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,terminal-zenburn-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,terminal-zenburn-red))))
   `(font-lock-type-face ((t (:foreground ,terminal-zenburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,terminal-zenburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,terminal-zenburn-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,terminal-zenburn-bg+3 :background ,terminal-zenburn-bg-05))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,terminal-zenburn-yellow-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,terminal-zenburn-fg))))
   `(newsticker-default-face ((t (:foreground ,terminal-zenburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,terminal-zenburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,terminal-zenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,terminal-zenburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,terminal-zenburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,terminal-zenburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,terminal-zenburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,terminal-zenburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,terminal-zenburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,terminal-zenburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,terminal-zenburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,terminal-zenburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,terminal-zenburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,terminal-zenburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,terminal-zenburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,terminal-zenburn-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(font-latex-italic-face ((t (:foreground ,terminal-zenburn-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,terminal-zenburn-orange))))
   `(font-latex-script-char-face ((t (:foreground ,terminal-zenburn-orange))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,terminal-zenburn-orange :background ,terminal-zenburn-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,terminal-zenburn-orange :background ,terminal-zenburn-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg-1))))
   `(company-tooltip-mouse ((t (:background ,terminal-zenburn-bg-1))))
   `(company-tooltip-common ((t (:foreground ,terminal-zenburn-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,terminal-zenburn-green+2))))
   `(company-scrollbar-fg ((t (:background ,terminal-zenburn-bg-1))))
   `(company-scrollbar-bg ((t (:background ,terminal-zenburn-bg+2))))
   `(company-preview ((t (:background ,terminal-zenburn-green+2))))
   `(company-preview-common ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg-1))))
;;;;; diff
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(diff-added          ((t (:background "#335533" :foreground ,terminal-zenburn-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,terminal-zenburn-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,terminal-zenburn-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,terminal-zenburn-green+4))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,terminal-zenburn-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,terminal-zenburn-red))))
   `(diff-header ((,class (:background ,terminal-zenburn-bg+2))
                  (t (:background ,terminal-zenburn-fg :foreground ,terminal-zenburn-bg))))
   `(diff-file-header
     ((,class (:background ,terminal-zenburn-bg+2 :foreground ,terminal-zenburn-fg :weight bold))
      (t (:background ,terminal-zenburn-fg :foreground ,terminal-zenburn-bg :weight bold))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-green-2))))
   `(ediff-current-diff-C ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-blue-5))))
   `(ediff-even-diff-A ((t (:background ,terminal-zenburn-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,terminal-zenburn-bg+1))))
   `(ediff-even-diff-B ((t (:background ,terminal-zenburn-bg+1))))
   `(ediff-even-diff-C ((t (:background ,terminal-zenburn-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,terminal-zenburn-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,terminal-zenburn-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,terminal-zenburn-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,terminal-zenburn-bg+2))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,terminal-zenburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,terminal-zenburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,terminal-zenburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,terminal-zenburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,terminal-zenburn-cyan :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-red-1) :inherit unspecified))
      (t (:foreground ,terminal-zenburn-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-yellow) :inherit unspecified))
      (t (:foreground ,terminal-zenburn-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-cyan) :inherit unspecified))
      (t (:foreground ,terminal-zenburn-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,terminal-zenburn-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,terminal-zenburn-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,terminal-zenburn-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,terminal-zenburn-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,terminal-zenburn-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-orange) :inherit unspecified))
      (t (:foreground ,terminal-zenburn-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-red) :inherit unspecified))
      (t (:foreground ,terminal-zenburn-red-1 :weight bold :underline t))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,terminal-zenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,terminal-zenburn-bg-05 :extend t)) ; old emacsen
              (t :weight bold)))
;;;;; info+
   `(info-command-ref-item ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-orange))))
   `(info-constant-ref-item ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-yellow))))
   `(info-function-ref-item ((t (:background ,terminal-zenburn-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-yellow))))
   `(info-menu ((t (:foreground ,terminal-zenburn-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,terminal-zenburn-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-blue+1))))
   `(info-user-option-ref-item ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-red))))
   `(info-variable-ref-item ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-orange))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,terminal-zenburn-green :background ,terminal-zenburn-bg))))
   `(ivy-current-match ((t (:foreground ,terminal-zenburn-yellow :weight bold :underline t))))
   `(ivy-cursor ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-fg))))
   `(ivy-match-required-face ((t (:foreground ,terminal-zenburn-red :background ,terminal-zenburn-bg))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,terminal-zenburn-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,terminal-zenburn-green-2))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,terminal-zenburn-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,terminal-zenburn-green+1))))
   `(ivy-remote ((t (:foreground ,terminal-zenburn-blue :background ,terminal-zenburn-bg))))
   `(ivy-subdir ((t (:foreground ,terminal-zenburn-yellow :background ,terminal-zenburn-bg))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,terminal-zenburn-orange))))
   `(js2-error ((t (:foreground ,terminal-zenburn-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,terminal-zenburn-green-2))))
   `(js2-jsdoc-type ((t (:foreground ,terminal-zenburn-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,terminal-zenburn-green+3))))
   `(js2-function-param ((t (:foreground, terminal-zenburn-orange))))
   `(js2-external-variable ((t (:foreground ,terminal-zenburn-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,terminal-zenburn-green-2))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,terminal-zenburn-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,terminal-zenburn-red-1))))
   `(js2-object-property ((t (:foreground ,terminal-zenburn-blue+1))))
   `(js2-magic-paren ((t (:foreground ,terminal-zenburn-blue-5))))
   `(js2-private-function-call ((t (:foreground ,terminal-zenburn-cyan))))
   `(js2-function-call ((t (:foreground ,terminal-zenburn-cyan))))
   `(js2-private-member ((t (:foreground ,terminal-zenburn-blue-1))))
   `(js2-keywords ((t (:foreground ,terminal-zenburn-magenta))))
;;;;; linum-mode
   `(linum ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,terminal-zenburn-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,terminal-zenburn-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,terminal-zenburn-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg))))
;;;;; magit
;;;;;; headings and diffs
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(magit-section-highlight           ((t (:background ,terminal-zenburn-bg+05))))
   `(magit-section-heading             ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,terminal-zenburn-bg+05 :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,terminal-zenburn-bg+05 :weight bold
                                                        :foreground ,terminal-zenburn-orange))))
   `(magit-diff-added                  ((t (:background ,terminal-zenburn-green-2))))
   `(magit-diff-added-highlight        ((t (:background ,terminal-zenburn-green))))
   `(magit-diff-removed                ((t (:background ,terminal-zenburn-red-4))))
   `(magit-diff-removed-highlight      ((t (:background ,terminal-zenburn-red-3))))
   `(magit-diff-hunk-heading           ((t (:background ,terminal-zenburn-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,terminal-zenburn-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,terminal-zenburn-bg+2
                                                        :foreground ,terminal-zenburn-orange))))
   `(magit-diff-lines-heading          ((t (:background ,terminal-zenburn-orange
                                                        :foreground ,terminal-zenburn-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,terminal-zenburn-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added              ((t (:foreground ,terminal-zenburn-green+4))))
   `(magit-diffstat-removed            ((t (:foreground ,terminal-zenburn-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,terminal-zenburn-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,terminal-zenburn-green-2 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,terminal-zenburn-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,terminal-zenburn-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,terminal-zenburn-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,terminal-zenburn-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,terminal-zenburn-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,terminal-zenburn-orange))))
   `(magit-log-date      ((t (:foreground ,terminal-zenburn-fg-1))))
   `(magit-log-graph     ((t (:foreground ,terminal-zenburn-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,terminal-zenburn-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,terminal-zenburn-green))))
   `(magit-sequence-part ((t (:foreground ,terminal-zenburn-yellow))))
   `(magit-sequence-head ((t (:foreground ,terminal-zenburn-blue))))
   `(magit-sequence-drop ((t (:foreground ,terminal-zenburn-red))))
   `(magit-sequence-done ((t (:foreground ,terminal-zenburn-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,terminal-zenburn-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,terminal-zenburn-green))))
   `(magit-bisect-skip ((t (:foreground ,terminal-zenburn-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,terminal-zenburn-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-blue-2))))
   `(magit-blame-hash    ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-blue-2))))
   `(magit-blame-name    ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-orange))))
   `(magit-blame-date    ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-orange))))
   `(magit-blame-summary ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,terminal-zenburn-bg+3))))
   `(magit-hash           ((t (:foreground ,terminal-zenburn-bg+3))))
   `(magit-tag            ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,terminal-zenburn-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,terminal-zenburn-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,terminal-zenburn-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,terminal-zenburn-blue   :weight bold))))
   `(magit-refname        ((t (:background ,terminal-zenburn-bg+2 :foreground ,terminal-zenburn-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,terminal-zenburn-bg+2 :foreground ,terminal-zenburn-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,terminal-zenburn-bg+2 :foreground ,terminal-zenburn-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,terminal-zenburn-green))))
   `(magit-signature-bad       ((t (:foreground ,terminal-zenburn-red))))
   `(magit-signature-untrusted ((t (:foreground ,terminal-zenburn-yellow))))
   `(magit-signature-expired   ((t (:foreground ,terminal-zenburn-orange))))
   `(magit-signature-revoked   ((t (:foreground ,terminal-zenburn-magenta))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,terminal-zenburn-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,terminal-zenburn-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,terminal-zenburn-green))))
   `(magit-reflog-amend        ((t (:foreground ,terminal-zenburn-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,terminal-zenburn-green))))
   `(magit-reflog-checkout     ((t (:foreground ,terminal-zenburn-blue))))
   `(magit-reflog-reset        ((t (:foreground ,terminal-zenburn-red))))
   `(magit-reflog-rebase       ((t (:foreground ,terminal-zenburn-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,terminal-zenburn-green))))
   `(magit-reflog-remote       ((t (:foreground ,terminal-zenburn-cyan))))
   `(magit-reflog-other        ((t (:foreground ,terminal-zenburn-cyan))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,terminal-zenburn-green+1))))
   `(message-header-other ((t (:foreground ,terminal-zenburn-green))))
   `(message-header-to ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,terminal-zenburn-green))))
   `(message-mml ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,terminal-zenburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,terminal-zenburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,terminal-zenburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,terminal-zenburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,terminal-zenburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,terminal-zenburn-green-2 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,terminal-zenburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,terminal-zenburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,terminal-zenburn-bg+3 :strike-through t))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,terminal-zenburn-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,terminal-zenburn-fg :weight bold))))
   `(org-block ((t (:background ,terminal-zenburn-bg+05 :extend t))))
   `(org-checkbox ((t (:background ,terminal-zenburn-bg+2 :foreground ,terminal-zenburn-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,terminal-zenburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,terminal-zenburn-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,terminal-zenburn-green+3))))
   `(org-formula ((t (:foreground ,terminal-zenburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,terminal-zenburn-green+3))))
   `(org-hide ((t (:foreground ,terminal-zenburn-bg))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-orange
                               ,@(when terminal-zenburn-scale-org-headlines
                                   (list :height terminal-zenburn-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-green+4
                               ,@(when terminal-zenburn-scale-org-headlines
                                   (list :height terminal-zenburn-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-blue-1
                               ,@(when terminal-zenburn-scale-org-headlines
                                   (list :height terminal-zenburn-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-yellow-2
                               ,@(when terminal-zenburn-scale-org-headlines
                                   (list :height terminal-zenburn-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-cyan))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-green+2))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-red-4))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-blue-4))))
   `(org-link ((t (:foreground ,terminal-zenburn-yellow-2 :underline t))))
   `(org-quote ((t (:background ,terminal-zenburn-bg+05 :extend t))))
   `(org-scheduled ((t (:foreground ,terminal-zenburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,terminal-zenburn-red))))
   `(org-scheduled-today ((t (:foreground ,terminal-zenburn-blue+1))))
   `(org-sexp-date ((t (:foreground ,terminal-zenburn-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,terminal-zenburn-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,terminal-zenburn-orange))))
   `(org-todo ((t (:weight bold :foreground ,terminal-zenburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,terminal-zenburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,terminal-zenburn-bg-1))))
   `(org-column-title ((t (:background ,terminal-zenburn-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-red-1))))
   `(org-ellipsis ((t (:foreground ,terminal-zenburn-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,terminal-zenburn-cyan :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-blue
                                      :weight bold
                                      ,@(when terminal-zenburn-scale-org-headlines
                                          (list :height terminal-zenburn-height-plus-4))))))
   `(org-document-info ((t (:foreground ,terminal-zenburn-blue))))
   `(org-habit-ready-face ((t :background ,terminal-zenburn-green)))
   `(org-habit-alert-face ((t :background ,terminal-zenburn-yellow-1 :foreground ,terminal-zenburn-bg)))
   `(org-habit-clear-face ((t :background ,terminal-zenburn-blue-3)))
   `(org-habit-overdue-face ((t :background ,terminal-zenburn-red-3)))
   `(org-habit-clear-future-face ((t :background ,terminal-zenburn-blue-4)))
   `(org-habit-ready-future-face ((t :background ,terminal-zenburn-green-2)))
   `(org-habit-alert-future-face ((t :background ,terminal-zenburn-yellow-2 :foreground ,terminal-zenburn-bg)))
   `(org-habit-overdue-future-face ((t :background ,terminal-zenburn-red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-orange
                             ,@(when terminal-zenburn-scale-outline-headlines
                                 (list :height terminal-zenburn-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-green+4
                             ,@(when terminal-zenburn-scale-outline-headlines
                                 (list :height terminal-zenburn-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-blue-1
                             ,@(when terminal-zenburn-scale-outline-headlines
                                 (list :height terminal-zenburn-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-yellow-2
                             ,@(when terminal-zenburn-scale-outline-headlines
                                 (list :height terminal-zenburn-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,terminal-zenburn-blue-4))))
;;;;; c/perl
   `(cperl-nonoverridable-face ((t (:foreground ,terminal-zenburn-magenta))))
   `(cperl-array-face ((t (:foreground ,terminal-zenburn-yellow, :backgorund ,terminal-zenburn-bg))))
   `(cperl-hash-face ((t (:foreground ,terminal-zenburn-yellow-1, :background ,terminal-zenburn-bg))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,terminal-zenburn-blue))))
   `(rcirc-other-nick ((t (:foreground ,terminal-zenburn-orange))))
   `(rcirc-bright-nick ((t (:foreground ,terminal-zenburn-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,terminal-zenburn-blue-2))))
   `(rcirc-server ((t (:foreground ,terminal-zenburn-green))))
   `(rcirc-server-prefix ((t (:foreground ,terminal-zenburn-green+1))))
   `(rcirc-timestamp ((t (:foreground ,terminal-zenburn-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,terminal-zenburn-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-magenta))))
   `(reb-match-1 ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-blue))))
   `(reb-match-2 ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-orange))))
   `(reb-match-3 ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-red))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,terminal-zenburn-green))))
   `(realgud-overlay-arrow2 ((t (:foreground ,terminal-zenburn-yellow))))
   `(realgud-overlay-arrow3 ((t (:foreground ,terminal-zenburn-orange))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:box (:color ,terminal-zenburn-red :style nil)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))
   `(realgud-line-number ((t (:foreground ,terminal-zenburn-yellow))))
   `(realgud-backtrace-number ((t (:foreground ,terminal-zenburn-yellow, :weight bold))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,terminal-zenburn-orange))))
   `(rst-level-2-face ((t (:foreground ,terminal-zenburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,terminal-zenburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,terminal-zenburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,terminal-zenburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,terminal-zenburn-green-2))))
;;;;; selectrum
   `(selectrum-current-candidate ((t (:foreground ,terminal-zenburn-yellow :weight bold :underline t))))
   `(selectrum-primary-highlight ((t (:background ,terminal-zenburn-green-2))))
   `(selectrum-secondary-highlight ((t (:background ,terminal-zenburn-green))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,terminal-zenburn-red+1 :background ,terminal-zenburn-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg+3 :weight bold))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,terminal-zenburn-bg+1 :foreground ,terminal-zenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,terminal-zenburn-bg+1 :foreground ,terminal-zenburn-bg+1))))
   `(whitespace-tab ((t (:background ,terminal-zenburn-red-1))))
   `(whitespace-newline ((t (:foreground ,terminal-zenburn-bg+1))))
   `(whitespace-trailing ((t (:background ,terminal-zenburn-red))))
   `(whitespace-line ((t (:background ,terminal-zenburn-bg :foreground ,terminal-zenburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,terminal-zenburn-orange :foreground ,terminal-zenburn-orange))))
   `(whitespace-indentation ((t (:background ,terminal-zenburn-yellow :foreground ,terminal-zenburn-red))))
   `(whitespace-empty ((t (:background ,terminal-zenburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,terminal-zenburn-yellow :foreground ,terminal-zenburn-red))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,terminal-zenburn-green+4))))
;;;;; lsp-ui
   '(lsp-ui-doc-background ((t (:background "color-236"))))
   ))

;;; Theme Variables
(terminal-zenburn-with-color-variables
  (custom-theme-set-variables
   'terminal-zenburn
;;;;; ansi-color
   `(ansi-color-names-vector [,terminal-zenburn-bg ,terminal-zenburn-red ,terminal-zenburn-green ,terminal-zenburn-yellow
                                          ,terminal-zenburn-blue ,terminal-zenburn-magenta ,terminal-zenburn-cyan ,terminal-zenburn-fg])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,terminal-zenburn-bg+1)
   `(company-quickhelp-color-foreground ,terminal-zenburn-fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,terminal-zenburn-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,terminal-zenburn-red ,terminal-zenburn-orange ,terminal-zenburn-yellow ,terminal-zenburn-green ,terminal-zenburn-green+4
       ,terminal-zenburn-cyan ,terminal-zenburn-blue+1 ,terminal-zenburn-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,terminal-zenburn-fg . ,terminal-zenburn-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,terminal-zenburn-red-1)
       ( 40. . ,terminal-zenburn-red)
       ( 60. . ,terminal-zenburn-orange)
       ( 80. . ,terminal-zenburn-yellow-2)
       (100. . ,terminal-zenburn-yellow-1)
       (120. . ,terminal-zenburn-yellow)
       (140. . ,terminal-zenburn-green-2)
       (160. . ,terminal-zenburn-green)
       (180. . ,terminal-zenburn-green+1)
       (200. . ,terminal-zenburn-green+2)
       (220. . ,terminal-zenburn-green+3)
       (240. . ,terminal-zenburn-green+4)
       (260. . ,terminal-zenburn-cyan)
       (280. . ,terminal-zenburn-blue-2)
       (300. . ,terminal-zenburn-blue-1)
       (320. . ,terminal-zenburn-blue)
       (340. . ,terminal-zenburn-blue+1)
       (360. . ,terminal-zenburn-magenta)))
   `(vc-annotate-very-old-color ,terminal-zenburn-magenta)
   `(vc-annotate-background ,terminal-zenburn-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defcustom terminal-zenburn-add-font-lock-keywords nil
  "Whether to add font-lock keywords for terminal-zenburn color names.

In buffers visiting library `terminal-zenburn-theme.el' the terminal-zenburn
specific keywords are always added, provided that library has
been loaded (because that is where the code that does it is
defined).  If you visit this file and only enable the theme,
then you have to turn `rainbow-mode' off and on again for the
terminal-zenburn-specific font-lock keywords to be used.

In all other Emacs-Lisp buffers this variable controls whether
this should be done.  This requires library `rainbow-mode'."
  :type 'boolean
  :group 'terminal-zenburn-theme)

(defvar terminal-zenburn-colors-font-lock-keywords nil)

(defun terminal-zenburn--rainbow-turn-on ()
  "Maybe also add font-lock keywords for terminal-zenburn colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (or terminal-zenburn-add-font-lock-keywords
                 (and (buffer-file-name)
                      (equal (file-name-nondirectory (buffer-file-name))
                             "terminal-zenburn-theme.el"))))
    (unless terminal-zenburn-colors-font-lock-keywords
      (setq terminal-zenburn-colors-font-lock-keywords
            `((,(regexp-opt (mapcar 'car terminal-zenburn-default-colors-alist) 'words)
               (0 (rainbow-colorize-by-assoc terminal-zenburn-default-colors-alist))))))
    (font-lock-add-keywords nil terminal-zenburn-colors-font-lock-keywords 'end)))

(defun terminal-zenburn--rainbow-turn-off ()
  "Also remove font-lock keywords for terminal-zenburn colors."
  (font-lock-remove-keywords nil terminal-zenburn-colors-font-lock-keywords))

(when (fboundp 'advice-add)
  (advice-add 'rainbow-turn-on :after  #'terminal-zenburn--rainbow-turn-on)
  (advice-add 'rainbow-turn-off :after #'terminal-zenburn--rainbow-turn-off))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'terminal-zenburn)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; terminal-zenburn-theme.el ends here
