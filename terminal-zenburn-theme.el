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
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,terminal-zenburn-fg-1 :background ,terminal-zenburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,terminal-zenburn-fg-1 :background ,terminal-zenburn-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,terminal-zenburn-green+1))))
   `(android-mode-error-face ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,terminal-zenburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,terminal-zenburn-green))))
   `(android-mode-warning-face ((t (:foreground ,terminal-zenburn-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,terminal-zenburn-cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,terminal-zenburn-red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-green))))
   `(anzu-match-2 ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-orange))))
   `(anzu-match-3 ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,terminal-zenburn-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,terminal-zenburn-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(font-latex-italic-face ((t (:foreground ,terminal-zenburn-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,terminal-zenburn-orange))))
   `(font-latex-script-char-face ((t (:foreground ,terminal-zenburn-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,terminal-zenburn-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,terminal-zenburn-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,terminal-zenburn-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,terminal-zenburn-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,terminal-zenburn-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,terminal-zenburn-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,terminal-zenburn-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,terminal-zenburn-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,terminal-zenburn-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,terminal-zenburn-bg+3 :foreground ,terminal-zenburn-bg-2))))
   `(ac-selection-face ((t (:background ,terminal-zenburn-blue-4 :foreground ,terminal-zenburn-fg))))
   `(popup-tip-face ((t (:background ,terminal-zenburn-yellow-2 :foreground ,terminal-zenburn-bg-2))))
   `(popup-menu-mouse-face ((t (:background ,terminal-zenburn-yellow-2 :foreground ,terminal-zenburn-bg-2))))
   `(popup-summary-face ((t (:background ,terminal-zenburn-bg+3 :foreground ,terminal-zenburn-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,terminal-zenburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,terminal-zenburn-bg-1))))
   `(popup-isearch-match ((t (:background ,terminal-zenburn-bg :foreground ,terminal-zenburn-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,terminal-zenburn-fg-1 :background ,terminal-zenburn-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,terminal-zenburn-green+3 :background ,terminal-zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,terminal-zenburn-yellow :background ,terminal-zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,terminal-zenburn-red+1 :background ,terminal-zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,terminal-zenburn-cyan :background ,terminal-zenburn-bg :inverse-video nil :weight bold))))
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
;;;;; bm
   `(bm-face ((t (:background ,terminal-zenburn-yellow-1 :foreground ,terminal-zenburn-bg))))
   `(bm-fringe-face ((t (:background ,terminal-zenburn-yellow-1 :foreground ,terminal-zenburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,terminal-zenburn-green-2 :foreground ,terminal-zenburn-bg))))
   `(bm-persistent-face ((t (:background ,terminal-zenburn-green-2 :foreground ,terminal-zenburn-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,terminal-zenburn-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,terminal-zenburn-green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,terminal-zenburn-fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,terminal-zenburn-cyan))))
   `(cfw:face-saturday ((t (:foreground ,terminal-zenburn-blue :weight bold))))
   `(cfw:face-select ((t (:background ,terminal-zenburn-blue-5))))
   `(cfw:face-sunday ((t (:foreground ,terminal-zenburn-red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,terminal-zenburn-cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,terminal-zenburn-blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; centaur-tabs
   `(centaur-tabs-default ((t (:background ,terminal-zenburn-bg :foreground ,terminal-zenburn-fg :box nil))))
   `(centaur-tabs-selected ((t (:background ,terminal-zenburn-bg :foreground ,terminal-zenburn-fg+2 :box nil))))
   `(centaur-tabs-unselected ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-fg-05 :box nil))))
   `(centaur-tabs-selected-modified ((t (:background ,terminal-zenburn-bg :foreground ,terminal-zenburn-orange :box nil))))
   `(centaur-tabs-unselected-modified ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-orange :box nil))))
   `(centaur-tabs-active-bar-face ((t (:background ,terminal-zenburn-yellow :box nil))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground ,terminal-zenburn-yellow :box nil))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground ,terminal-zenburn-yellow :box nil))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,terminal-zenburn-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,terminal-zenburn-green+1))))
   `(cider-deprecated-face ((t (:background ,terminal-zenburn-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,terminal-zenburn-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,terminal-zenburn-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,terminal-zenburn-red-4))))
   `(cider-test-error-face ((t (:background ,terminal-zenburn-magenta))))
   `(cider-test-success-face ((t (:background ,terminal-zenburn-green-2))))
   `(cider-fringe-good-face ((t (:foreground ,terminal-zenburn-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,terminal-zenburn-cyan))))
   `(circe-my-message-face ((t (:foreground ,terminal-zenburn-fg))))
   `(circe-fool-face ((t (:foreground ,terminal-zenburn-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,terminal-zenburn-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,terminal-zenburn-fg))))
   `(circe-server-face ((t (:foreground ,terminal-zenburn-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,terminal-zenburn-orange :background ,terminal-zenburn-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,terminal-zenburn-fg)))
   `(context-coloring-level-1-face ((t :foreground ,terminal-zenburn-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,terminal-zenburn-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,terminal-zenburn-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,terminal-zenburn-orange)))
   `(context-coloring-level-5-face ((t :foreground ,terminal-zenburn-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,terminal-zenburn-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,terminal-zenburn-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,terminal-zenburn-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,terminal-zenburn-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,terminal-zenburn-blue :foreground ,terminal-zenburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,terminal-zenburn-bg-05 :foreground ,terminal-zenburn-bg))))
   `(ctbl:face-row-select ((t (:background ,terminal-zenburn-cyan :foreground ,terminal-zenburn-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,terminal-zenburn-fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,terminal-zenburn-green))))
   `(debbugs-gnu-new ((t (:foreground ,terminal-zenburn-red))))
   `(debbugs-gnu-pending ((t (:foreground ,terminal-zenburn-blue))))
   `(debbugs-gnu-stale ((t (:foreground ,terminal-zenburn-orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,terminal-zenburn-red))))
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
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,terminal-zenburn-blue :background ,terminal-zenburn-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,terminal-zenburn-red+1 :background ,terminal-zenburn-red-1))))
   `(diff-hl-insert ((,class (:foreground ,terminal-zenburn-green+1 :background ,terminal-zenburn-green-2))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,terminal-zenburn-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,terminal-zenburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,terminal-zenburn-orange))))
   `(diredp-date-time ((t (:foreground ,terminal-zenburn-magenta))))
   `(diredp-deletion ((t (:foreground ,terminal-zenburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,terminal-zenburn-red))))
   `(diredp-dir-heading ((t (:foreground ,terminal-zenburn-blue :background ,terminal-zenburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,terminal-zenburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,terminal-zenburn-red))))
   `(diredp-executable-tag ((t (:foreground ,terminal-zenburn-green+1))))
   `(diredp-file-name ((t (:foreground ,terminal-zenburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,terminal-zenburn-green))))
   `(diredp-flag-mark ((t (:foreground ,terminal-zenburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,terminal-zenburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,terminal-zenburn-red))))
   `(diredp-link-priv ((t (:foreground ,terminal-zenburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,terminal-zenburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,terminal-zenburn-orange))))
   `(diredp-no-priv ((t (:foreground ,terminal-zenburn-fg))))
   `(diredp-number ((t (:foreground ,terminal-zenburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,terminal-zenburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,terminal-zenburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,terminal-zenburn-green-2))))
   `(diredp-symlink ((t (:foreground ,terminal-zenburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,terminal-zenburn-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,terminal-zenburn-red :weight bold))))
   `(dired-async-message ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,terminal-zenburn-yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,terminal-zenburn-orange))))
   `(diredfl-date-time ((t (:foreground ,terminal-zenburn-magenta))))
   `(diredfl-deletion ((t (:foreground ,terminal-zenburn-yellow))))
   `(diredfl-deletion-file-name ((t (:foreground ,terminal-zenburn-red))))
   `(diredfl-dir-heading ((t (:foreground ,terminal-zenburn-blue :background ,terminal-zenburn-bg-1))))
   `(diredfl-dir-priv ((t (:foreground ,terminal-zenburn-cyan))))
   `(diredfl-exec-priv ((t (:foreground ,terminal-zenburn-red))))
   `(diredfl-executable-tag ((t (:foreground ,terminal-zenburn-green+1))))
   `(diredfl-file-name ((t (:foreground ,terminal-zenburn-blue))))
   `(diredfl-file-suffix ((t (:foreground ,terminal-zenburn-green))))
   `(diredfl-flag-mark ((t (:foreground ,terminal-zenburn-yellow))))
   `(diredfl-flag-mark-line ((t (:foreground ,terminal-zenburn-orange))))
   `(diredfl-ignored-file-name ((t (:foreground ,terminal-zenburn-red))))
   `(diredfl-link-priv ((t (:foreground ,terminal-zenburn-yellow))))
   `(diredfl-no-priv ((t (:foreground ,terminal-zenburn-fg))))
   `(diredfl-number ((t (:foreground ,terminal-zenburn-green+1))))
   `(diredfl-other-priv ((t (:foreground ,terminal-zenburn-yellow-1))))
   `(diredfl-rare-priv ((t (:foreground ,terminal-zenburn-red-1))))
   `(diredfl-read-priv ((t (:foreground ,terminal-zenburn-green-1))))
   `(diredfl-symlink ((t (:foreground ,terminal-zenburn-yellow))))
   `(diredfl-write-priv ((t (:foreground ,terminal-zenburn-magenta))))
;;;;; doom-modeline
   `(doom-modeline-bar  ((t (:background ,terminal-zenburn-yellow))))
   `(doom-modeline-inactive-bar  ((t (:background nil))))
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
;;;;; egg
   `(egg-text-base ((t (:foreground ,terminal-zenburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,terminal-zenburn-yellow))))
   `(egg-help-header-2 ((t (:foreground ,terminal-zenburn-green+3))))
   `(egg-branch ((t (:foreground ,terminal-zenburn-yellow))))
   `(egg-branch-mono ((t (:foreground ,terminal-zenburn-yellow))))
   `(egg-term ((t (:foreground ,terminal-zenburn-yellow))))
   `(egg-diff-add ((t (:foreground ,terminal-zenburn-green+4))))
   `(egg-diff-del ((t (:foreground ,terminal-zenburn-red+1))))
   `(egg-diff-file-header ((t (:foreground ,terminal-zenburn-yellow-2))))
   `(egg-section-title ((t (:foreground ,terminal-zenburn-yellow))))
   `(egg-stash-mono ((t (:foreground ,terminal-zenburn-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,terminal-zenburn-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,terminal-zenburn-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,terminal-zenburn-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,terminal-zenburn-green))))
   `(elfeed-search-feed-face ((t (:foreground ,terminal-zenburn-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,terminal-zenburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,terminal-zenburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,terminal-zenburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,terminal-zenburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg))))
   `(w3m-lnum-match ((t (:background ,terminal-zenburn-bg-1
                                     :foreground ,terminal-zenburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,terminal-zenburn-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,terminal-zenburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,terminal-zenburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,terminal-zenburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,terminal-zenburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,terminal-zenburn-green))))
   `(erc-pal-face ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,terminal-zenburn-orange :background ,terminal-zenburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,terminal-zenburn-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,terminal-zenburn-green+4 :background ,terminal-zenburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,terminal-zenburn-red :background ,terminal-zenburn-bg))))
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
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,terminal-zenburn-green+2 :weight bold))))
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
;;;;; full-ack
   `(ack-separator ((t (:foreground ,terminal-zenburn-fg))))
   `(ack-file ((t (:foreground ,terminal-zenburn-blue))))
   `(ack-line ((t (:foreground ,terminal-zenburn-yellow))))
   `(ack-match ((t (:foreground ,terminal-zenburn-orange :background ,terminal-zenburn-bg-1 :weight bold))))
;;;;; git-annex
   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,terminal-zenburn-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,terminal-zenburn-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,terminal-zenburn-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,terminal-zenburn-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,terminal-zenburn-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,terminal-zenburn-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,terminal-zenburn-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,terminal-zenburn-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,terminal-zenburn-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,terminal-zenburn-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,terminal-zenburn-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,terminal-zenburn-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, terminal-zenburn-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,terminal-zenburn-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,terminal-zenburn-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,terminal-zenburn-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,terminal-zenburn-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,terminal-zenburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,terminal-zenburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,terminal-zenburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,terminal-zenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,terminal-zenburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,terminal-zenburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,terminal-zenburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,terminal-zenburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,terminal-zenburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,terminal-zenburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,terminal-zenburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,terminal-zenburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,terminal-zenburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,terminal-zenburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,terminal-zenburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,terminal-zenburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,terminal-zenburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,terminal-zenburn-green))))
   `(gnus-cite-7 ((t (:foreground ,terminal-zenburn-red))))
   `(gnus-cite-8 ((t (:foreground ,terminal-zenburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,terminal-zenburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,terminal-zenburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,terminal-zenburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,terminal-zenburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,terminal-zenburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,terminal-zenburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,terminal-zenburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,terminal-zenburn-bg+2))))
   `(gnus-signature ((t (:foreground ,terminal-zenburn-yellow))))
   `(gnus-x ((t (:background ,terminal-zenburn-fg :foreground ,terminal-zenburn-bg))))
   `(mm-uu-extract ((t (:background ,terminal-zenburn-bg-05 :foreground ,terminal-zenburn-green+1))))
;;;;; go-guru
   `(go-guru-hl-identifier-face ((t (:foreground ,terminal-zenburn-bg-1 :background ,terminal-zenburn-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,terminal-zenburn-blue))))
   `(guide-key/key-face ((t (:foreground ,terminal-zenburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,terminal-zenburn-green+1))))
;;;;; hackernews
   '(hackernews-comment-count ((t (:inherit link-visited :underline nil))))
   '(hackernews-link          ((t (:inherit link         :underline nil))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,terminal-zenburn-green
                      :background ,terminal-zenburn-bg
                      :underline nil
                      :box nil
                      :extend t))))
   `(helm-source-header
     ((t (:foreground ,terminal-zenburn-yellow
                      :background ,terminal-zenburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)
                      :extend t))))
   `(helm-selection ((t (:background ,terminal-zenburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,terminal-zenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,terminal-zenburn-green+4 :background ,terminal-zenburn-bg-1))))
   `(helm-separator ((t (:foreground ,terminal-zenburn-red :background ,terminal-zenburn-bg))))
   `(helm-time-zone-current ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg))))
   `(helm-time-zone-home ((t (:foreground ,terminal-zenburn-red :background ,terminal-zenburn-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,terminal-zenburn-orange :background ,terminal-zenburn-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,terminal-zenburn-magenta :background ,terminal-zenburn-bg))))
   `(helm-bookmark-info ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg))))
   `(helm-bookmark-man ((t (:foreground ,terminal-zenburn-yellow :background ,terminal-zenburn-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,terminal-zenburn-magenta :background ,terminal-zenburn-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,terminal-zenburn-red :background ,terminal-zenburn-bg))))
   `(helm-buffer-process ((t (:foreground ,terminal-zenburn-cyan :background ,terminal-zenburn-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg))))
   `(helm-buffer-size ((t (:foreground ,terminal-zenburn-fg-1 :background ,terminal-zenburn-bg))))
   `(helm-ff-directory ((t (:foreground ,terminal-zenburn-cyan :background ,terminal-zenburn-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,terminal-zenburn-red :background ,terminal-zenburn-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,terminal-zenburn-yellow :background ,terminal-zenburn-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,terminal-zenburn-cyan :background ,terminal-zenburn-bg))))
   `(helm-grep-file ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg))))
   `(helm-grep-finish ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg))))
   `(helm-grep-lineno ((t (:foreground ,terminal-zenburn-fg-1 :background ,terminal-zenburn-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,terminal-zenburn-red :background ,terminal-zenburn-bg))))
   `(helm-match ((t (:foreground ,terminal-zenburn-orange :background ,terminal-zenburn-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,terminal-zenburn-cyan :background ,terminal-zenburn-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,terminal-zenburn-fg-1 :background ,terminal-zenburn-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg))))
;;;;; helm-lxc
   `(helm-lxc-face-frozen ((t (:foreground ,terminal-zenburn-blue :background ,terminal-zenburn-bg))))
   `(helm-lxc-face-running ((t (:foreground ,terminal-zenburn-green :background ,terminal-zenburn-bg))))
   `(helm-lxc-face-stopped ((t (:foreground ,terminal-zenburn-red :background ,terminal-zenburn-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,terminal-zenburn-yellow :background ,terminal-zenburn-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,terminal-zenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,terminal-zenburn-bg-05 :extend t)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,terminal-zenburn-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,terminal-zenburn-red-1 :background ,terminal-zenburn-bg))))
   `(hydra-face-amaranth ((t (:foreground ,terminal-zenburn-red-3 :background ,terminal-zenburn-bg))))
   `(hydra-face-blue ((t (:foreground ,terminal-zenburn-blue :background ,terminal-zenburn-bg))))
   `(hydra-face-pink ((t (:foreground ,terminal-zenburn-magenta :background ,terminal-zenburn-bg))))
   `(hydra-face-teal ((t (:foreground ,terminal-zenburn-cyan :background ,terminal-zenburn-bg))))
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
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,terminal-zenburn-red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,terminal-zenburn-red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,terminal-zenburn-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,terminal-zenburn-cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,terminal-zenburn-green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,terminal-zenburn-green+3))))
   `(irfc-title-face ((t (:foreground ,terminal-zenburn-yellow
                                      :underline t :weight bold))))
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
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,terminal-zenburn-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,terminal-zenburn-yellow))))
   `(ido-indicator ((t (:foreground ,terminal-zenburn-yellow :background ,terminal-zenburn-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,terminal-zenburn-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,terminal-zenburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,terminal-zenburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,terminal-zenburn-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,terminal-zenburn-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,terminal-zenburn-orange))))
   `(jabber-roster-user-error ((t (:foreground ,terminal-zenburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,terminal-zenburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,terminal-zenburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,terminal-zenburn-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,terminal-zenburn-green+3))))
   `(jabber-activity-face((t (:foreground ,terminal-zenburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,terminal-zenburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
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
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,terminal-zenburn-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,terminal-zenburn-fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,terminal-zenburn-red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,terminal-zenburn-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,terminal-zenburn-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,terminal-zenburn-green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,terminal-zenburn-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,terminal-zenburn-fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,terminal-zenburn-orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,terminal-zenburn-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,terminal-zenburn-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,terminal-zenburn-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,terminal-zenburn-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,terminal-zenburn-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,terminal-zenburn-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,terminal-zenburn-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,terminal-zenburn-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,terminal-zenburn-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,terminal-zenburn-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,terminal-zenburn-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,terminal-zenburn-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,terminal-zenburn-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,terminal-zenburn-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,terminal-zenburn-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,terminal-zenburn-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,terminal-zenburn-blue-1))))
   `(lui-hilight-face ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,terminal-zenburn-green+2 :background ,terminal-zenburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,terminal-zenburn-red+1 :background ,terminal-zenburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,terminal-zenburn-blue+1 :background ,terminal-zenburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,terminal-zenburn-magenta :background ,terminal-zenburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,terminal-zenburn-yellow :background ,terminal-zenburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
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
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,terminal-zenburn-blue+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,terminal-zenburn-yellow-2 :underline t))))
   `(markup-list-face ((t (:foreground ,terminal-zenburn-fg+1))))
   `(markup-meta-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(markup-meta-hide-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(markup-secondary-text-face ((t (:foreground ,terminal-zenburn-yellow-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,terminal-zenburn-yellow))))
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
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,terminal-zenburn-orange))))
   `(mew-face-header-from ((t (:foreground ,terminal-zenburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,terminal-zenburn-green))))
   `(mew-face-header-to ((t (:foreground ,terminal-zenburn-red))))
   `(mew-face-header-key ((t (:foreground ,terminal-zenburn-green))))
   `(mew-face-header-private ((t (:foreground ,terminal-zenburn-green))))
   `(mew-face-header-important ((t (:foreground ,terminal-zenburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,terminal-zenburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,terminal-zenburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,terminal-zenburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,terminal-zenburn-red))))
   `(mew-face-body-url ((t (:foreground ,terminal-zenburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,terminal-zenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,terminal-zenburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,terminal-zenburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,terminal-zenburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,terminal-zenburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,terminal-zenburn-red))))
   `(mew-face-mark-review ((t (:foreground ,terminal-zenburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,terminal-zenburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,terminal-zenburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,terminal-zenburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,terminal-zenburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,terminal-zenburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,terminal-zenburn-green))))
   `(mew-face-eof-part ((t (:foreground ,terminal-zenburn-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,terminal-zenburn-cyan :background ,terminal-zenburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,terminal-zenburn-blue))))
   `(mingus-pausing-face ((t (:foreground ,terminal-zenburn-magenta))))
   `(mingus-playing-face ((t (:foreground ,terminal-zenburn-cyan))))
   `(mingus-playlist-face ((t (:foreground ,terminal-zenburn-cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,terminal-zenburn-magenta))))
   `(mingus-song-file-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(mingus-artist-face ((t (:foreground ,terminal-zenburn-cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,terminal-zenburn-red+1))))
   `(mingus-album-stale-face ((t (:foreground ,terminal-zenburn-red+1))))
   `(mingus-stopped-face ((t (:foreground ,terminal-zenburn-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,terminal-zenburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,terminal-zenburn-cyan))))
   `(nav-face-dir ((t (:foreground ,terminal-zenburn-green))))
   `(nav-face-hdir ((t (:foreground ,terminal-zenburn-red))))
   `(nav-face-file ((t (:foreground ,terminal-zenburn-fg))))
   `(nav-face-hfile ((t (:foreground ,terminal-zenburn-red-4))))
;;;;; merlin
   `(merlin-type-face ((t (:inherit highlight))))
   `(merlin-compilation-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-orange)))
      (t
       (:underline ,terminal-zenburn-orange))))
   `(merlin-compilation-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-red)))
      (t
       (:underline ,terminal-zenburn-red))))
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
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,terminal-zenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,terminal-zenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,terminal-zenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,terminal-zenburn-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,terminal-zenburn-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,terminal-zenburn-fg))))
   `(neo-root-dir-face ((t (:foreground ,terminal-zenburn-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,terminal-zenburn-blue))))
   `(neo-file-link-face ((t (:foreground ,terminal-zenburn-fg))))
   `(neo-expand-btn-face ((t (:foreground ,terminal-zenburn-blue))))
   `(neo-vc-default-face ((t (:foreground ,terminal-zenburn-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,terminal-zenburn-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,terminal-zenburn-fg))))
   `(neo-vc-edited-face ((t (:foreground ,terminal-zenburn-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,terminal-zenburn-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,terminal-zenburn-red :background ,terminal-zenburn-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,terminal-zenburn-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,terminal-zenburn-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,terminal-zenburn-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,terminal-zenburn-fg-1))))
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
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; c/perl
   `(cperl-nonoverridable-face ((t (:foreground ,terminal-zenburn-magenta))))
   `(cperl-array-face ((t (:foreground ,terminal-zenburn-yellow, :backgorund ,terminal-zenburn-bg))))
   `(cperl-hash-face ((t (:foreground ,terminal-zenburn-yellow-1, :background ,terminal-zenburn-bg))))
;;;;; paren-face
   `(parenthesis ((t (:foreground ,terminal-zenburn-fg-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,terminal-zenburn-yellow-2))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,terminal-zenburn-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,terminal-zenburn-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,terminal-zenburn-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,terminal-zenburn-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-orange))))
   `(proof-error-face ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-orange))))
   `(proof-locked-face ((t (:background ,terminal-zenburn-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-orange))))
   `(proof-queue-face ((t (:background ,terminal-zenburn-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,terminal-zenburn-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,terminal-zenburn-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,terminal-zenburn-bg))))
   `(proof-warning-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,terminal-zenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,terminal-zenburn-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,terminal-zenburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,terminal-zenburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,terminal-zenburn-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,terminal-zenburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,terminal-zenburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,terminal-zenburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,terminal-zenburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,terminal-zenburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,terminal-zenburn-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,terminal-zenburn-blue-5))))
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
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,terminal-zenburn-blue-4 :weight bold))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,terminal-zenburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,terminal-zenburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,terminal-zenburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,terminal-zenburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,terminal-zenburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,terminal-zenburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,terminal-zenburn-red))))
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
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,terminal-zenburn-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,terminal-zenburn-red+1 :background ,terminal-zenburn-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,terminal-zenburn-fg :background ,terminal-zenburn-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Terminal-Zenburn for sml
   `(sml/global ((,class (:foreground ,terminal-zenburn-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,terminal-zenburn-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,terminal-zenburn-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,terminal-zenburn-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,terminal-zenburn-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,terminal-zenburn-orange))))
   `(sml/git ((,class (:foreground ,terminal-zenburn-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,terminal-zenburn-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,terminal-zenburn-red-2))))
   `(sml/outside-modified ((,class (:foreground ,terminal-zenburn-orange))))
   `(sml/modified ((,class (:foreground ,terminal-zenburn-red))))
   `(sml/vc-edited ((,class (:foreground ,terminal-zenburn-green+2))))
   `(sml/charging ((,class (:foreground ,terminal-zenburn-green+4))))
   `(sml/discharging ((,class (:foreground ,terminal-zenburn-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,terminal-zenburn-red+1 :background ,terminal-zenburn-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,terminal-zenburn-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,terminal-zenburn-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,terminal-zenburn-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-red)))
      (t
       (:underline ,terminal-zenburn-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-orange)))
      (t
       (:underline ,terminal-zenburn-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-yellow)))
      (t
       (:underline ,terminal-zenburn-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,terminal-zenburn-green)))
      (t
       (:underline ,terminal-zenburn-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; solaire
   `(solaire-default-face ((t (:inherit default :background ,terminal-zenburn-bg-08))))
   `(solaire-minibuffer-face ((t (:inherit default :background ,terminal-zenburn-bg-08))))
   `(solaire-hl-line-face ((t (:inherit hl-line :background ,terminal-zenburn-bg))))
   `(solaire-org-hide-face ((t (:inherit org-hide :background ,terminal-zenburn-bg-08))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,terminal-zenburn-green+2))))
   `(speedbar-directory-face ((t (:foreground ,terminal-zenburn-cyan))))
   `(speedbar-file-face ((t (:foreground ,terminal-zenburn-fg))))
   `(speedbar-highlight-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-green+2))))
   `(speedbar-selected-face ((t (:foreground ,terminal-zenburn-red))))
   `(speedbar-separator-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,terminal-zenburn-yellow))))
;;;;; swiper
   `(swiper-line-face ((t (:underline t))))
;;;;; sx
   `(sx-custom-button
     ((t (:background ,terminal-zenburn-fg :foreground ,terminal-zenburn-bg-1
          :box (:line-width 3 :style released-button) :height 0.9))))
   `(sx-question-list-answers
     ((t (:foreground ,terminal-zenburn-green+3
          :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
     ((t (:foreground ,terminal-zenburn-green+3
          :height 1.3 :inherit sx-question-mode-title))))
   '(sx-question-mode-content-face ((t (:inherit highlight))))
   `(sx-question-mode-kbd-tag
     ((t (:box (:color ,terminal-zenburn-bg-1 :line-width 3 :style released-button)
          :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,terminal-zenburn-fg
                                    :background ,terminal-zenburn-bg))))
   `(tabbar-selected ((t (:foreground ,terminal-zenburn-fg
                                      :background ,terminal-zenburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,terminal-zenburn-fg
                                        :background ,terminal-zenburn-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,terminal-zenburn-bg
                                       :background ,terminal-zenburn-bg-1))))
   `(term-color-red ((t (:foreground ,terminal-zenburn-red-2
                                     :background ,terminal-zenburn-red-4))))
   `(term-color-green ((t (:foreground ,terminal-zenburn-green
                                       :background ,terminal-zenburn-green+2))))
   `(term-color-yellow ((t (:foreground ,terminal-zenburn-orange
                                        :background ,terminal-zenburn-yellow))))
   `(term-color-blue ((t (:foreground ,terminal-zenburn-blue-1
                                      :background ,terminal-zenburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,terminal-zenburn-magenta
                                         :background ,terminal-zenburn-red))))
   `(term-color-cyan ((t (:foreground ,terminal-zenburn-cyan
                                      :background ,terminal-zenburn-blue))))
   `(term-color-white ((t (:foreground ,terminal-zenburn-fg
                                       :background ,terminal-zenburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,terminal-zenburn-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,terminal-zenburn-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,terminal-zenburn-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,terminal-zenburn-cyan))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-green :weight bold))))
   `(vr/group-1 ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,terminal-zenburn-yellow-2 :background ,terminal-zenburn-bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,terminal-zenburn-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,terminal-zenburn-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,terminal-zenburn-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,terminal-zenburn-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,terminal-zenburn-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,terminal-zenburn-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,terminal-zenburn-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,terminal-zenburn-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,terminal-zenburn-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,terminal-zenburn-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,terminal-zenburn-red))))
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
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,terminal-zenburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,terminal-zenburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,terminal-zenburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,terminal-zenburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,terminal-zenburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,terminal-zenburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,terminal-zenburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,terminal-zenburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,terminal-zenburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,terminal-zenburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,terminal-zenburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,terminal-zenburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,terminal-zenburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,terminal-zenburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,terminal-zenburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,terminal-zenburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,terminal-zenburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,terminal-zenburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,terminal-zenburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,terminal-zenburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,terminal-zenburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,terminal-zenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,terminal-zenburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,terminal-zenburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,terminal-zenburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,terminal-zenburn-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,terminal-zenburn-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,terminal-zenburn-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,terminal-zenburn-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,terminal-zenburn-bg :background ,terminal-zenburn-blue+1))))
   `(cscope-separator-face ((t (:foreground ,terminal-zenburn-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,terminal-zenburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,terminal-zenburn-bg-1 :foreground ,terminal-zenburn-bg-1))))
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
