;;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

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
;; Org configurations.
;; --------------------------------------

;;; Code:

(add-hook 'org-mode-hook 'auto-fill-mode)
;; (setq org-startup-with-inline-images t) ; Display inline images by default
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(defun add-pcomplete-to-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook #'add-pcomplete-to-capf) ; Enable org mode completion

;; Sunrise and Sunset
(defun diary-sunrise ()
  (let ((dss (diary-sunrise-sunset)))
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ",")
      (buffer-substring (point-min) (match-beginning 0)))))

(defun diary-sunset ()
  (let ((dss (diary-sunrise-sunset))
        start end)
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ", ")
      (setq start (match-end 0))
      (search-forward " at")
      (setq end (match-beginning 0))
      (goto-char start)
      (capitalize-word 1)
      (buffer-substring start end))))

(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook
          (lambda ()
            (when (boundp 'electric-pair-inhibit-predicate)
              (setq-local
               electric-pair-inhibit-predicate
               `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))
(with-eval-after-load 'org
  ;; (org-defkey org-mode-map "\C-c{" 'org-cdlatex-environment-indent)
  ;; (add-to-list 'image-type-file-name-regexps '("\\.eps\\'" . imagemagick))
  ;; (add-to-list 'image-file-name-extensions "eps")
  ;; (setq org-image-actual-width '(400)) ; Prevent inline images being too big
  ;; (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images) ; Redisplay after babel executing
  (setq org-adapt-indentation nil)
  (setq org-highlight-latex-and-related '(native))
  ;; (setq org-export-coding-system 'utf-8)           ; Ensure exporting with UTF-8
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (C . t)
     (js . t)
     (ditaa . t)
     (dot . t)
     (org . t)
     (shell . t)
     (latex . t)
     (R . t)
     (gnuplot . t)))
  ;; (setq org-preview-latex-default-process 'imagemagick)
  ;; (setq org-format-latex-options '(:foreground auto :background "Transparent" :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
  ;;                                              ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (setq org-src-window-setup 'current-window)
  (setq org-export-use-babel nil) ; Stop Org from evaluating code blocks
  (setq org-babel-python-command "python3") ; Set the command to python3 instead of python
  (setq org-confirm-babel-evaluate nil)   ; Don't prompt me to confirm everytime I want to evaluate a block
  )

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(with-eval-after-load 'org-capture
  (setq org-capture-templates
        '(("t" "Todo list item" entry
           (file "~/Documents/Org-mode/capture/tasks.org")
           "* TODO %?
SCHEDULED: %^T")
          ("j" "Journals" entry
           (file+olp+datetree "~/Documents/Org-mode/capture/journals.org")
           "* %?
Entered on %T")
          ("n" "Notes" entry
           (file "~/Documents/Org-mode/notes/notes.org")
           "* %?")
          ("p" "Programming notes" entry
           (file "~/Documents/Org-mode/notes/prog-notes.org")
           "* %? %^g"))))
(with-eval-after-load 'org-agenda
  (setq org-agenda-files
        '("~/Documents/Org-mode/capture/journals.org"
          "~/Documents/Org-mode/capture/tasks.org"
          "~/Documents/Org-mode/src/agenda-expressions.org"))
  (setq org-agenda-span 'day)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (300 600 900 1200 1500 1800 2100 2400)
          "......" "----------------"))
  (setq org-log-done 'time))

(with-eval-after-load 'ox
  (require 'ox-md)
  (require 'ox-beamer))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-packages-alist '("UTF8" "ctex"))
  (add-to-list 'org-latex-packages-alist '("" "minted")) ; use minted for code blocks
  (setq org-latex-listings 'minted)
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(with-eval-after-load 'org-roam
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c n t") 'org-roam-tag-add)
    (define-key org-mode-map (kbd "C-c n o") 'org-id-get-create)
    (define-key org-mode-map (kbd "C-c n a") 'org-roam-alias-add)
    (define-key org-mode-map (kbd "C-c n i") 'org-roam-node-insert)
    (define-key org-mode-map (kbd "C-c n l") 'org-roam-buffer-toggle))
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:20}" 'face 'org-tag)))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "${slug}.org" "#+title: ${title}\n")
           :unnarrowed)))
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n r") 'org-roam-node-random)
(global-set-key (kbd "C-c n g") 'org-roam-graph)
(global-set-key (kbd "C-c n c") 'org-roam-capture)
(global-set-key (kbd "C-c n j") 'org-roam-dailies-capture-today)

;; Beautify org -------------------------------------------------------------- ;

(with-eval-after-load 'org
  (set-face-background 'org-block-begin-line 'unspecified)
  (set-face-background 'org-block-end-line 'unspecified))

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local prettify-symbols-alist
                        '(("lambda"  . ?λ)
                          ("#+BEGIN_SRC" . "")
					      ("#+END_SRC" . "")
					      ("#+begin_src" . "")
					      ("#+end_src" . "")
                          ("#+RESULTS:" . "")
                          ("#+results:" . "")
                          ("[ ]" . "")
                          ("[-]" . "")
                          ("[X]" . "")
					      ("#+begin_quote" . "ﱕ")
					      ("#+BEGIN_QUOTE" . "ﱕ")
					      ("#+end_quote" . "")
					      ("#+END_QUOTE" . "")
					      ("#+begin_verse" . "﯑")
					      ("#+BEGIN_VERSE" . "﯑")
					      ("#+end_verse" . "")
					      ("#+END_VERSE" . "")
					      ("#+begin_example" . "")
					      ("#+BEGIN_EXAMPLE" . "")
					      ("#+end_example" . "")
					      ("#+END_EXAMPLE" . "")
                          ("#+begin_export" . "")
                          ("#+BEGIN_EXPORT" . "")
                          ("#+end_export" . "")
                          ("#+END_EXPORT" . "")
                          ("#+END:" . "")
                          ("#+end:" . "")
                          ("#+BEGIN:" . "")
                          ("#+begin:" . "")
                          ("#+CAPTION:" . "✑")
                          ("#+caption:" . "✑")
                          ("#+ATTR_LATEX:" . "✝︎")
                          ("#+attr_latex:" . "✝︎")
                          ("#+TITLE:" . "")
                          ("#+title:" . "")
                          (":PROPERTIES:" . "")
                          (":properties:" . "")
                          (":ID:" . "")
                          (":id:" . "")
                          (":END:" . "")
                          (":end:" . "")
                          ("#+AUTHOR:" . "")
                          ("#+author:" . "")
                          ("#+OPTIONS:" . "")
                          ("#+options:" . "")
                          ))
            (prettify-symbols-mode)))

(provide 'init-org)

;;; init-org.el ends here
