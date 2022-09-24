;;; init-minibuffer.el --- Minibuffer configurations	-*- lexical-binding: t -*-

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
;; Minibuffer configurations.
;; --------------------------------------

;;; Code:

(selectrum-mode t)
(selectrum-prescient-mode t)
(marginalia-mode t)
(icon-tools-completion-mode t)

(with-eval-after-load 'selectrum-prescient
  (setq selectrum-prescient-enable-sorting nil))

;; Prescient sorting mechanism:
;; 1. Most front: recently selected candidates
;; 2. Followed by: frequently selected ones
;; 3. The rest, if `prescient-sort-length-enable' is t (the default), are sorted
;;    by length, otherwise are presented by their original order.
(with-eval-after-load 'prescient
  ;; Prescient filter method can be toggled during session via M-s a/f/...
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (setq prescient-sort-full-matches-first t)
  (setq prescient-sort-length-enable nil)

  (defvar prescient-flx-threshold 500)
  (require 'flx)
  (defun my/prescient-filter-flx-advice (fn query candidates)
    (let ((results (funcall fn query candidates)))
      (if (and results
               (not (string-empty-p query))
               ;; (not (string-match-p " " query))
               (< (length results) prescient-flx-threshold))
          (let* ((queries (prescient-split-query query))
                 (matches (mapcar (lambda (item)
                                    (cons item
                                          (apply '+
                                                 (mapcar
                                                  (lambda (q)
                                                    (car (flx-score item q flx-file-cache)))
                                                  queries))))
                                  results)))
            (setq matches (sort matches (lambda (x y) (> (cdr x) (cdr y)))))
            (mapcar (lambda (x) (car x)) matches))
        results)))

  (advice-add #'prescient-filter :around #'my/prescient-filter-flx-advice)
  ;; (advice-remove #'prescient-filter #'my/prescient-filter-flx-advice)
  )

(with-eval-after-load 'selectrum
  (setq selectrum-count-style 'current/matches)
  (define-key selectrum-minibuffer-map (kbd "C-j") 'embark-act))

(global-set-key (kbd "C-j") 'embark-act)
(global-set-key (kbd "C-q") 'embark-export)
(with-eval-after-load 'embark
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (eval-when-compile
    (defmacro my/embark-split-action (fn split-type)
      `(defun ,(intern (concat "my/embark-"
                               (symbol-name fn)
                               "-"
                               (car (last  (split-string
                                            (symbol-name split-type) "-"))))) ()
         (interactive)
         (funcall #',split-type)
         (call-interactively #',fn))))

  (define-key embark-file-map     (kbd "2") (my/embark-split-action find-file split-window-below))
  (define-key embark-buffer-map   (kbd "2") (my/embark-split-action switch-to-buffer split-window-below))
  (define-key embark-bookmark-map (kbd "2") (my/embark-split-action bookmark-jump split-window-below))

  (define-key embark-file-map     (kbd "3") (my/embark-split-action find-file split-window-right))
  (define-key embark-buffer-map   (kbd "3") (my/embark-split-action switch-to-buffer split-window-right))
  (define-key embark-bookmark-map (kbd "3") (my/embark-split-action bookmark-jump split-window-right))
  )

(global-set-key (kbd "C-c p s") 'consult-ripgrep)
(global-set-key (kbd "C-h a") 'consult-apropos)
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult))

  ;; Start consult-ripgrep search with active region or symbol at point
  (defun my/consult-ripgrep-initial-input-advice (consult-fn &optional dir given-initial)
    "Advising function around CONSULT-FN.

DIR and GIVEN-INITIAL match the method signature of `consult-wrapper'."
    (interactive "P")
    (let ((initial (list (or given-initial
                             (when (use-region-p)
                               (buffer-substring-no-properties (region-beginning) (region-end)))
                             (thing-at-point 'symbol t)))))
      (apply consult-fn dir initial)))
  (advice-add #'consult-ripgrep :around #'my/consult-ripgrep-initial-input-advice)
  (setq consult-preview-key (kbd "C-o")))

;; Provide completion for recent files
(defun recentf-open-files-compl ()
  "Find recentf files with `completing-read'."
  (interactive)
  (let* ((tocpl (mapcar
                 (lambda (x)
                   (propertize
                    (file-name-nondirectory x)
                    'selectrum--candidate-full x))
                 recentf-list))
         (selectrum-should-sort nil)
         (fname (completing-read "File name: "
                                 (lambda (str pred action)
                                   (if (eq action 'metadata)
                                       '(metadata (category . file))
                                     (complete-with-action
                                      action tocpl str pred))))))
    (when fname
      (find-file
       (or (get-text-property 0 'selectrum--candidate-full fname)
           fname)))))
(global-set-key (kbd "C-c f r") 'recentf-open-files-compl)

;; Switch to buffer that belongs to the current project
(defun project-switch-to-buffer ()
  "Switch to buffers of current buffers."
  (interactive)
  (read-buffer
   (format "Switch to buffer in current project (%s):" (project-root (project-current)))
   nil nil
   (lambda (buf)
     (let ((root (expand-file-name (file-name-as-directory (project-root (project-current))))))
       (string-prefix-p
        root (expand-file-name (buffer-local-value 'default-directory (cdr buf))))))))
(global-set-key (kbd "C-c p b") 'project-switch-to-buffer)

(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
