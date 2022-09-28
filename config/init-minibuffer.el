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

;; Vertico ------------------------------------------------------------------- ;

(vertico-mode)
(savehist-mode)

;; Copied from vertico github example configurations:
;; 
;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Orderless ----------------------------------------------------------------- ;

;; Only activate orderless in minibuffer completion
(add-hook 'minibuffer-setup-hook (lambda ()
                                   (setq completion-styles '(orderless basic))))
(setq completion-category-defaults nil
      completion-category-overrides '((file (styles orderless partial-completion))))
(setq orderless-matching-styles '(orderless-literal
                                  orderless-regexp
                                  orderless-initialism
                                  orderless-flex))
(with-eval-after-load 'orderless
  (defun my/orderless-literal-if-suffix-bang (pattern index _total)
    (if (string-suffix-p "!" pattern)
        `(orderless-literal . ,(substring pattern 0 -1))))
  (setq orderless-style-dispatchers '(my/orderless-literal-if-suffix-bang))

  (defvar orderless-fuz-threshold 200)
  (require 'flx)
  (defun my/vertico-sort-flx (candidates)
    "Sort CANDIDATES with flx scores."
    ;; Copied from https://github.com/minad/vertico/issues/76#issuecomment-877427128
    (when candidates
      (let* ((query (buffer-substring (minibuffer-prompt-end)
                                      (max (minibuffer-prompt-end) (point))))
             (category (completion-metadata-get
                        (completion-metadata query
                                             minibuffer-completion-table
                                             minibuffer-completion-predicate)
                        'category)))
        (when (eq category 'file)
          (if (string-suffix-p "/" query)
              (setq query "")
            (setq query (file-name-nondirectory query))))
        (if (and (not (string-empty-p query))
                 (< (length candidates) orderless-fuz-threshold))
            (let* ((queries (split-string query orderless-component-separator))
                   (matches (mapcar (lambda (item)
                                      (cons item
                                            (apply '+
                                                   (mapcar
                                                    (lambda (q)
                                                      (car (or
                                                            (flx-score item q flx-strings-cache)
                                                            '(-1000))))
                                                    queries))))
                                    candidates)))
              (setq matches (sort matches (lambda (x y) (> (cdr x) (cdr y)))))
              (mapcar (lambda (x) (car x)) matches))
          candidates))))
  (setq vertico-sort-override-function #'my/vertico-sort-flx)

  ;; (defun my/orderless-filter-fuz-advice (fn query table &optional pred)
  ;;   (when-let ((results (funcall fn query table pred)))
  ;;     (if (and (not (string-empty-p query))
  ;;              (< (length results) orderless-fuz-threshold))
  ;;         (let* ((queries (split-string query orderless-component-separator))
  ;;                (matches (mapcar (lambda (item)
  ;;                                   (cons item
  ;;                                         (apply '+
  ;;                                                (mapcar
  ;;                                                 (lambda (q)
  ;;                                                   (car (or
  ;;                                                         (flx-score item q flx-strings-cache)
  ;;                                                         '(-1000))))
  ;;                                                 queries))))
  ;;                                 results)))
  ;;           (setq matches (sort matches (lambda (x y) (> (cdr x) (cdr y)))))
  ;;           (mapcar (lambda (x) (car x)) matches))
  ;;       results)))

  ;; (advice-add #'orderless-filter :around #'my/orderless-filter-fuz-advice))

  ;; Disable vertico's default sorting
  ;; (vertico--define-sort (history) 32 (if (eq % "") 0 (/ (aref % 0) 4)) (lambda (a b) -1) (lambda (a b) -1))
  ;; (setq vertico-sort-override-function #'vertico-sort-history))
  )

;; Marginalia ---------------------------------------------------------------- ;

(marginalia-mode)

;; Icons --------------------------------------------------------------------- ;

(icon-tools-completion-mode)

;; Embark -------------------------------------------------------------------- ;

(define-key minibuffer-mode-map (kbd "C-j") 'embark-act)

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

;; Recentf files completion -------------------------------------------------- ;

(defun recentf-open-files-compl ()
  "Find recentf files with `completing-read'."
  (interactive)
  (let* ((file-list (mapcar
                 (lambda (x)
                   (cons (propertize (file-name-nondirectory x)
                                     'full x)
                         x))
                 recentf-list))
         (selectrum-should-sort nil)
         (fname (completing-read "File name: "
                                 (lambda (str pred action)
                                   (if (eq action 'metadata)
                                       '(metadata (category . recentf-file))
                                     (complete-with-action
                                      action file-list str pred))))))
    (when fname (find-file (cdr (assoc fname file-list))))))
(global-set-key (kbd "C-c f r") 'recentf-open-files-compl)

(defun recentf-open-files-annotator (cand)
  (marginalia-annotate-file (get-text-property 0 'full cand)))

(add-to-list 'marginalia-annotator-registry
             '(recentf-file recentf-open-files-annotator builtin none))

;; Switch to buffer in the current project ----------------------------------- ;

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

;; Jump to symbols across the whole project ---------------------------------- ;

(defun ctags-generate-tags ()
  "Generate ctags in project."
  (interactive)
  (let ((default-directory (project-root (project-current)))
        (cmd "git ls-files \"*.el\" \"*.py\" \"*.java\" \"*.cpp\" \"*.c\" \"*.h\" \"*.js\" \"*.jsx\" | ctags -f TAGS -e -L -"))
    (cond
     ;; Windows
     ((memq system-type '(ms-dos windows-nt cygwin))
      (shell-command (concat "Powershell -Command " (shell-quote-argument cmd))))
     ;; MacOS, Linux
     (t
      (shell-command cmd)))))

(defun ctags-read-tag (&optional file)
  "Read ctags tag at point."
  (let ((line (buffer-substring-no-properties
               (point)
               (progn (skip-chars-forward "^") (point))))
        (symbol (buffer-substring-no-properties
                 (progn (skip-chars-forward "") (point))
                 (progn (skip-chars-forward "^") (point))))
        (line-no (string-to-number
                  (buffer-substring-no-properties
                   (progn (skip-chars-forward "") (point))
                   (progn (skip-chars-forward "^,") (point))))))
    (if file
        (list symbol line-no line file)
      (list symbol line-no line))))

(defun project-ctags-tag-annotator (cand)
  (when-let ((tag-info (get-text-property 0 'tag-info cand)))
    (concat (propertize " " 'display '(space :align-to center))
            (propertize (format "%s:%s" (cadddr tag-info) (cadr tag-info)) 'face 'marginalia-value))))

(add-to-list 'marginalia-annotator-registry
             '(etags project-ctags-tag-annotator builtin none))

(defun project-ctags-find-tag ()
  "Jump to symbols across the whole project."
  (interactive)
  (let* ((project-root (project-root (project-current)))
         (tag-file-name
          (concat project-root "TAGS"))
         tag-list
         tag-info-list)
    (with-temp-buffer
      (insert-file-contents tag-file-name)
      (goto-char (point-min))
      (while (re-search-forward "\f\n" nil t)
        (let ((file (buffer-substring-no-properties
                     (point)
                     (save-excursion (skip-chars-forward "^,") (point))))
              (count 0)
              tag-info tag-name)
          (forward-line 1)
          ;; Exuberant ctags add a line starting with the DEL character;
          ;; skip past it.
          (when (looking-at "\177")
            (forward-line 1))
          (while (not (or (eobp) (looking-at "\f")))
            (setq tag-info (save-excursion (ctags-read-tag file)))
            (setq tag-name (car tag-info))
            (push
             ;; Unique symbol matcher
             (format "%s%s"
              (propertize tag-name 'tag-info tag-info)
              (propertize (number-to-string count);; (format " - %s:%s" file (cadr tag-info))
                          'invisible t))
             tag-list)
            (push (list
                   ;; Unique symbol matcher
                   (format "%s%s" (car tag-info) count)
                   tag-info
                   file)
                  tag-info-list)
            (forward-line 1)
            (setq count (1+ count))))))
    (let* ((selectrum-should-sort nil)
           (marginalia--cache-size 0)
           (symbol-at-point (if (use-region-p)
                                (buffer-substring-no-properties
                                 (region-beginning) (region-end))
                              (thing-at-point 'symbol t)))
           (selected-tag
            (completing-read
             "Go to tag: "
             (lambda (str pred action)
               (if (eq action 'metadata)
                   '(metadata . ((category . etags)))
                 (complete-with-action
                  action tag-list str pred)))
             nil nil symbol-at-point))
           (tag-info-list-match (assoc selected-tag tag-info-list))
           (file (caddr tag-info-list-match))
           (full-file-path
            (if (string-prefix-p "/" file)
                file
              (concat project-root file))))
      (find-file full-file-path)
      (goto-char (point-min))
      (forward-line (- (cadr (cadr tag-info-list-match)) 1)))))

(global-set-key (kbd "C-c p g") 'ctags-generate-tags)
(global-set-key (kbd "C-c p i") 'project-ctags-find-tag)

(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
