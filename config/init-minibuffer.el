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
(minibuffer-depth-indicate-mode)

;; Orderless ----------------------------------------------------------------- ;

;; Only activate orderless in minibuffer completion
(add-hook 'minibuffer-setup-hook (lambda ()
                                   (setq-local completion-styles '(orderless basic))))
(setq completion-category-defaults nil
      completion-category-overrides '((file (styles orderless partial-completion))))
(with-eval-after-load 'orderless
  (setq orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-initialism
                                    orderless-flex))

  (defun my/orderless-literal-if-suffix-bang (pattern index _total)
    (if (string-suffix-p "!" pattern)
        `(orderless-literal . ,(substring pattern 0 -1))))
  (setq orderless-style-dispatchers '(my/orderless-literal-if-suffix-bang))

  (defun my/vertico-sort-history (candidates)
    "Sort candidates by history."
    (let ((hhash (vertico--history-hash))
          hcands results2)
      (dolist
          (c candidates)
        (if-let (idx (gethash c hhash))
            (push (cons idx c) hcands)
          (push c results2)))
       (nconc (vertico--sort-decorated hcands)
              (nreverse results2))))

  (defvar orderless-fuz-threshold 200)
  (flx-rs-load-dyn)
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
                   (matches (mapcar
                             (lambda (item)
                               (cons item
                                     (apply
                                      '+
                                      (mapcar
                                       (lambda (q)
                                         (car (or
                                               (flx-rs-score item q)
                                               '(-1000))))
                                       queries))))
                             candidates)))
              (setq matches (sort matches (lambda (x y) (> (cdr x) (cdr y)))))
              (mapcar (lambda (x) (car x)) matches))
          candidates))))
  (defun my/vertico-sort-flx-history (candidates)
    "Sort vertico CANDIDATES first by flx scoring then by history."
    (my/vertico-sort-history (my/vertico-sort-flx candidates)))
  (setq vertico-sort-override-function #'my/vertico-sort-flx-history))

;; Marginalia ---------------------------------------------------------------- ;

(with-eval-after-load 'marginalia
  (setq marginalia-separator "  ")

  ;; Hack: when candidates are truncated for example by advicing
  ;; `vertico--format-candidate', we can't access the truncation candidates
  ;; here. Therefor use my custom 'truncate-to property of candidates if
  ;; present. (The ctags candidates are marked with this property, as can be
  ;; seen below.)
  (defun marginalia--align (cands)
    "Align annotations of CANDS according to `marginalia-align'."
    (when (and cands (> (length cands) 0))
      (setq marginalia--candw-max
            (seq-max (cl-loop for (cand . ann) in cands collect
                              (if-let (truncate-to (get-text-property 0 'truncate-to cand))
                                  (min (string-width cand) truncate-to)
                                (string-width cand))))))
    (cl-loop for (cand . ann) in cands collect
             (progn
               (when-let (align (text-property-any 0 (length ann) 'marginalia--align t ann))
                 (put-text-property
                  align (1+ align) 'display
                  `(space :align-to
                    ,(pcase-exhaustive marginalia-align
                       ('center `(+ center ,marginalia-align-offset))
                       ('left `(+ left ,(+ marginalia-align-offset marginalia--candw-max)))
                       ('right `(+ right ,(+ marginalia-align-offset 1
                                             (- (string-width (substring ann 0 align))
                                                (string-width ann)))))))
                  ann))
               (list cand "" ann))))

  )
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
         (fname (completing-read "File name: "
                                 (lambda (str pred action)
                                   (if (eq action 'metadata)
                                       '(metadata (category . recentf-file))
                                     (complete-with-action
                                      action file-list str pred))))))
    (when fname (find-file (cdr (assoc fname file-list))))))
(global-set-key (kbd "C-c f r") 'recentf-open-files-compl)

(add-to-list 'icon-tools-completion-category-icon-alist
             '(recentf-file . icon-tools-completion-get-file-icon))

(defun marginalia--recentf-file-annotator (cand)
  "Annotate recentf file CAND."
  (when-let* ((file (get-text-property 0 'full cand))
              (attrs (ignore-errors
                       ;; may throw permission denied errors
                       (file-attributes (substitute-in-file-name
                                         (marginalia--full-candidate file))
                                        'integer))))
    (marginalia--fields
     ((file-size-human-readable (file-attribute-size attrs))
      :face 'marginalia-size :width -7)
     ((marginalia--time (file-attribute-modification-time attrs))
      :face 'marginalia-date :width -12)
     ((file-name-directory (abbreviate-file-name file))
      :face 'marginalia-file-name))))

(add-to-list 'marginalia-annotator-registry
             '(recentf-file marginalia--recentf-file-annotator builtin none))

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

(defvar ctags-tag-file "TAGS.json")
(defvar ctags-truncate-width (floor (* (window-width) 0.3)))

(defun ctags-generate-tags ()
  "Generate ctags in project."
  (delete-file (concat (project-root (project-current)) ctags-tag-file))
  (let ((default-directory (project-root (project-current)))
        (cmd (concat "git ls-files \"*.el\" \"*.py\" \"*.java\" \"*.cpp\" \"*.c\" \"*.h\" \"*.js\" \"*.jsx\" \"*.ts\" \"*.tsx\""
                     " | ctags --output-format=json --pseudo-tags= -L - --fields=+n")))
    (cond
     ;; Windows
     ((memq system-type '(ms-dos windows-nt cygwin))
      (shell-command-to-string (concat "Powershell -Command " (shell-quote-argument cmd))))
     ;; MacOS, Linux
     (t
      (shell-command-to-string cmd)))))

(defun ctags-get-tags-json ()
  "Parse ctag tags json."
  (let ((str (ctags-generate-tags))
        (count 0) tags)
    (setq tags (mapcar (lambda (l) (json-parse-string l))
                       (split-string str "\n" t)))
    (mapc (lambda (tag)
            (puthash "name"
                     (format "%s%s"
                             ;; Add 'truncate-to property to tag
                             (propertize (gethash "name" tag) 'full-json tag
                                         'truncate-to ctags-truncate-width)
                             (propertize (number-to-string count) 'invisible t))
                     tag)
            (puthash "pattern"
                     (string-trim
                      (string-remove-suffix
                       "$"
                       (string-remove-prefix
                        "^"
                        (substring (gethash "pattern" tag) 1 -1))))
                     tag)
            (setq count (1+ count)))
          tags)))

(add-to-list 'icon-tools-completion-category-icon-alist
             '(ctags . icon-tools-completion-get-imenu-icon))

(defun project-ctags-tag-annotator (cand)
  (when-let (full-json (get-text-property 0 'full-json cand))
    (marginalia--fields
     ((gethash "kind" full-json)
      :face 'marginalia-type :width 10)
     ((format "%s:%s"
              (abbreviate-file-name (string-trim (gethash "path" full-json)))
              (gethash "line" full-json))
      :face 'marginalia-file-name :truncate 0.5)
     ((string-trim (or (gethash "pattern" full-json) ""))
      :face 'marginalia-function))))

(add-to-list 'marginalia-annotator-registry
             '(ctags project-ctags-tag-annotator builtin none))

(defun project-ctags-find-tag ()
  "Jump to symbols across the whole project."
  (interactive)
  (let* ((project-root (project-root (project-current)))
         (symbol-at-point (if (use-region-p)
                              (buffer-substring-no-properties
                               (region-beginning) (region-end))
                            (thing-at-point 'symbol t)))
         (tags-json (ctags-get-tags-json))
         (tag-list (mapcar (lambda (ht) (gethash "name" ht)) tags-json))
         (selected-tag
          (completing-read
           "Go to tag: "
           (lambda (str pred action)
             (if (eq action 'metadata)
                 '(metadata . ((category . ctags)))
               (complete-with-action
                action tag-list str pred)))
           nil nil symbol-at-point))
         (tag-json (seq-find (lambda (ht) (equal (gethash "name" ht) selected-tag))
                             tags-json))
         (file (gethash "path" tag-json))
         (full-file-path (concat project-root file))
         (line-no (gethash "line" tag-json)))
    (find-file full-file-path)
    (goto-char (point-min))
    (forward-line (- line-no 1))))

(with-eval-after-load 'vertico
  (defun my/vertico-truncate-ctags-candidates (args)
    (when-let (((eq (vertico--metadata-get 'category) 'ctags))
               (w (floor (* (window-width) 0.3)))
               (l (length (car args)))
               ((> l w)))
      (setcar args (concat (truncate-string-to-width (car args) (- w 3)) "...")))
    args)
  (advice-add #'vertico--format-candidate :filter-args #'my/vertico-truncate-ctags-candidates))

(global-set-key (kbd "C-c p i") 'project-ctags-find-tag)

(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
