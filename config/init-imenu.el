;;; init-imenu.el --- Imenu configurations	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Imenu configurations.
;; --------------------------------------

;;; Code:

;; Imenu --------------------------------------------------------------------- ;

(global-set-key (kbd "M-i") 'imenu)
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local imenu-create-index-function #'python-imenu-create-flat-index)))

;; Imenu using ctags --------------------------------------------------------- ;

(with-eval-after-load 'imenu
  (defun ctags-create-index-function ()
    (when buffer-file-name
      (let ((bfn buffer-file-name)
            (buf (get-buffer-create "*ctags-output*"))
            (w (floor (* (frame-width) 0.3)))
            (count 0)
            linestr tag imenu-alist)
        (with-current-buffer buf
          (erase-buffer)
          (setq buffer-undo-list t)
          (shell-command (concat "ctags"
                                 " -f -"
                                 " --kinds-all=\\*"
                                 " --output-format=json --pseudo-tags="
                                 " --fields=NPznF --sort=no " bfn)
                         buf)
          (goto-char (point-min))
          (while (not (eobp))
            (setq linestr (buffer-substring-no-properties (point) (line-end-position)))
            (when (and (string-prefix-p "{" linestr) (string-suffix-p "}" linestr))
              (setq tag (json-parse-string linestr))
              (when (gethash "name" tag)
                (puthash "name" (format "%s%s"
                                        ;; Add 'truncate-to property to tag
                                        (propertize (gethash "name" tag)
                                                    'full-json tag
                                                    'kind (gethash "kind" tag)
                                                    'truncate-to w)
                                        (propertize (number-to-string count) 'invisible t))
                         tag))
              (when (gethash "pattern" tag)
                (puthash "pattern" (string-trim
                                    (string-remove-suffix
                                     "$"
                                     (string-remove-prefix
                                      "^"
                                      (substring (gethash "pattern" tag) 1 -1))))
                         tag))
              (push `(,(gethash "name" tag) . ,(gethash "line" tag)) imenu-alist)
              (setq count (1+ count)))
            (forward-line 1)))
        (sort imenu-alist
              (lambda (a b) (< (cdr a) (cdr b)))))))

  (defun ctags-imenu-goto-function (name position &rest rest)
    (goto-char (point-min))
    (forward-line (1- position)))

  (when (executable-find "ctags")
    (setq-default imenu-create-index-function #'ctags-create-index-function)
    (setq-default imenu-default-goto-function #'ctags-imenu-goto-function))

  (defun imenu-ctags-annotator (cand)
    (when-let (full-json (get-text-property 0 'full-json cand))
      (marginalia--fields
       ((gethash "kind" full-json)
        :face 'marginalia-type :width 10)
       ((gethash "line" full-json)
        :face 'marginalia-file-name :width 5)
       ((string-trim (or (gethash "pattern" full-json) ""))
        :face 'marginalia-function))))

  (add-to-list 'marginalia-annotator-registry
               '(imenu imenu-ctags-annotator builtin none)))

;; Project-wise imenu using ctags -------------------------------------------- ;

(defun ctags-generate-tags ()
  "Generate ctags in project."
  (let* ((buf (get-buffer-create "*ctags-output*"))
         (default-directory (project-root (project-current)))
         (git-ls-cmd "git ls-files \"*.el\" \"*.py\" \"*.java\" \"*.cpp\" \"*.c\" \"*.h\" \"*.js\" \"*.jsx\" \"*.ts\" \"*.tsx\"")
         (ctags-cmd (concat "ctags -f - "
                            (if (memq system-type '(ms-dos windows-nt cygwin))
                                "--kinds-all=*"
                              "--kinds-all=\\*")
                            " --output-format=json --pseudo-tags= -L - --fields=NPznF --sort=no"))
         (cmd (format "%s | %s" git-ls-cmd ctags-cmd)))
    (with-current-buffer buf
      (erase-buffer))
    (cond
     ;; Windows
     ((memq system-type '(ms-dos windows-nt cygwin))
      (call-process-shell-command
       (concat "Powershell -Command " (shell-quote-argument cmd))
       nil buf nil))
     ;; MacOS, Linux
     (t
      (call-process-shell-command cmd nil buf nil)))
    buf))

(defun ctags-parse-json ()
  "Parse ctag tags json."
  (let ((buf (ctags-generate-tags))
        (w (floor (* (frame-width) 0.3)))
        (count 0) linestr tag tags)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (setq linestr (buffer-substring-no-properties (point) (line-end-position)))
        (when (and (string-prefix-p "{" linestr) (string-suffix-p "}" linestr))
          (setq tag (json-parse-string linestr))
          (when (gethash "name" tag)
            (puthash "name" (format "%s%s"
                                    ;; Add 'truncate-to property to tag
                                    (propertize (gethash "name" tag)
                                                'full-json tag
                                                'kind (gethash "kind" tag)
                                                'truncate-to w)
                                    (propertize (number-to-string count) 'invisible t))
                     tag))
          (when (gethash "pattern" tag)
            (puthash "pattern" (string-trim
                                (string-remove-suffix
                                 "$"
                                 (string-remove-prefix
                                  "^"
                                  (substring (gethash "pattern" tag) 1 -1))))
                     tag))
          (push tag tags)
          (setq count (1+ count)))
        (forward-line 1)))
    tags))

(defun project-imenu ()
  "Jump to symbols across the whole project."
  (interactive)
  (require 'project)
  (let* ((project-root (project-root (project-current)))
         (symbol-at-point (if (use-region-p)
                              (buffer-substring-no-properties
                               (region-beginning) (region-end))
                            (thing-at-point 'symbol t)))
         (tags-json (ctags-parse-json))
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

(global-set-key (kbd "C-c p i") 'project-imenu)

;; Ctags completion ---------------------------------------------------------- ;

(with-eval-after-load 'icon-tools-completion
  (add-to-list 'icon-tools-completion-category-icon-alist
               '(ctags . icon-tools-completion-get-imenu-icon)))

(with-eval-after-load 'marginalia
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
               '(imenu project-ctags-tag-annotator builtin none))
  (add-to-list 'marginalia-annotator-registry
               '(ctags project-ctags-tag-annotator builtin none)))

(with-eval-after-load 'vertico
  (defun my/vertico-truncate-ctags-candidates (args)
    (when-let (((eq (vertico--metadata-get 'category) 'ctags))
               (w (floor (* (window-width) 0.3)))
               (l (length (car args)))
               ((> l w)))
      (setcar args (concat (truncate-string-to-width (car args) (- w 3)) "...")))
    args)
  (advice-add #'vertico--format-candidate :filter-args #'my/vertico-truncate-ctags-candidates))

(provide 'init-imenu)

;;; init-imenu.el ends here
