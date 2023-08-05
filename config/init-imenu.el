;;; init-imenu.el --- Imenu configurations	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Imenu configurations.
;; --------------------------------------

;;; Code:

(when (executable-find "ctags")

  ;; Symbols outline --------------------------------------------------------- ;
  (with-eval-after-load 'symbols-outline
    ;; (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)
    ;; (setq symbols-outline-use-nerd-icon-in-tui nil)
    (setq symbols-outline-window-position 'left)
    (symbols-outline-follow-mode))
  (global-set-key (kbd "C-c i") 'symbols-outline-show)

  (defun ctags-parse-json (buf)
    "Parse ctag tags json."
    (let ((w (floor (* (frame-width) 0.3)))
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

  ;; Ctags find symbol in buffer, meant as an alternative to imenu ----------- ;

  (defun ctags-generate-tags-in-buffer ()
    "Generate ctags in buffer."
    (let* ((bfn buffer-file-name)
           (buf (get-buffer-create "*ctags-output*")))
      (with-current-buffer buf
        (erase-buffer)
        (shell-command (concat "ctags"
                               " -f -"
                               (if (memq system-type '(ms-dos windows-nt cygwin))
                                   " --kinds-all=*"
                                 " --kinds-all=\\*")
                               " --output-format=json --pseudo-tags="
                               " --fields=NPzn --sort=no " bfn)
                       buf))
      buf))

  (defun ctags-jump-to-symbol-in-buffer ()
    "Jump to symbol in the buffer."
    (interactive)
    (let* ((symbol-at-point (if (use-region-p)
                                (buffer-substring-no-properties
                                 (region-beginning) (region-end))
                              (thing-at-point 'symbol t)))
           (tags-json (ctags-parse-json (ctags-generate-tags-in-buffer)))
           (tag-list (mapcar (lambda (ht) (gethash "name" ht)) tags-json)))
      (if (length= tag-list 0)
          (message "No symbols found.")
        (let* ((selected-tag
                (completing-read
                 "Go to symbol: "
                 (lambda (str pred action)
                   (if (eq action 'metadata)
                       '(metadata . ((category . ctags)))
                     (complete-with-action
                      action tag-list str pred)))
                 nil nil symbol-at-point))
               (tag-json (seq-find (lambda (ht) (equal (gethash "name" ht) selected-tag))
                                   tags-json))
               (line-no (gethash "line" tag-json)))
          (goto-char (point-min))
          (forward-line (- line-no 1))))))

  (global-set-key (kbd "M-i") 'ctags-jump-to-symbol-in-buffer)

  ;; Project-wise imenu using ctags ------------------------------------------ ;
  (defun ctags-generate-tags-in-project ()
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

  (defun ctags-jump-to-symbol-in-project ()
    "Jump to symbols across the whole project."
    (interactive)
    (require 'project)
    (let* ((project-root (project-root (project-current)))
           (symbol-at-point (if (use-region-p)
                                (buffer-substring-no-properties
                                 (region-beginning) (region-end))
                              (thing-at-point 'symbol t)))
           (tags-json (ctags-parse-json (ctags-generate-tags-in-project)))
           (tag-list (mapcar (lambda (ht) (gethash "name" ht)) tags-json))
           (selected-tag
            (completing-read
             "Go to symbol: "
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

  (global-set-key (kbd "C-c p i") 'ctags-jump-to-symbol-in-project)

  (with-eval-after-load 'nerd-icons-completion
    (add-to-list 'nerd-icons-completion-category-icon-alist
                 '(ctags . nerd-icons-completion-get-imenu-icon)))

  (with-eval-after-load 'marginalia
    (defun project-ctags-tag-annotator (cand)
      (when-let (full-json (get-text-property 0 'full-json cand))
        (if-let (path (gethash "path" full-json))
            (marginalia--fields
             ((gethash "kind" full-json)
              :face 'marginalia-type :width 10)
             ((format "%s:%s"
                      (file-name-nondirectory path)
                      (gethash "line" full-json))
              :face 'marginalia-file-name :width 25)
             ((string-trim (or (gethash "pattern" full-json) ""))
              :face 'marginalia-function))
          (marginalia--fields
           ((gethash "kind" full-json)
            :face 'marginalia-type :width 10)
           ((number-to-string (gethash "line" full-json))
            :face 'marginalia-file-name :width 4)
           ((string-trim (or (gethash "pattern" full-json) ""))
            :face 'marginalia-function)))))

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
  )

;; Ctags completion ---------------------------------------------------------- ;

(provide 'init-imenu)

;;; init-imenu.el ends here
