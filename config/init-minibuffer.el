;;; init-minibuffer.el --- Minibuffer configurations	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Minibuffer configurations.
;; --------------------------------------

;;; Code:

(savehist-mode)

;; Vertico ------------------------------------------------------------------- ;

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
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

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Orderless ----------------------------------------------------------------- ;

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  (setq orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-initialism
                                    orderless-flex))

  (defun my/orderless-literal-if-suffix-bang (pattern index _total)
    (if (string-suffix-p "!" pattern)
        `(orderless-literal . ,(substring pattern 0 -1))))
  (setq orderless-style-dispatchers '(my/orderless-literal-if-suffix-bang)))

;; Marginalia ---------------------------------------------------------------- ;

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :config
  (setq marginalia-separator "  ")

  (defun marginalia-annotate-buffer-a (cand)
    "Annotate buffer CAND with modification status, file name and major mode."
    (when-let* ((buffer (get-buffer cand)))
      (marginalia--fields
       ((marginalia--buffer-status buffer))
       ((marginalia--buffer-file buffer)
        :face 'marginalia-file-name))))
  (advice-add #'marginalia-annotate-buffer :override #'marginalia-annotate-buffer-a)

  (defun marginalia-annotate-bookmark-a (cand)
    "Annotate bookmark CAND with its file name and front context string."
    (when-let* ((bm (assoc cand (bound-and-true-p bookmark-alist))))
      (let ((front (bookmark-get-front-context-string bm)))
        (marginalia--fields
         ((marginalia--bookmark-type bm) :width 10 :face 'marginalia-type)
         ((unless (or (not front) (string= front ""))
            (concat (string-trim
                     (replace-regexp-in-string
                      "[ \t]+" " "
                      (replace-regexp-in-string "\n" "\\\\n" front)))
                    (marginalia--ellipsis)))
          :width 25 :face 'marginalia-documentation)
         ((bookmark-get-filename bm)
          :face 'marginalia-file-name)))))
  (advice-add #'marginalia-annotate-bookmark :override #'marginalia-annotate-bookmark-a)

  ;; Hack: when candidates are truncated for example by advicing
  ;; `vertico--format-candidate', we can't access the truncated candidates
  ;; here. Therefore use my custom 'truncate-to property of candidates if
  ;; present. (The ctags candidates are marked with this property, as can be
  ;; seen below.)
  (defun marginalia--align-a (cands)
    "Align annotations of CANDS according to `marginalia-align'."
    (when (and cands (> (length cands) 0))
      (setq marginalia--candw-max
            (seq-max (cl-loop for (cand . ann) in cands collect
                              (if-let* ((truncate-to (get-text-property 0 'truncate-to cand)))
                                  (min (string-width cand) truncate-to)
                                (string-width cand))))))
    (cl-loop for (cand . ann) in cands collect
             (progn
               (when-let* ((align (text-property-any 0 (length ann) 'marginalia--align t ann)))
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
  (advice-add #'marginalia--align :override #'marginalia--align-a))

;; Icons --------------------------------------------------------------------- ;

(use-package nerd-svg-icons-completion
  :load-path "packages/nerd-svg-icons"
  :commands nerd-svg-icons-completion-mode
  :init (nerd-svg-icons-completion-mode))

;; Embark -------------------------------------------------------------------- ;

(use-package embark
  :ensure t
  :bind (("C-j" . embark-act)
         ("C-q" . embark-export)
         :map minibuffer-mode-map
         ("C-j" . embark-act))
  :config
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
  (define-key embark-bookmark-map (kbd "3") (my/embark-split-action bookmark-jump split-window-right)))

(use-package consult
  :ensure t
  :bind ("C-c p s" . consult-ripgrep)
  :config
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
  (setq consult-preview-key "C-o"))

(use-package embark-consult
  :ensure t
  :after (consult embark))

;; Recentf files completion -------------------------------------------------- ;

(defun recentf-open-files-compl ()
  "Find recentf files with `completing-read'."
  (interactive)
  (let* ((count 0)
         (file-list (mapcar
                     (lambda (x)
                       (cons
                        (format "%s%s"
                                (propertize (if (file-directory-p x)
                                                (concat (file-name-nondirectory
                                                         (directory-file-name x)) "/")
                                              (file-name-nondirectory x))
                                            'full x)
                                (propertize (number-to-string (setq count (1+ count)))
                                            'invisible t))
                        x))
                     recentf-list))
         (fname (completing-read "File name: "
                                 (lambda (str pred action)
                                   (if (eq action 'metadata)
                                       '(metadata
                                         (category . recentf-file)
                                         (cycle-sort-function . identity)
                                         (display-sort-function . identity))
                                     (complete-with-action
                                      action file-list str pred))))))
    (when fname (find-file (cdr (assoc fname file-list))))))
(global-set-key (kbd "C-c f r") 'recentf-open-files-compl)

(use-package nerd-svg-icons
  :defer t
  :config
  (defun nerd-svg-icons-completion-get-recentf-file-icon (cand)
    "Return the icon for the candidate CAND of completion category recentf-file."
    (let ((real-cand (substring cand 0 (next-single-property-change 0 'invisible cand))))
      (nerd-svg-icons-completion-get-file-icon real-cand)))

  (add-to-list 'nerd-svg-icons-completion-category-icon-alist
               '(recentf-file . nerd-svg-icons-completion-get-recentf-file-icon)))

(use-package marginalia
  :defer t
  :config
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
               '(recentf-file marginalia--recentf-file-annotator builtin none)))

;; Switch to buffer in the current project ----------------------------------- ;

(global-set-key (kbd "C-c p b") 'project-switch-to-buffer)

;; Insert symbol at point into minibuffer ------------------------------------ ;

(defun minibuffer-insert-symbol-at-point ()
  "Get symbol at point in original buffer and insert it to minibuffer."
  (interactive)
  (let (symbol)
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (setq symbol (thing-at-point 'symbol)))
    (when symbol (insert symbol))))

(defun my-minibuffer-setup-hook ()
  (local-set-key (kbd "C-w") 'minibuffer-insert-symbol-at-point))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
