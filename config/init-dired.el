;;; init-dired.el --- Dired configuration	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Dired configuration.
;; --------------------------------------

;;; Code:

;; ;; nerd-svg-icons-dired
;; (use-package nerd-svg-icons-dired
;;   :load-path "packages/nerd-svg-icons"
;;   :hook (dired-mode . nerd-svg-icons-dired-mode))

(use-package dired
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :ensure t
  :config
  (dirvish-override-dired-mode)

  ;; Override nerd-icons attribute using nerd-svg-icons
  (use-package nerd-svg-icons
    :load-path "packages/nerd-svg-icons"
    :demand t)
  (require 'dirvish-icons)
  (dirvish-define-attribute nerd-icons
    "File icons provided by `nerd-svg-icons.el'."
    :width (+ (length dirvish-icon-delimiter) 2)
    (let* ((face (when hl-face hl-face `(:face ,hl-face)))
           (icon (if (eq (car f-type) 'dir)
                     (apply #'nerd-svg-icons-icon-for-dir f-name face)
                   (apply #'nerd-svg-icons-icon-for-file f-str face)))
           (icon-str (concat icon (propertize dirvish-icon-delimiter 'face hl-face)))
           (ov (make-overlay (1- f-beg) f-beg)))
      (overlay-put ov 'after-string icon-str)
      `(ov . ,ov)))

  ;; Substree state chevrons using nerd-svg-icons
  (dirvish-define-attribute subtree-state-custom
    "A indicator for directory expanding state."
    :when (or dirvish-subtree-always-show-state dirvish-subtree--overlays)
    :width 1
    (let* ((nerd-svg-icons-icon-width 1)
           (state-str
            (propertize (if (eq (car f-type) 'dir)
                            (if (dirvish-subtree--expanded-p)
                                (nerd-svg-icons-icon-str "md-chevron_down" :face 'dirvish-subtree-state)
                              (nerd-svg-icons-icon-str "md-chevron_right" :face 'dirvish-subtree-state))
                          " ")))
           (ov (make-overlay (1+ l-beg) (1+ l-beg))))
      (when hl-face
        (add-face-text-property 0 1 hl-face t state-str))
      (overlay-put ov 'after-string state-str)
      `(ov . ,ov)))

  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state nerd-icons subtree-state-custom file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons subtree-state-custom))

  ;; Disable modeline
  (setq dirvish-use-header-line nil)
  (setq dirvish-use-mode-line nil)

  ;; (dirvish-peek-mode)             ; Preview files in minibuffer
  (setq dirvish-side-width 25)
  (dirvish-side-follow-mode)      ; similar to `dired-follow-mode'

  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)

  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c e" . dirvish-side)
   ("C-c d" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

(provide 'init-dired)

;;; init-dired.el ends here
