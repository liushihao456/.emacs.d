;;; init-font.el --- Configurations for font	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for font.
;; --------------------------------------

;;; Code:

;; When in GUI, set fonts
(when (display-graphic-p)
  (when (and (boundp 'local-config-font) (boundp 'local-config-font-size))
    (add-to-list 'default-frame-alist
                 `(font . ,(format "%s-%s" local-config-font
                                   local-config-font-size)))
    (set-face-attribute 'fixed-pitch nil :family local-config-font)
    (set-face-attribute 'fixed-pitch-serif nil :family local-config-font)
    (when (boundp 'local-config-chinese-font)
      (set-fontset-font t 'han local-config-chinese-font))
    ;; Fix unicode font height bug on macOS
    (when (memq window-system '(mac ns))
      (set-fontset-font t 'unicode
			(format "Menlo-%s" local-config-font-size)))
    ))

(provide 'init-font)

;;; init-font.el ends here
