;;; tsx-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tree-sitter-indent" "tree-sitter-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from tree-sitter-indent.el

(autoload 'tree-sitter-indent-line "tree-sitter-indent" "\
Use Tree-sitter as backend to indent current line." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tree-sitter-indent" '("tree-sitter-indent-")))

;;;***

;;;### (autoloads nil "tsx-mode" "tsx-mode.el" (0 0 0 0))
;;; Generated autoloads from tsx-mode.el

(autoload 'tsx-mode "tsx-mode" "\
Major mode for editing typescript-react (.tsx).

Key bindings:

\\{tsx-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tsx-mode" '("tsx-mode-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tsx-mode-autoloads.el ends here
