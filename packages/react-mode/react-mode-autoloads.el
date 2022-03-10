;;; react-mode-autoloads.el --- automatically extracted autoloads
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

;;;### (autoloads nil "react-mode" "react-mode.el" (0 0 0 0))
;;; Generated autoloads from react-mode.el

(autoload 'react-mode "react-mode" "\
Major mode for editing typescript-react (.react).

Key bindings:

\\{react-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "react-mode" '("react-mode-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; react-mode-autoloads.el ends here
