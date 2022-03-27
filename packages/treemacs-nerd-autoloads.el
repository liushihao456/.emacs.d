;;; treemacs-nerd-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "autodoc/autodoc" "autodoc/autodoc.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from autodoc/autodoc.el

(autoload 'autodoc-generate-docstring "autodoc/autodoc" "\
Generate docstring skeleton." t nil)

(autoload 'autodoc-mode "autodoc/autodoc" "\
Generates documentation automatically.

This is a minor mode.  If called interactively, toggle the
`Autodoc mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `autodoc-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "autodoc/autodoc" '("autodoc-"))

;;;***

;;;### (autoloads nil "cheat-sh/cheat-sh" "cheat-sh/cheat-sh.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from cheat-sh/cheat-sh.el

(autoload 'cheat-sh "cheat-sh/cheat-sh" "\
Look up THING on cheat.sh and display the result.

\(fn THING)" t nil)

(autoload 'cheat-sh-region "cheat-sh/cheat-sh" "\
Look up the text between START and END on cheat.sh.

\(fn START END)" t nil)

(autoload 'cheat-sh-maybe-region "cheat-sh/cheat-sh" "\
If region is active lookup content of region, otherwise prompt." t nil)

(autoload 'cheat-sh-help "cheat-sh/cheat-sh" "\
Get help on using cheat.sh." t nil)

(autoload 'cheat-sh-list "cheat-sh/cheat-sh" "\
Get a list of topics available on cheat.sh.

Either gets a topic list for subject THING, or simply gets a list
of all available topics on cheat.sh if THING is supplied as an
empty string.

\(fn THING)" t nil)

(autoload 'cheat-sh-search "cheat-sh/cheat-sh" "\
Search for THING on cheat.sh and display the result.

\(fn THING)" t nil)

(autoload 'cheat-sh-search-topic "cheat-sh/cheat-sh" "\
Search TOPIC for THING on cheat.sh and display the result.

\(fn TOPIC THING)" t nil)

(register-definition-prefixes "cheat-sh/cheat-sh" '("cheat-sh-"))

;;;***

;;;### (autoloads nil "company-tip/company-tip" "company-tip/company-tip.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-tip/company-tip.el

(autoload 'company-tip-local-mode "company-tip/company-tip" "\
Provides documentation popups for `company-mode' using `popup-tip'.

This is a minor mode.  If called interactively, toggle the
`Company-Tip-Local mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `company-tip-local-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'company-tip-mode 'globalized-minor-mode t)

(defvar company-tip-mode nil "\
Non-nil if Company-Tip mode is enabled.
See the `company-tip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-tip-mode'.")

(custom-autoload 'company-tip-mode "company-tip/company-tip" nil)

(autoload 'company-tip-mode "company-tip/company-tip" "\
Toggle Company-Tip-Local mode in all buffers.
With prefix ARG, enable Company-Tip mode if ARG is positive; otherwise, disable
it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Company-Tip-Local mode is enabled in all buffers where `company-tip-local-mode'
would do it.

See `company-tip-local-mode' for more information on Company-Tip-Local mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "company-tip/company-tip" '("company-tip-"))

;;;***

;;;### (autoloads nil "powerline/powerline" "powerline/powerline.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from powerline/powerline.el

(autoload 'powerline-hud "powerline/powerline" "\
Return an XPM of relative buffer location using FACE1 and FACE2 of optional WIDTH.

\(fn FACE1 FACE2 &optional WIDTH)" nil nil)

(autoload 'powerline-mouse "powerline/powerline" "\
Return mouse handler for CLICK-GROUP given CLICK-TYPE and STRING.

\(fn CLICK-GROUP CLICK-TYPE STRING)" nil nil)

(autoload 'powerline-concat "powerline/powerline" "\
Concatonate STRINGS and pad sides by spaces.

\(fn &rest STRINGS)" nil nil)

(autoload 'defpowerline "powerline/powerline" "\
Create function NAME by wrapping BODY with powerline padding an propetization.

\(fn NAME BODY)" nil t)

(autoload 'powerline-raw "powerline/powerline" "\
Render STR as mode-line data using FACE and optionally PAD import on left (l) or right (r).

\(fn STR &optional FACE PAD)" nil nil)

(autoload 'powerline-fill "powerline/powerline" "\
Return empty space using FACE and leaving RESERVE space on the right.

\(fn FACE RESERVE)" nil nil)
 (autoload 'powerline-major-mode "powerline")
 (autoload 'powerline-minor-modes "powerline")
 (autoload 'powerline-narrow "powerline")
 (autoload 'powerline-vc "powerline")
 (autoload 'powerline-encoding "powerline")
 (autoload 'powerline-buffer-size "powerline")
 (autoload 'powerline-buffer-id "powerline")
 (autoload 'powerline-process "powerline")
 (autoload 'powerline-selected-window-active "powerline")

(register-definition-prefixes "powerline/powerline" '("pl/" "powerline-"))

;;;***

;;;### (autoloads nil "powerline/powerline-separators" "powerline/powerline-separators.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from powerline/powerline-separators.el

(register-definition-prefixes "powerline/powerline-separators" '("pl/" "powerline-image-apple-rgb"))

;;;***

;;;### (autoloads nil "powerline/powerline-themes" "powerline/powerline-themes.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from powerline/powerline-themes.el

(autoload 'powerline-default-theme "powerline/powerline-themes" "\
Setup the default mode-line." t nil)

(autoload 'powerline-center-theme "powerline/powerline-themes" "\
Setup a mode-line with major and minor modes centered." t nil)

(autoload 'powerline-vim-theme "powerline/powerline-themes" "\
Setup a Vim-like mode-line." t nil)

(autoload 'powerline-nano-theme "powerline/powerline-themes" "\
Setup a nano-like mode-line." t nil)

(register-definition-prefixes "powerline/powerline-themes" '("powerline-"))

;;;***

;;;### (autoloads nil "prettier-js/prettier-js" "prettier-js/prettier-js.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from prettier-js/prettier-js.el

(autoload 'prettier-js "prettier-js/prettier-js" "\
Format the current buffer according to the prettier tool." t nil)

(autoload 'prettier-js-mode "prettier-js/prettier-js" "\
Runs prettier on file save when this mode is turned on

This is a minor mode.  If called interactively, toggle the
`Prettier-Js mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `prettier-js-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "prettier-js/prettier-js" '("prettier-js-"))

;;;***

;;;### (autoloads nil "react-mode/react-mode" "react-mode/react-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from react-mode/react-mode.el

(autoload 'react-mode "react-mode/react-mode" "\
Major mode for editing typescript-react (.react).

Key bindings:

\\{react-mode-map}

\(fn)" t nil)

(register-definition-prefixes "react-mode/react-mode" '("react-mode-"))

;;;***

;;;### (autoloads nil "react-mode/tree-sitter-indent" "react-mode/tree-sitter-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from react-mode/tree-sitter-indent.el

(autoload 'tree-sitter-indent-line "react-mode/tree-sitter-indent" "\
Use Tree-sitter as backend to indent current line." nil nil)

(register-definition-prefixes "react-mode/tree-sitter-indent" '("tree-sitter-indent-"))

;;;***

;;;### (autoloads nil "treemacs-nerd/treemacs-nerd" "treemacs-nerd/treemacs-nerd.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-nerd/treemacs-nerd.el

(autoload 'treemacs-nerd-config "treemacs-nerd/treemacs-nerd" "\
Install treemacs-nerd theme configuration." nil nil)

;;;***

;;;### (autoloads nil "wgrep/wgrep" "wgrep/wgrep.el" (0 0 0 0))
;;; Generated autoloads from wgrep/wgrep.el

(autoload 'wgrep-setup "wgrep/wgrep" "\
Setup wgrep preparation." nil nil)

(add-hook 'grep-setup-hook 'wgrep-setup)

(register-definition-prefixes "wgrep/wgrep" '("wgrep-"))

;;;***

;;;### (autoloads nil "wgrep/wgrep-ack" "wgrep/wgrep-ack.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from wgrep/wgrep-ack.el

(autoload 'wgrep-ack-and-a-half-setup "wgrep/wgrep-ack" nil nil nil)

(autoload 'wgrep-ack-setup "wgrep/wgrep-ack" nil nil nil)

(add-hook 'ack-and-a-half-mode-hook 'wgrep-ack-and-a-half-setup)

(add-hook 'ack-mode-hook 'wgrep-ack-setup)

(register-definition-prefixes "wgrep/wgrep-ack" '("wgrep-ack-"))

;;;***

;;;### (autoloads nil "wgrep/wgrep-ag" "wgrep/wgrep-ag.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from wgrep/wgrep-ag.el

(autoload 'wgrep-ag-setup "wgrep/wgrep-ag" nil nil nil)

(add-hook 'ag-mode-hook 'wgrep-ag-setup)

(register-definition-prefixes "wgrep/wgrep-ag" '("wgrep-ag-"))

;;;***

;;;### (autoloads nil "wgrep/wgrep-deadgrep" "wgrep/wgrep-deadgrep.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from wgrep/wgrep-deadgrep.el

(autoload 'wgrep-deadgrep-setup "wgrep/wgrep-deadgrep" "\
Setup `wgrep-deadgrep' for `deadgrep'." nil nil)

(add-hook 'deadgrep-finished-hook 'wgrep-deadgrep-setup)

(register-definition-prefixes "wgrep/wgrep-deadgrep" '("wgrep-deadgrep-"))

;;;***

;;;### (autoloads nil "wgrep/wgrep-helm" "wgrep/wgrep-helm.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from wgrep/wgrep-helm.el

(autoload 'wgrep-helm-setup "wgrep/wgrep-helm" nil nil nil)

(add-hook 'helm-grep-mode-hook 'wgrep-helm-setup)

(add-hook 'helm-occur-mode-hook 'wgrep-helm-setup)

(register-definition-prefixes "wgrep/wgrep-helm" '("wgrep-helm-"))

;;;***

;;;### (autoloads nil "wgrep/wgrep-pt" "wgrep/wgrep-pt.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from wgrep/wgrep-pt.el

(autoload 'wgrep-pt-setup "wgrep/wgrep-pt" nil nil nil)

(add-hook 'pt-search-mode-hook 'wgrep-pt-setup)

(register-definition-prefixes "wgrep/wgrep-pt" '("wgrep-pt-unload-function"))

;;;***

;;;### (autoloads nil "wgrep/wgrep-subtest" "wgrep/wgrep-subtest.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from wgrep/wgrep-subtest.el

(register-definition-prefixes "wgrep/wgrep-subtest" '("wgrep-test--ag"))

;;;***

;;;### (autoloads nil "wgrep/wgrep-test" "wgrep/wgrep-test.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from wgrep/wgrep-test.el

(register-definition-prefixes "wgrep/wgrep-test" '("wgrep-test"))

;;;***

;;;### (autoloads nil nil ("cheat-sh/cheat-sh-autoloads.el" "company-tip/company-tip-autoloads.el"
;;;;;;  "powerline/powerline-autoloads.el" "prettier-js/prettier-js-autoloads.el"
;;;;;;  "react-mode/react-mode-autoloads.el" "spacemacs-icon/spacemacs-icon-autoloads.el"
;;;;;;  "wgrep/wgrep-autoloads.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; treemacs-nerd-autoloads.el ends here
