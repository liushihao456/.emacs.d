;;; jsts-mode.el --- Mode for JavaScript and TypeScript	-*- lexical-binding: t -*-

;; Author: Shihao Liu
;; Keywords: company popup documentation tip
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3") (s "1.9.0") (cl-lib "0.5") (dash "2.10.0") (company "0.8.9"))

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
;; Mode for JavaScript and TypeScript; supports React syntax.
;; --------------------------------------

;;; Code:
(require 'cl-lib)
(require 'cc-mode)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'tree-sitter-indent)

(defvar jsts-mode-indent-scopes
  '((indent-all . ;; these nodes are always indented
                ())
    (indent-body . ;; if parent node is one of these → indent to parent line's start col + offset
                 (
                  array
                  subscript_expression
                  object
                  arguments
                  statement_block
                  class_body
                  parenthesized_expression
                  jsx_element
                  jsx_fragment
                  jsx_opening_element
                  jsx_expression
                  switch_body
                  member_expression
                  formal_parameters
                  ternary_expression
                  variable_declarator
                  pair
                  jsx_self_closing_element
                  named_imports
                  object_pattern
                  array_pattern
                  if_statement
                  switch_case
                  assignment_expression
                  else_clause
                  return_statement
                  object_type))

    (indent-relative-to-parent . ;; if parent node is one of these -> indent to parent start column + offset
                               (arrow_function))
    (no-nesting . ;; if parent's node is same type as parent's parent, no indent
                (ternary_expression))
    (align-to-parent . ;; if parent node is one of these -> align to parent node's start column
                     (binary_expression
                      unary_expression))
    (aligned-siblings . ;; siblings (nodes with same parent and of same type) should be aligned to the first child
                      (jsx_attribute))
    (outdent . ;; these nodes always outdent (1 shift in opposite direction)
             (jsx_closing_element
              else_clause
              "<"
              ">"
              "/"
              ")"
              "}"
              "]")))
  "Scopes for indenting in typescript-react.")

(defun jsts-mode-goto-prev-tag ()
  "Go to the previous JSX tag."
  (interactive)
  (let ((echo-keystrokes nil))
    (unless
        (save-match-data
          (search-backward-regexp "\\(?:<>\\|<[^/>]+\\)" nil t))
      (message "No previous tag found"))
    (message "Goto tag: [n]ext [p]revious")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?n] 'jsts-mode-goto-next-tag)
       (define-key map [?p] 'jsts-mode-goto-prev-tag)
       map))))

(defun jsts-mode-goto-next-tag ()
  "Go to the next JSX tag."
  (interactive)
  (let ((echo-keystrokes nil))
    (unless
      (save-match-data
        (search-forward-regexp "\\(?:/>\\|</[[:alnum:][:space:]]*>\\)" nil t))
      (message "No next tag found"))
    (message "Goto tag: [n]ext [p]revious")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?n] 'jsts-mode-goto-next-tag)
       (define-key map [?p] 'jsts-mode-goto-prev-tag)
       map))))

;;; Syntax table and parsing

(defvar jsts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `jsts-mode'.")

;;; KeyMap

(defvar jsts-mode-map
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Keymap for `jsts-mode'.")

;;; Mode hook

(defcustom jsts-mode-hook nil
  "*Hook called by `jsts-mode'."
  :type 'hook
  :group 'jsts)

;;;###autoload
(define-derived-mode jsts-mode prog-mode
  (pcase (file-name-extension (buffer-file-name))
    ("ts" "TypeScript")
    ("tsx" "TypeScript[TSX]")
    ("js" "JavaScript")
    ("jsx" "JavaScript[JSX]"))
  "Major mode for editing JavaScript/TypeScript files; supports React syntax.

Key bindings:

\\{jsts-mode-map}"

  :group 'jsts
  :syntax-table jsts-mode-syntax-table

  (tree-sitter-hl-mode)
  (setq-local tree-sitter-indent-current-scopes jsts-mode-indent-scopes)
  (setq-local indent-line-function #'tree-sitter-indent-line)

  (define-key jsts-mode-map (kbd "C-c C-n") 'jsts-mode-goto-next-tag)
  (define-key jsts-mode-map (kbd "C-c C-p") 'jsts-mode-goto-prev-tag)

  (setq-local electric-pair-pairs
              (append electric-pair-pairs '((?\' . ?\')) '((?\` . ?\`))))
  (setq-local electric-pair-text-pairs electric-pair-pairs)
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))
  (setq-local electric-layout-rules
              '((?\; . after) (?\{ . after) (?\} . before)))

  ;; Yasnippet


  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  ;; for filling, pretend we're cc-mode
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "$"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  )

(provide 'jsts-mode)

;;; jsts-mode.el ends here
