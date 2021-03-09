;;; tsx-mode.el --- Mode for typescript and tsx	-*- lexical-binding: t -*-

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
;; Mode for typescript and tsx
;; --------------------------------------

;;; Code:
(require 'cc-mode)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'tree-sitter-indent)

(defvar tsx-mode-indent-scopes
  '((indent-all . ;; these nodes are always indented
                (
                 break_statement
                 ))
    (indent-rest . ;; if parent node is one of these and node is not first → indent
                 (
                  variable_declarator
                  member_expression
                  pair
                  unary_expression
                  binary_expression
                  ternary_expression
                  jsx_self_closing_element
                  ))
    (indent-body . ;; if parent node is one of these and current node is in middle → indent
                 (
                  array
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

                  named_imports
                  object_pattern
                  array_pattern
                  if_statement
                  switch_case

                  ;; comment
                  ;; subscript_expression
                  ;; lexical_declaration
                  ;; assignment_expression
                  ;; expression_statement
                  ;; return_statement
                  ;; arrow_function
                  ;; call_expression
                  ;; formal_parameters
                  ))

    (paren-indent . ;; if parent node is one of these → indent to paren opener
                  ())
    (align-char-to . ;; chaining char → node types we move parentwise to find the first chaining char
                   (;; "."
                    ))
    (aligned-siblings . ;; siblings (nodes with same parent) should be aligned to the first child
                      (jsx_attribute
                       jsx_closing_element
                       "<"
                       ))

    (multi-line-text . ;; if node is one of these, then don't modify the indent
                     ;; this is basically a peaceful way out by saying "this looks like something
                     ;; that cannot be indented using AST, so best I leave it as-is"
                     ())
    (outdent . ;; these nodes always outdent (1 shift in opposite direction)
             (
              "/"
              ;; ")"
              ;; "}"
              ;; "]"
              ))
    )
  "Scopes for indenting in typescript-react.")

;;; Syntax table and parsing

(defvar tsx-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `tsx-mode'.")

;;; KeyMap

(defvar tsx-mode-map
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Keymap for `tsx-mode'.")

;;; Mode hook

(defcustom tsx-mode-hook nil
  "*Hook called by `tsx-mode'."
  :type 'hook
  :group 'tsx)

;;;###autoload
(define-derived-mode tsx-mode prog-mode "Typescript(tsx)"
  "Major mode for editing typescript-react (.tsx).

Key bindings:

\\{tsx-mode-map}"

  :group 'tsx
  :syntax-table tsx-mode-syntax-table

  (tree-sitter-hl-mode)
  (setq-local tree-sitter-indent-current-scopes tsx-mode-indent-scopes)
  (setq-local indent-line-function #'tree-sitter-indent-line)

  ;; (setq-local electric-indent-chars
  ;;             (append "{}():;," electric-indent-chars))
  ;; (setq-local electric-layout-rules
  ;;             '((?\; . after) (?\{ . after) (?\} . before)))

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

(provide 'tsx-mode)

;;; tsx-mode.el ends here
