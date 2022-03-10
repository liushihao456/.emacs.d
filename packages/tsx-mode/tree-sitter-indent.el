;;; tree-sitter-indent.el --- Provide indentation with a Tree-sitter backend -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.12.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use Tree-sitter as backend to indent source code.
;;
;; Provide an `indent-line-function` using the emacs-tree-sitter package
;; Usage (e.g., for TSX):
;;
;; (require 'tree-sitter-indent)
;; (setq-local tree-sitter-indent-current-scopes tsx-mode-indent-scopes)
;; (setq-local indent-line-function #'tree-sitter-indent-line)
;;
;; The indent scopes is an alist, whose keys include:
;; 1. indent-all, whose nodes will be indented one ``tree-sitter-indent-offset''
;;    forward relative to the indentation level of the parent node.
;; 2. indent-body, meaning any node whose parent node is one of these will be
;;    indented one ``tree-sitter-indent-offset'' forward relative to the
;;    indentation level of the parent node.
;; 3. indent-relative-to-parent, meaning any node whose parent node is one of
;;    these will be indented one ``tree-sitter-indent-offset'' forward relative
;;    to the column of the parent node's start-position.
;; 4. no-nesting, meaning any node whose parent node and parent's parent node
;;    are both one of these, will not be indented forward, even if its parent
;;    node is one of indent-body's nodes.
;;    This is used for situations such as nested ternary_expressions.
;; 5. align-to-parent, meaning any node whose parent node is one of these will
;;    be aligned to the parent node's start column.
;; 6. aligned-siblings, whose nodes will be aligned to their siblings (nodes
;;    with the same parent and of the same type).
;; 7. outdent, whose nodes will be outdented one ``tree-sitter-indent-offset''
;;    backward relative to what they would otherwise.
;;
;; And the indent scopes alist's values are node types, e.g.,
;; (statement_block class_body).

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tree-sitter)

(defgroup tree-sitter-indent nil "Indent lines using Tree-sitter as backend"
  :group 'tree-sitter)

(defvar-local tree-sitter-indent-offset 4
  "Indent offset to be used by major modes")

(defvar-local tree-sitter-indent-current-scopes nil
  "Current scopes in use for tree-sitter-indent.")

;;;; Private functions
(defun tree-sitter-indent--node-is-indent-all (node)
  "Non-nil if NODE type is in indent-all group.

Nodes in this group will be always +1 indentend."
  (let-alist tree-sitter-indent-current-scopes
    (and node
         (member (tsc-node-type node)
                 .indent-all))))

(defun tree-sitter-indent--node-is-indent-rest (node)
  "Non-nil if NODE type is in indent-rest group.

Nodes in this group will +1 indent offset if they are a non-first child of
parent node."
  (let-alist tree-sitter-indent-current-scopes
    (and node
         (member (tsc-node-type node)
                 .indent-rest))))

(defun tree-sitter-indent--node-is-indent-body (node)
  "Non-nil if NODE type is in indent-body group.

Nodes in this group will +1 indent offset."
  (let-alist tree-sitter-indent-current-scopes
    (and node
         (member (tsc-node-type node)
                 .indent-body))))

(defun tree-sitter-indent--node-is-indent-relative-to-parent (node)
  "Non-nil if NODE type is in indent-relative-to-parent group.

Nodes in this group will indent to parent's start column + offset."
  (let-alist tree-sitter-indent-current-scopes
    (and node
         (member (tsc-node-type node)
                 .indent-relative-to-parent))))

(defun tree-sitter-indent--node-is-no-nesting (node)
  "Non-nil if NODE type is in no-nesting group.

Nodes in this group will be always +1 indentend."
  (let-alist tree-sitter-indent-current-scopes
    (and node
         (tsc-get-parent node)
         (member (tsc-node-type node)
                 .no-nesting)
         (member (tsc-node-type (tsc-get-parent node))
                 .no-nesting))))

(defun tree-sitter-indent--node-is-align-to-parent (node)
  "Non-nil if NODE type is in align-to-parent group.

Nodes in this group will indent to parent's start column."
  (let-alist tree-sitter-indent-current-scopes
    (and node
         (member (tsc-node-type node)
                 .align-to-parent))))

(defun tree-sitter-indent--node-is-aligned-sibling (node)
  "Non-nil if NODE type is in aligned-siblings group.

Nodes in this group will be aligned to the column of the first sibling."
  (let-alist tree-sitter-indent-current-scopes
    (member (tsc-node-type node)
            .aligned-siblings)))

(defun tree-sitter-indent--node-is-outdent (node)
  "Return non-nil if NODE outdents per SCOPES.

NODE is tested if it belongs into the \"outdent\" group in SCOPES."
  (let-alist tree-sitter-indent-current-scopes
    (member (tsc-node-type node)
            .outdent)))

(defun tree-sitter-indent--highest-node-at-position (position)
  "Get the node at buffer POSITION that's at the highest level.

POSITION is a byte position in buffer like \\(point-min\\)."
  (save-excursion
    (goto-char position)
    ;; maybe implement this as a cl-loop
    (let* ((current-node (tree-sitter-node-at-point)))
      ;; move upwards until we either don't have aparent node
      ;; or we moved out of line
      (while (and
	          current-node
	          (when-let* ((parent-node (tsc-get-parent current-node)))
                (when (and ;; parent and current share same position
                       (eq (tsc-node-start-byte parent-node)
                           (tsc-node-start-byte current-node)))
		          ;; move upwards to the parent node
		          (setq current-node parent-node)))))
      current-node)))

(defun tree-sitter-indent--first-sibling-column (current-node parent-node)
  "Column position for CURRENT-NODE's first sibling.

If CURRENT-NODE belongs to the aligned-siblings group, will look up the first
sibling in same group \\(running through PARENT-NODE's children) and return
its column.

TREE-SITTER-INDENT-CURRENT-SCOPES is used to test whether
CURRENT-NODE belongs to the aligned-siblings group."
  (when (and parent-node
             (tree-sitter-indent--node-is-aligned-sibling current-node))
    (when-let* ((current-node-type
                 (tsc-node-type current-node))
                (first-sibling
                 (cl-loop for ith-sibling = (tsc-get-nth-child parent-node 0)
                          then (tsc-get-next-sibling ith-sibling)
                          while ith-sibling
                          if (equal current-node-type
                                    (tsc-node-type ith-sibling))
                          return ith-sibling
                          end)))
      (unless (tsc-node-eq current-node first-sibling)
        (cdr (tsc-node-start-point first-sibling))))))

(defun tree-sitter-indent--string-nonempty-p (str)
  "Return t if STR consists not only of whitespaces."
  (save-match-data
    (string-match "[^ \t\n\r]+" str)))

(defun tree-sitter-indent--node-nonempty-p (node)
  "Return t if text in NODE consists not only of whitespaces."
  (tree-sitter-indent--string-nonempty-p (tsc-node-text node)))

(defun tree-sitter-indent--get-prev-nonempty-sibling (node)
  "Get the previous sibling of NODE that is non-empty.

This wrapper function exists because some nodes might contain only white spaces,
e.g., ``jsx_text''."
  (cl-loop for prev-sibling = (tsc-get-prev-sibling node)
           then (tsc-get-prev-sibling prev-sibling)
           when (or
                 (not prev-sibling)
                 (tree-sitter-indent--node-nonempty-p prev-sibling))
           return prev-sibling))

(defun tree-sitter-indent--get-next-nonempty-sibling (node)
  "Get the next sibling of NODE that is non-empty.

This wrapper function exists because some nodes might contain only white spaces,
e.g., ``jsx_text''."
  (cl-loop for next-sibling = (tsc-get-next-sibling node)
           then (tsc-get-next-sibling next-sibling)
           when (or
                 (not next-sibling)
                 (tree-sitter-indent--node-nonempty-p next-sibling))
           return next-sibling))

(defun tree-sitter-indent--get-node-start-line-first-char-col (node)
  "Get the column of the first char on NODE's start line."
  (if node
      (let ((start-pos (tsc-node-start-position node)))
        (save-excursion
          (goto-char start-pos)
          ;; (forward-line line-num)
          (beginning-of-line)
          (skip-chars-forward " \t")
          (current-column)))
    0))

(defun tree-sitter-indent--indent-node (current-node)
  "Indent CURRENT-NODE, and return the indentation column."
  (let* ((previous-node
          (tree-sitter-indent--get-prev-nonempty-sibling current-node))
         (next-node
          (tree-sitter-indent--get-next-nonempty-sibling current-node))
         (parent-node
          (tsc-get-parent current-node))
         (same-line-with-parent-node-p
          (when parent-node
            (equal (car (tsc-node-start-point parent-node))
                   (car (tsc-node-start-point current-node)))))
         (same-line-with-prev-node-p
          (when previous-node
            (equal (car (tsc-node-end-point previous-node))
                   (car (tsc-node-start-point current-node)))))
         (current-node-must-indent
          (tree-sitter-indent--node-is-indent-all current-node))
         (current-node-must-outdent
          (tree-sitter-indent--node-is-outdent current-node))
         (sibling-column
          (tree-sitter-indent--first-sibling-column
           current-node
           parent-node))
         (parent-line-column
          (tree-sitter-indent--get-node-start-line-first-char-col parent-node))
         (parent-column
          (cdr (tsc-node-start-point parent-node)))
         (relative-to-parent-column
          (when (and
                 parent-node
                 (tree-sitter-indent--node-is-indent-relative-to-parent parent-node))
            (+ parent-column
               (if (or
                    current-node-must-outdent
                    (tree-sitter-indent--node-is-no-nesting parent-node))
                   0
                 tree-sitter-indent-offset))))
         (relative-to-parent-line
          (when (or current-node-must-indent
                    (and
                     parent-node
                     (tree-sitter-indent--node-is-indent-body parent-node)))
            (+ parent-line-column
               (if (or
                    current-node-must-outdent
                    (tree-sitter-indent--node-is-no-nesting parent-node))
                   0
                 tree-sitter-indent-offset)))))
    (cond
     (same-line-with-parent-node-p
      parent-line-column)
     (same-line-with-prev-node-p
      parent-line-column)
     ((tree-sitter-indent--node-is-align-to-parent parent-node)
      parent-column)
     ((numberp sibling-column)
      sibling-column)
     ((numberp relative-to-parent-column)
      relative-to-parent-column)
     ((numberp relative-to-parent-line)
      relative-to-parent-line)
     (current-node-must-outdent
      (max
       0
       (- parent-line-column tree-sitter-indent-offset)))
     (t
      parent-line-column))))

(cl-defun tree-sitter-indent--indent-column ()
  "Return the column the current line should indent to."
  (let* ((indent-node
          (tree-sitter-indent--highest-node-at-position
           (save-excursion
             (back-to-indentation)
             (point))))
         (indent-col
          (if indent-node
              (tree-sitter-indent--indent-node indent-node)
            0))
         (first-char-white-space-p
          (memq (save-excursion
                  (back-to-indentation)
                  (char-after (point)))
                '(?\s ?\C-j ?\C-i))))
    (if (and
         first-char-white-space-p
         indent-node
         (tree-sitter-indent--node-is-indent-body indent-node))
        (+ indent-col tree-sitter-indent-offset)
      indent-col)))

;;;; Public API

;;;###autoload
(defun tree-sitter-indent-line ()
  "Use Tree-sitter as backend to indent current line."
  (cl-assert tree-sitter-indent-current-scopes)
  (let* ((original-position
          (point))
         (first-non-blank-pos ;; see savep in `smie-indent-line'
          (save-excursion
            (forward-line 0)
            (skip-chars-forward " \t")
            (point)))
         (should-save-excursion
          (< first-non-blank-pos original-position))
         (new-column
          (tree-sitter-indent--indent-column)))
    (when (numberp new-column)
      (if should-save-excursion
          (save-excursion (indent-line-to new-column))
        (indent-line-to new-column)))))

(define-minor-mode tree-sitter-indent-mode
  "Use Tree-sitter as backend for indenting buffer."
  :init-value nil :lighter nil :keymap nil
  (cond
   (tree-sitter-indent-mode
    (unless tree-sitter-mode
      ;; ensure that tree-sitter-mode is activated
      (tree-sitter-mode))
    (setq-local indent-line-function
                #'tree-sitter-indent-line)
    (setq-local tree-sitter-indent-offset
                (thread-last major-mode
                  (symbol-name)
                  (replace-regexp-in-string (rx "-mode") "")
                  (format "%s-indent-offset")
                  (intern)
                  (symbol-value)))
    (setq-local tree-sitter-indent-current-scopes
                (thread-last major-mode
                  (symbol-name)
                  (replace-regexp-in-string (rx "-mode") "")
                  (format "tree-sitter-indent-%s-scopes")
                  (intern)
                  (symbol-value))))
   (t
    (setq-local indent-line-function (default-value 'indent-line-function)))))

(provide 'tree-sitter-indent)
;;; tree-sitter-indent.el ends here
