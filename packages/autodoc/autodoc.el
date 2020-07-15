;;; autodoc.el --- Auto insert docstrings for functions  -*- lexical-binding: t; -*-

;; Author: Shihao Liu <liushihao@pku.edu.com>
;; Keywords: docstring generate
;; Package-Requires: ((s "1.9.0") (cl-lib "0.5") (dash "2.10.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides functions for inserting docstring skeletons for
;; various languages, including Java, Javascript, Typescript, Python,
;; C/C++.  The styles of the docstring may vary for different languages.

;;; Code:

(require 'yasnippet)
(require 'dash)

(defconst autodoc--end-of-defun-pattern
  '(:python
    ":$"
    :cc
    ")"))
(defconst autodoc--defun-regex
  '(:python
    "def \\([[:alnum:]_]+\\)[[:space:]\n]*(\\([^)]*\\))\\(?:[[:space:]\n]*->[[:space:]\n]*\\([^):]+\\)\\)? *:$"
    :cc
    "\\(?:^\\|[[:space:]]\\)+\\([][<>[:alnum:]_]+\\)[[:space:]\n]+\\([[:alnum:]_]+\\)+[[:space:]\n]*(\\([^)]*\\))"))

(defconst autodoc--docstring-begin-regex "\"\"\"[[:space:]\n]*")
(defconst autodoc--docstring-end-regex "[[:space:]\n]*\"\"\"")

(defun autodoc--get-docstring-existing ()
  "Get the existing docstring.  Cursor has to be at the end of function declaration."
  (skip-chars-forward "[:space:]\n")
  (when (looking-at-p autodoc--docstring-begin-regex)
    (save-match-data
      (search-forward-regexp autodoc--docstring-begin-regex)
      (push-mark)
      (search-forward-regexp autodoc--docstring-end-regex)
      (skip-chars-backward "\"[:space:]\n"))
    (buffer-substring-no-properties (mark) (point))))

(defun autodoc--generate-docstring-template-cc ()
  "Generate the docstring template for cc language methods.
The ``match-data'' has to be set to the result of
applying ``autodoc--defun-regex'' when calling this function."
  (let ((params (match-string 3))
        (rtype (match-string 1))
        (doc-lines (list " \* ${1:[Summary]}" "/\*\*"))
        (n 1))
    (when params
      (!cons " \*" doc-lines)
      (dolist (param-type (mapcar 'string-trim (split-string params ",")))
        (let ((param-type-list (mapcar 'string-trim (split-string param-type "[ \f\t\n\r\v]+"))))
          (!cons (concat " \* @param " (nth 1 param-type-list) (format " ${%d:[ParamDescription]}" (setq n (1+ n))))
                 doc-lines))))
    (unless (string= rtype "void")
      (when (eq (length doc-lines) 2)
        (!cons " \*" doc-lines))
      (!cons (format " \* @return ${%d:[ReturnDescription]}" (setq n (1+ n))) doc-lines))
    (!cons " \*/" doc-lines)
    (string-join (nreverse doc-lines) "\n")))

(defun autodoc--generate-docstring-template-python ()
  "Generate the docstring template for python functions.
The ``match-data'' has to be set to the result of
applying ``autodoc--defun-regex'' when calling this function."
  (let ((params (match-string 2))
        (rtype (match-string 3))
        (doc-lines (list "${1:[Summary]}" "\"\"\""))
        (n 1))
    (when (and params (not (string= (string-trim params) "self")))
      (!cons "" doc-lines)
      (dolist (param-type (mapcar 'string-trim (split-string params ",")))
        (let ((param-type-list (mapcar 'string-trim (split-string param-type ":"))))
          (unless (string= (car param-type-list) "self")
            (!cons (concat ":param " (car param-type-list) (format ": ${%d:[ParamDescription]}" (setq n (1+ n))))
                   doc-lines)
            (!cons (concat ":type " (car param-type-list) ": "
                           (or (nth 1 param-type-list) (format "${%d:[ParamType]}" (setq n (1+ n))))
                           (format "${%d:(, optional)}" (setq n (1+ n))))
                   doc-lines)))))
    (setcar doc-lines (concat (car doc-lines) (format "${%d:" (setq n (1+ n)))))
    (!cons "" doc-lines)
    (!cons (format ":return: ${%d:[ReturnDescription]}" (setq n (1+ n))) doc-lines)
    (!cons (concat ":rtype: " (or rtype (format "${%d:[ReturnType]}" (setq n (1+ n)))) "}") doc-lines)
    (!cons "\"\"\"" doc-lines)
    (string-join (nreverse doc-lines) "\n")))

(defun autodoc--generate-docstring-template ()
  "Generate the docstring template for function.
The ``match-data'' has to be set to the result of
applying ``autodoc--defun-regex'' when calling this function."
  (cond
   ((eq major-mode 'python-mode) (autodoc--generate-docstring-template-python))
   ((eq major-mode 'java-mode) (autodoc--generate-docstring-template-cc))))

(defun autodoc--goto-docstring-insertion-point ()
  "Go to the insertion point of the docstring.
Cursor has to be at the beginning of function declaration when calling this
function."
  (autodoc--goto-end-of-defun-pattern)
  (cond
   ((eq major-mode 'python-mode)
    (newline-and-indent))
   ((memq major-mode '(java-mode c-mode c++-mode))
    (autodoc--search-backward-defun)
    (goto-char (point-at-bol))
    (newline)
    (forward-line -1)
    (indent-for-tab-command))))

(defun autodoc--goto-end-of-defun-pattern ()
  "Go to the next ending pattern of defun."
  (goto-char (point-at-bol))
  (cond
   ((eq major-mode 'python-mode)
    (search-forward-regexp (plist-get autodoc--end-of-defun-pattern :python) nil t))
   ((eq major-mode 'java-mode)
    (search-forward-regexp (plist-get autodoc--end-of-defun-pattern :cc) nil t))))

(defun autodoc--search-backward-defun ()
  "Search forward for defun."
  (cond
   ((eq major-mode 'python-mode)
    (search-backward-regexp (plist-get autodoc--defun-regex :python)))
   ((eq major-mode 'java-mode)
    (search-backward-regexp (plist-get autodoc--defun-regex :cc)))))

;;;###autoload
(defun autodoc-generate-docstring ()
  "Generate docstring skeleton."
  (interactive)
  (let ((initial-pos (point)))
    (autodoc--goto-end-of-defun-pattern)
    (if (autodoc--search-backward-defun)
        (let* ((docstring-template (autodoc--generate-docstring-template))
               (docstring-existing (autodoc--get-docstring-existing)))
          (autodoc--goto-docstring-insertion-point)
          (yas-expand-snippet docstring-template))
      (goto-char initial-pos))))


(defvar autodoc-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c M-d") 'autodoc-generate-docstring)
    m))

(defun autodoc--enable ()
  "Enable autodoc."
  (setq-local yas-inhibit-overlay-modification-protection t))

;;;###autoload
(define-minor-mode autodoc-mode
  "Generates documentation automatically."
  :keymap autodoc-mode-map
  (autodoc--enable))

(provide 'autodoc)
;;; autodoc.el ends here
