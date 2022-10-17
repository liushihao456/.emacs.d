;;; company-fuzzy.el --- Fuzzy matching for company -*- lexical-binding: t -*-

;; Author: Shihao Liu
;; Keywords: company fuzzy
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3") (company "0.8.9"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package adds fuzzy matching to company.
;; --------------------------------------

;;; Usage
;;
;; After the package is installed, you can enable `company-fuzzy` by adding the
;; following to your init file:

;;     (with-eval-after-load 'company
;;       (company-fuzzy-mode))

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'flx-rs)

(defgroup company-fuzzy nil
  "Sort company candidates by flx score."
  :group 'convenience
  :prefix "company-fuzzy-")

(defcustom company-fuzzy-limit 500
  "The maximum number of company candidates to flx sort."
  :type 'number
  :group 'company-fuzzy)

(defun company-fuzzy-commonality (strs)
  "Return the largest string that fuzzy matches all STRS"
  (cl-letf* ((commonality-cache (make-hash-table :test 'equal :size 200))
             ((symbol-function
               #'fuzzy-commonality)
              (lambda (strs)
                (let ((hash-value (gethash strs commonality-cache nil)))
                  (if hash-value
                      (if (eq hash-value 'nothing)
                          nil
                        hash-value)

                    (setq strs (mapcar #'string-to-list strs))
                    (let ((res) (tried) (idx))
                      (dolist (char (car strs))
                        (unless (memq char tried)
                          (catch 'notfound
                            (setq idx (mapcar (lambda (str)
                                                (or
                                                 (cl-position char str)
                                                 (throw 'notfound nil)))
                                              strs))
                            (push (cons char
                                        (fuzzy-commonality
                                         (cl-mapcar (lambda (str idx)
                                                      (cl-subseq str (1+ idx)))
                                                    strs idx)))
                                  res)
                            (push char tried))))
                      (setq res (if res
                                    (cl-reduce
                                     (lambda (a b)
                                       (if (> (length a) (length b)) a b))
                                     res)
                                  nil))
                      (puthash strs
                               (if res res 'nothing)
                               commonality-cache)
                      res))))))
    (concat (fuzzy-commonality strs))))

(defun company-fuzzy-find-holes (merged str)
  "Find positions in MERGED, where insertion by the user is likely, wrt. STR"
  (let ((holes) (matches (cdr (flx-rs-score str merged))))
    (dolist (i (number-sequence 0 (- (length matches) 2)))
      (when (>
             (elt matches (1+ i))
             (1+ (elt matches i)))
        (push (1+ i) holes)))
    (unless (<= (length str) (car (last matches)))
      (push (length merged) holes))
    holes))

(defun company-fuzzy-merge (strs)
  "Merge a collection of strings, including their collective holes"
  (let ((common (company-fuzzy-commonality strs))
        (holes))
    (setq holes (make-vector (1+ (length common)) 0))
    (dolist (str strs)
      (dolist (hole (company-fuzzy-find-holes common str))
        (cl-incf (elt holes hole))))

    (cons common (append holes nil))))

(defun company-fuzzy-completion (string table predicate point
                                      &optional all-p)
  "Helper function implementing a fuzzy completion-style"
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (boundaries (completion-boundaries beforepoint table predicate afterpoint))
         (prefix (substring beforepoint 0 (car boundaries)))
         (infix (concat
                 (substring beforepoint (car boundaries))
                 (substring afterpoint 0 (cdr boundaries))))
         (suffix (substring afterpoint (cdr boundaries)))
         ;; |-              string                  -|
         ;;              point^
         ;;            |-  boundaries -|
         ;; |- prefix -|-    infix    -|-  suffix   -|
         ;;
         ;; Infix is the part supposed to be completed by table, AFAIKT.
         ;; The first letter must be at the beginning of candidate
         (regexp (concat "\\`" (substring infix 0 1)
                         (mapconcat
                          (lambda (x)
                            (setq x (string x))
                            (concat "[^" x "]*" (regexp-quote x)))
                          (substring infix 1)
                          "")))
         (completion-regexp-list (cons regexp completion-regexp-list))
         (candidates (or (all-completions prefix table predicate)
                         (all-completions infix table predicate))))

    (if all-p
        ;; Implement completion-all-completions interface
        (when candidates
          ;; Not doing this may result in an error.
          (setcdr (last candidates) (length prefix))
          candidates)
      ;; Implement completion-try-completions interface
      (if (= (length candidates) 1)
          (if (equal infix (car candidates))
              t
            ;; Avoid quirk of double / for filename completion. I don't
            ;; know how this is *supposed* to be handled.
            (when (and (> (length (car candidates)) 0)
                       (> (length suffix) 0)
                       (char-equal (aref (car candidates)
                                         (1- (length (car candidates))))
                                   (aref suffix 0)))
              (setq suffix (substring suffix 1)))
            (cons (concat prefix (car candidates) suffix)
                  (length (concat prefix (car candidates)))))
        (if (= (length infix) 0)
            (cons string point)
          (cl-destructuring-bind (merged . holes)
              (company-fuzzy-merge candidates)
            (cons
             (concat prefix merged suffix)
             (+ (length prefix)
                (cl-position (apply #'max holes) holes)))))))))

(defun company-fuzzy-try-completion (string table predicate point)
  "Fuzzy version of completion-try-completion"
  (company-fuzzy-completion string table predicate point))
(defun company-fuzzy-all-completions (string table predicate point)
  "Fuzzy version of completion-all-completions"
  (company-fuzzy-completion string table predicate point 'all))

;; Suppress warning of free variable
(defvar company-fuzzy-mode)

(defun company-fuzzy-company-capf--candidates-advice (old-fun &rest args)
  ;; Filtering (this function) is the performance bottleneck.
  ;; Scoring and sorting (`company-fuzzy-transformer') is fast.
  ;; Flex is slower than orderless which leverages the `all-completions' API
  ;; written in C.
  (if company-fuzzy-mode
      (let* ((completion-styles '(fuzzy)))
        (apply old-fun args)))
  (apply old-fun args))

(defun company-fuzzy-transformer (cands)
  "Sort up to company-fuzzy-limit CANDS by their flx score."
  (let* ((num-cands (length cands))
         (sub-cands (if (< num-cands company-fuzzy-limit)
                        cands
                      (let* ((seq (sort cands (lambda (c1 c2)
                                                (< (length c1)
                                                   (length c2)))))
                             (end (min company-fuzzy-limit
                                       num-cands
                                       (length seq))))
                        (cl-subseq seq 0 end))))
         (scored-cands (mapcar
                        (lambda (cand)
                          (cons cand
                                (or (car (flx-rs-score cand company-prefix))
                                    most-negative-fixnum)))
                        sub-cands))
         (sorted-cands (sort scored-cands
                             (lambda (c1 c2)
                               ;; break ties by length
                               (if (/= (cdr c1) (cdr c2))
                                   (> (cdr c1)
                                      (cdr c2))
                                 (< (length (car c1))
                                    (length (car c2))))))))
    (mapcar #'car sorted-cands)))

;;;###autoload
(define-minor-mode company-fuzzy-mode
  "Company-fuzzy minor mode."
  :init-value nil
  :group 'company-fuzzy
  (if company-fuzzy-mode
      (progn
        (add-to-list 'completion-styles-alist
                     '(fuzzy
                       company-fuzzy-try-completion
                       company-fuzzy-all-completions
                       "An intelligent fuzzy matching completion style with the first letter at the beginning."))

        (flx-rs-load-dyn)
        (advice-add #'company-capf--candidates :around #'company-fuzzy-company-capf--candidates-advice)
        (make-local-variable 'company-transformers)
        (add-to-list 'company-transformers #'company-fuzzy-transformer t))
    (advice-remove #'company-capf--candidates #'company-fuzzy-company-capf--candidates-advice)
    (setq-local company-transformers
                (delete #'company-fuzzy-transformer company-transformers))))

(provide 'company-fuzzy)
;;; company-fuzzy.el ends here
