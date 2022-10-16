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

(defun company-fuzzy-company-capf--candidates-advice (old-fun &rest args)
  ;; Flex is slower than orderless which leverages the `all-completions' API written in C.
  (let* ((orderless-matching-styles '(orderless-flex))
         (completion-styles '(orderless)))
    (apply old-fun args)))

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
                        (cl-subseq seq 0 end)
                        )))
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
  :global t
  (if company-fuzzy-mode
      (progn
        (require 'orderless)
        (require 'flx-rs)
        (flx-rs-load-dyn)
        (advice-add #'company-capf--candidates :around #'company-fuzzy-company-capf--candidates-advice)
        (add-to-list 'company-transformers #'company-fuzzy-transformer t))
    (advice-remove #'company-capf--candidates #'company-fuzzy-company-capf--candidates-advice)
    (setq company-transformers
          (delete #'company-fuzzy-transformer company-transformers))))

(provide 'company-fuzzy)
;;; company-fuzzy.el ends here
