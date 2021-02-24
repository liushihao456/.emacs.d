;;; init-wgrep.el --- Configurations for wgrep	-*- lexical-binding: t -*-

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
;; Configurations for wgrep.
;; --------------------------------------

;;; Code:

;; Deadgrep
(global-set-key (kbd "C-c s") 'deadgrep)

;; wgrep-deadgrep
;; C-c C-p to enable editing in the grep result buffer
;; C-c C-e to apply the changes
(add-hook 'deadgrep-finished-hook 'wgrep-deadgrep-setup)
;; (define-key deadgrep-mode-map "C-c C-s" 'wgrep-save-all-buffers)

(provide 'init-wgrep)

;;; init-wgrep.el ends here
