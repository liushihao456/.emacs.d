;; init-custom-packages.el --- Local custom packages.	-*- lexical-binding: t -*-

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
;; Local custom packages.
;; --------------------------------------

;;; Code:

(dolist (name (with-no-warnings
                (append (directory-files-recursively "~/.config/emacs/packages/" "" t))))
  (when (and (file-regular-p name) (string-suffix-p "-autoloads.el" name))
    (load name)))

(defun generate-recursive-autoloads (pkg-name pkg-dir)
  "Update a package's recursive autoloads.

The first ARG PKG-NAME is the name of the package.  The second ARG PKG-DIR is
the directory path of the package.

This is needed because the package.el infrastructure doesn't
process autoloads in subdirectories; instead we create an
additional autoloads file of our own, and we load it from an
autoloaded form."
  (interactive "sPackage name: \nDPackage directory: ")
  (require 'autoload)
  (let* ((auto-name (format "%s-autoloads.el" pkg-name))
         (generated-autoload-file (expand-file-name auto-name pkg-dir))
         (autoload-timestamps nil)
         (backup-inhibited t)
         (version-control 'never))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file))
    (message "Generating autoloads to file: %s" generated-autoload-file)
    (write-region (autoload-rubric generated-autoload-file "package" nil) nil generated-autoload-file nil 'silent)
    (dolist (name (with-no-warnings
                    (append (list pkg-dir) (directory-files-recursively pkg-dir "" t))))
      (when (file-directory-p name)
        (message "Generating autoloads for directory: %s..." name)
        (update-directory-autoloads name)))
    (let ((buf (find-buffer-visiting generated-autoload-file)))
      (when buf (kill-buffer buf)))))

(defun generate-autoloads-custom-packages ()
  "Generate autoloads for custom packages."
  (interactive)
  (dolist (name (directory-files "~/.config/emacs/packages/" t))
    (when (and (file-directory-p name) (not (string-suffix-p "." name)) (not (string-suffix-p ".." name)))
      (message "Generating autoloads for package %s..." (file-name-nondirectory (directory-file-name name)))
      (generate-recursive-autoloads (file-name-nondirectory (directory-file-name name)) name))))

(provide 'init-custom-packages)

;;; init-custom-packages.el ends here
