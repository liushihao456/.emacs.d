;; init-custom-packages.el --- Local custom packages.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Local custom packages.
;; --------------------------------------

;;; Code:

(dolist (name
         (with-no-warnings
           (append (directory-files-recursively
                    (file-name-concat user-emacs-directory "packages") "" t))))
  (if (and (file-regular-p name) (string-suffix-p "-autoloads.el" name))
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
         (generated-autoload-file (expand-file-name auto-name pkg-dir)))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file))
    (message "Generating autoloads to file: %s" generated-autoload-file)
    (package-generate-autoloads pkg-name pkg-dir)))

(defun generate-autoloads-custom-packages ()
  "Generate autoloads for all custom packages."
  (interactive)
  (dolist
      (name
       (directory-files (file-name-concat user-emacs-directory "packages") t))
    (when (and (file-directory-p name)
               (not (string-suffix-p "." name))
               (not (string-suffix-p ".." name)))
      (message "Generating autoloads for package %s..."
               (file-name-nondirectory (directory-file-name name)))
      (byte-recompile-directory name 0)
      (generate-recursive-autoloads
       (file-name-nondirectory (directory-file-name name)) name))))

(global-set-key (kbd "C-c f g") 'generate-autoloads-custom-packages)

(provide 'init-custom-packages)

;;; init-custom-packages.el ends here
