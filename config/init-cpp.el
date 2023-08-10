;;; init-cpp.el --- Configurations for C/C++	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configurations for C/C++
;; --------------------------------------

;;; Code:

;; Use c++-mode for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-common-hook 'lsp)
(with-eval-after-load 'cc-mode
  (defun my/cmake-project-generate-compile-commands ()
    "Generate the compile_commands.json file containing build flags in a cmake
project in order for clangd to understand the project code."
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (shell-command
       (if (memq system-type '(ms-dos windows-nt cygwin))
           "cmake . -G Ninja -Bbuild_nj -DCMAKE_EXPORT_COMPILE_COMMANDS=YES && move \".\\build_nj\\compile_commands.json\" \".\""
         "cmake . -Bbuild -DCMAKE_EXPORT_COMPILE_COMMANDS=YES"))))
  (define-key c-mode-base-map (kbd "C-c l s") 'my/cmake-project-generate-compile-commands)

  (c-set-offset 'innamespace 0))

;; Cmake
(when (executable-find "cmake")
  (require 'dash)
  (setq cmake-load-path (--> "cmake"
                             (executable-find it)
                             (file-truename it)
                             (file-name-concat it ".." ".." "share" "emacs" "site-lisp")
                             ;; cmake-mode.el either resides in cmake/share/emacs/site-lisp/
                             ;; or cmake/share/emacs/site-lisp/cmake/
                             (if (file-exists-p (file-name-concat it "cmake-mode.el"))
                                 it
                               (file-name-concat it "cmake"))
                             (file-truename it)))
  (add-to-list 'load-path cmake-load-path)
  (autoload 'cmake-mode (file-name-concat cmake-load-path "cmake-mode.el") "Major mode for editing CMake listfiles." t)
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
  (when (executable-find "cmake-language-server")
    (add-hook 'cmake-mode-hook 'lsp)))

(with-eval-after-load 'cc-mode
  (defun cpp-compile-vs-sln ()
    "Compile visual studio projects."
    (interactive)
    (let * ((vsvars (shell-quote-argument "C:\\Program Files (x86)\\Microsoft Visual Studio\\2022\\BuildTools\\VC\\Auxiliary\\Build\\vcvars64.bat"))
            (sln-dir (locate-dominating-file default-directory
                                             (lambda (dir)
                                               (directory-files dir
                                                                nil
                                                                ".*\\.sln$"
                                                                t))))
            (sln-file (shell-quote-argument
                       (car (directory-files sln-dir t ".*\\.sln$"))))
            (build-config "Release")
            (compile-command (concat "call " vsvars " && msbuild " sln-file
                                     " /p:Configuration=" build-config)))
         (call-interactively #'compile)))
  (define-key c-mode-base-map (kbd "<f1>") 'cpp-compile-vs-sln))

(provide 'init-cpp)

;;; init-cpp.el ends here
