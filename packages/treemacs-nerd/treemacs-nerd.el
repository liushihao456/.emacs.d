;;; treemacs-nerd.el --- Treemacs theme with nerd icons  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: treemacs nerd icon
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3") (treemacs "2.9.5"))

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
;; A treemacs theme with nerd font icons that work in TUI.
;; --------------------------------------

;;; Usage:
;;
;; --------------------------------------

;;; Code:
(require 'treemacs)

(defgroup treemacs-nerd nil
  "Options for treemacs-nerd theme."
  :group 'treemacs-nerd)

(with-eval-after-load 'treemacs
  (treemacs-create-theme "treemacs-nerd"
    :config
    (progn
      ;; book
      (treemacs-create-icon :icon (propertize " " 'face 'treemacs-term-node-face) :fallback 'same-as-icon :extensions (root-open))
      (treemacs-create-icon :icon (propertize " " 'face 'treemacs-term-node-face) :fallback 'same-as-icon :extensions (root-closed))
      ;; folder
      (treemacs-create-icon :icon (propertize " " 'face 'treemacs-term-node-face) :fallback 'same-as-icon :extensions (dir-closed))
      (treemacs-create-icon :icon (propertize " " 'face 'treemacs-term-node-face) :fallback 'same-as-icon :extensions (dir-open))
      ;; tag
      (treemacs-create-icon :icon (propertize "炙" 'face 'font-lock-constant-face) :fallback 'same-as-icon :extensions (tag-leaf))
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-funtion-name-face)   :fallback 'same-as-icon :extensions (tag-open))
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-funtion-name-face)   :fallback 'same-as-icon :extensions (tag-closed))
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-warning-face)   :fallback 'same-as-icon :extensions (error))
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-warning-face)   :fallback 'same-as-icon :extensions (warning))
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-warning-face)   :fallback 'same-as-icon :extensions (info))
      ;; fallback
      (treemacs-create-icon :icon (propertize " " 'face 'font-lock-string-face)   :fallback 'same-as-icon :extensions (fallback))
      ;; code
      (treemacs-create-icon
       :icon (propertize " " 'face 'font-lock-string-face)
       :fallback 'same-as-icon
       :extensions ("accdb" "accdt" "actionscript" "adoc" "adoc" "ansible"
                    "antlr" "applescript" "asciidoc" "asm" "c" "cask" "cc"
                    "cc" "clj" "cljc" "cljs" "cmake" "coffee" "cpp" "css"
                    "cxx" "cython" "d" "dart" "diet" "diff" "dml"
                    "docker-compose.yml" "dockerfile" "dscript" "edn" "eex"
                    "el" "elm" "ex" "exs" "fennel" "fish" "fortran"
                    "fortran-modern" "fortranfreeform" "fsharp" "gdscript"
                    "go" "gradle" "graphql" "h" "hh" "hpp" "hs" "htm" "html"
                    "hy" "iced" "inc" "ino" "j2" "j2" "java" "jinja" "jinja2"
                    "jl" "js" "jsx" "kt" "kts" "ledger" "less" "lhs" "lisp"
                    "lua" "makefile" "matlab" "merlin" "mips" "ml" "mli"
                    "moonscript" "nim" "nims" "nix" "objectpascal" "ocaml"
                    "pascal" "patch" "pde" "perl" "pgsql" "php" "php4" "php5"
                    "phps" "pl" "plt" "pm" "pm6" "pony" "pp" "pp" "pro"
                    "prolog" "ps1" "purs" "py" "pyc" "r" "racket" "rb" "rd"
                    "rdx" "re" "rei" "rkt" "rktd" "rktl" "rs" "rsx" "sass"
                    "sbt" "scala" "scm" "scpt" "scrbl" "scribble" "scss" "sh"
                    "sql" "styles" "sv" "tex" "tpp" "ts" "tsx" "v"
                    "vagrantfile" "vh" "vhd" "vhdl" "vhms" "vim" "vue" "xsl"
                    "zsh" "zshrc"))
      (treemacs-create-icon
       :icon (propertize " " 'face 'font-lock-string-face)
       :fallback 'same-as-icon
       :extensions ("ai" "aiff" "avi" "bmp" "eps" "flac" "gif" "ico" "indd"
                    "jpeg" "jpg" "midi" "mkv" "mov" "mp3" "mp4" "ogg" "png"
                    "psd" "svg" "tif" "tiff" "wav" "webm" "webp"))
      (treemacs-create-icon
       :icon (propertize " " 'face 'font-lock-string-face)
       :fallback 'same-as-icon
       :extensions ("azw" "azw3" "cb7" "cba" "cbr" "cbt" "cbz" "ceb" "chm"
                    "djvu" "doc" "docx" "exe" "fb2" "inf" "kf8" "kfx" "lit"
                    "lrf" "lrx" "mobi" "opf" "or" "oxps" "pdb" "pdb" "pdb"
                    "pdg" "pkg" "prc" "ps" "rtf" "tr2" "tr3" "txt" "xeb" "xps"
                    "pot" "potx" "potm" "pps" "ppsx" "ppsm" "ppt" "pptx"
                    "pptm" "pa" "ppa" "ppam" "sldm" "sldx" ))
      (treemacs-create-icon
       :icon (propertize " " 'face 'font-lock-string-face)
       :fallback 'same-as-icon
       :extensions ("Vagrantfile" "babel.config.js" "babelignore" "babelrc"
                    "babelrc.js" "babelrc.json" "bashrc" "bazel" "bazelrc"
                    "bower.json" "bowerrc" "cabal" "cfg" "conf" "config"
                    "cson" "csv" "editorconfig" "envrc" "eslintignore"
                    "eslintrc" "feature" "gemfile" "git" "gitattributes"
                    "gitconfig" "gitignore" "gitmodules" "ideavimrc" "iml"
                    "ini" "inputrc" "json" "ledgerrc" "lock" "nginx"
                    "npm-shrinkwrap.json" "npmignore" "npmrc"
                    "package-lock.json" "package.json" "phpunit" "pkg" "plist"
                    "properties" "terminalrc" "toml" "tridactylrc"
                    "vimperatorrc" "vimrc" "vrapperrc" "xdefaults" "xml"
                    "xresources" "yaml" "yarn-integrity" "yarnclean"
                    "yarnignore" "yarnrc" "yml"))
      (treemacs-create-icon
       :icon (propertize " " 'face 'font-lock-string-face)
       :fallback 'same-as-icon
       :extensions ("md" "markdown" "rst" "org" "log" "txt" "contribute"
                    "license" "readme" "changelog"))
      (treemacs-create-icon
       :icon (propertize " " 'face 'font-lock-string-face)
       :fallback 'same-as-icon
       :extensions ("exe" "dll" "obj" "so" "o" "out" "elc" "cmake-cache" "csr"
                    "eslintcache" "crt" "cer" "der" "pfx" "p12" "p7b" "p7r"
                    "DS_STORE" "key" "pem" "src" "crl" "sst" "stl" "ipynb"))
       (treemacs-create-icon
        :icon (propertize " " 'face 'font-lock-string-face)
        :fallback 'same-as-icon
        :extensions ("pdf"))
       (treemacs-create-icon
        :icon (propertize " " 'face 'font-lock-string-face)
        :fallback 'same-as-icon
        :extensions ("zip" "xz" "7z" "tar" "gz" "rar" "tgz" "jar"))))
  (treemacs-load-theme "treemacs-nerd"))

;;;###autoload
(defun treemacs-nerd-config ()
  "Install treemacs-nerd theme configuration.")

(provide 'treemacs-nerd)

;;; company-tip.el ends here
