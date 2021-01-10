;;;
;;; Copyright © 2019 Jiri Spacek <spaceji3@fit.cvut.cz>
;;;;;;
;;; The Work is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; The Work is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with The Work.  If not, see <http://www.gnu.org/licenses/>.

(define-module (personal packages rust-apps)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages file)
  #:use-module (gnu packages c)

  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gnome)      ;; for vte and libpeas
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages llvm)

  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (personal packages crates-io)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages textutils)
  #:use-module (guix git-download))


(define-public parinfer-rust
  (let ((commit "998484ba3ca9c162971e3fa6f01e67cc3e9ac85e")
        (revision "1"))
    (package
      (name "parinfer-rust")
      (version "0.4.3")
      (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/eraserhd/parinfer-rust.git")
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0hj5in5h7pj72m4ag80ing513fh65q8xlsf341qzm3vmxm3y3jgd"))))
      (build-system cargo-build-system)
      (arguments
       `(#:cargo-inputs
         (("rust-getopts" ,rust-getopts-0.2)
          ("rust-serde" ,rust-serde-1)
          ("rust-serde-json" ,rust-serde-json-1)
          ("rust-serde-derive" ,rust-serde-derive-1)
          ("rust-emacs" ,rust-emacs) ;; TODO pin input version
          ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
          ("rust-unicode-width" ,rust-unicode-width-0.1))
         #:phases
         ;; clang is required by rust-emacs-module
         (modify-phases %standard-phases
                        (add-after 'unpack 'set-environmental-variable
                                   (lambda* (#:key inputs #:allow-other-keys)
                                     (let ((clang (assoc-ref inputs "clang")))
                                       (setenv "LIBCLANG_PATH"
                                               (string-append clang "/lib")))
                                     #t))
                        (add-after 'install 'install-so
                                   (lambda* (#:key inputs #:allow-other-keys)
                                     (let ((lib-target (string-append (assoc-ref %outputs "out")
                                                                      "/lib/")))
                                       (mkdir-p lib-target)
                                       (copy-file
                                        "target/release/libparinfer_rust.so"
                                        (string-append lib-target "parinfer-rust-linux.so"))
                                       #t))))))
      (native-inputs
       `(("clang" ,clang)))
      (home-page "https://github.com/BurntSushi/ripgrep")
      (synopsis "Line-oriented search tool")
      (description
       "ripgrep is a line-oriented search tool that recursively searches
your current directory for a regex pattern while respecting your
gitignore rules.")
      (license (list license:unlicense license:expat)))))
