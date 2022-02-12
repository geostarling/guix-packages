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

(define-module (personal packages rustlibs)
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
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages textutils))

(define-public rust-bitflags
 (package
   (name "rust-bitflags")
   (version "1.0.4")
   (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitflags" version))
       (file-name
         (string-append name "-" version ".tar.gz"))
       (sha256
         (base32
           "04nfhscc9mxwhmai5xgwh4q458rjszmwsvkpf752g1j6dyklg012"))))
   (build-system cargo-build-system)
   (home-page
     "https://github.com/bitflags/bitflags")
   (synopsis
     "A macro to generate structures which behave like bitflags.
 ")
   (description
     "This package provides a macro to generate structures which behave like bitflags.
 ")
   (license #f)))


(define-public rust-argparse-0.2
  (package
    (name "rust-argparse")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "argparse" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0iqy2jkifwq0azrrh26qjssp7sknjylycq35jkalzb744xcbz3iz"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "http://github.com/tailhook/rust-argparse")
    (synopsis "Powerful command-line argument parsing library")
    (description "Powerful command-line argument parsing library")
    (license license:expat)))

(define-public rust-unshare-0.7
  (package
    (name "rust-unshare")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unshare" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "16vvivmj7r64ymxc0grqgv4bjxmdaiv26j57z24xl7iaanas5v8w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-nix" ,rust-nix-0.20))
        #:cargo-development-inputs
        (("rust-argparse" ,rust-argparse-0.2)
         ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/tailhook/unshare")
    (synopsis
      "    The low-level interface for linux namespaces (containers)
")
    (description
      "    The low-level interface for linux namespaces (containers)")
    (license (list license:expat license:asl2.0))))
