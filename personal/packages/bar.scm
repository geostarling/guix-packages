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

(define-module (personal packages bar)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system haskell)
  #:use-module (guix utils)
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
  #:use-module (gnu packages textutils))


(define-public ghc-xmobar
  (package
    (name "ghc-xmobar")
    (version "0.29.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/xmobar/xmobar-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0k5hjpr1vanj6hxf5mn3j5rfjrmiz4x29kcvi55d8qpzsan3iz0m"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-regex-compat" ,ghc-regex-compat)
        ("ghc-old-locale" ,ghc-old-locale)
        ("ghc-x11" ,ghc-x11)
        ("ghc-mtl" ,ghc-mtl)
        ("ghc-parsec" ,ghc-parsec)
        ("ghc-parsec-numbers" ,ghc-parsec-numbers)
        ("ghc-stm" ,ghc-stm)
        ("ghc-extensible-exceptions"
         ,ghc-extensible-exceptions)
        ("ghc-async" ,ghc-async)
        ("ghc-utf8-string" ,ghc-utf8-string)
        ("ghc-http-conduit" ,ghc-http-conduit)
        ("ghc-http-types" ,ghc-http-types)))
    (native-inputs
      `(("ghc-temporary" ,ghc-temporary)
        ("ghc-hspec" ,ghc-hspec)))
    (home-page "http://xmobar.org")
    (synopsis "A Minimalistic Text Based Status Bar")
    (description
      "Xmobar is a minimalistic text based status bar. . Inspired by the Ion3 status bar, it supports similar features, like dynamic color management, output templates, and extensibility through plugins.")
    (license license:bsd-3)))
