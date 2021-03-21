;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Adam Massmann <massmannak@gmail.com>
;;; Copyright © 2018 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (personal packages scheme)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:select (gpl2+ lgpl2.0+ lgpl2.1 lgpl2.1+ lgpl3+ asl2.0 bsd-3
                          cc-by-sa4.0 non-copyleft expat))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages libphidget)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public my-racket
  (package
    (name "my-racket")
    (version "8.0")            ; note: remember to also update racket-minimal
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://mirror.racket-lang.org/installers/"
                                        version "/racket-src.tgz")
                         ;; this mirror seems to have broken HTTPS:
                         (string-append
                          "http://mirror.informatik.uni-tuebingen.de/mirror/racket/"
                          version "/racket-src.tgz")))
              (sha256
               (base32
                "047wpjblfzmf1msz7snrp2c2h0zxyzlmbsqr9bwsyvz3frcg0888"))
              (patches
               (parameterize
                   ((%patch-path
                     (map (lambda (directory)
                            (string-append directory "/personal/packages/patches"))
                          %load-path)))
                 (search-patches "racket-sh-via-rktio.patch"
                                 ;; TODO: If we're no longer patching Racket source
                                 ;; files with store paths, we may also fix the
                                 ;; issue that necessitated the following patch:
                                 "racket-store-checksum-override.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `(,(string-append "CPPFLAGS=-DGUIX_RKTIO_PATCH_BIN_SH="
                         (assoc-ref %build-inputs "sh")
                         "/bin/sh")
         "--enable-libz"
         "--enable-liblz4")
       #:modules
       ((guix build gnu-build-system)
        (guix build utils)
        (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-chez-configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "src/cs/c/Makefile.in"
               (("/bin/sh") (which "sh")))
             ;; TODO: Racket CS uses a fork of Chez Scheme.
             ;; Most of this is copy-pasted from the "chez.scm",
             ;; but maybe there's a way to reuse more directly.
             (with-directory-excursion "src/ChezScheme"
               (substitute* (find-files "mats" "Mf-.*")
                 (("^[[:space:]]+(cc ) *") "\tgcc "))
               (substitute*
                   (find-files "." (string-append
                                    "("
                                    "Mf-[a-zA-Z0-9.]+"
                                    "|Makefile[a-zA-Z0-9.]*"
                                    "|checkin"
                                    "|stex\\.stex"
                                    "|newrelease"
                                    "|workarea"
                                    "|unix\\.ms"
                                    "|^6\\.ms"
                                    ;;"|[a-zA-Z0-9.]+\\.ms" ; guile can't read
                                    ")"))
                 (("/bin/rm") (which "rm"))
                 (("/bin/ln") (which "ln"))
                 (("/bin/cp") (which "cp"))
                 (("/bin/echo") (which "echo")))
               (substitute* "makefiles/installsh"
                 (("/bin/true") (which "true"))))
             #t))
         (add-before 'configure 'pre-configure-minimal
           (lambda* (#:key inputs #:allow-other-keys)
             (chdir "src")
             #t))
         (add-after 'build 'patch-config.rktd-lib-search-dirs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; We do this between the `build` and `install` phases
             ;; so that we have racket to read and write the hash table,
             ;; but it comes before `raco setup`, when foreign libraries
             ;; are needed to build the documentation.
             (define out (assoc-ref outputs "out"))
             (apply invoke
                    "./cs/c/racketcs"
                    "-e"
                    ,(format #f
                             "~s"
                             '(let* ((args
                                      (vector->list
                                       (current-command-line-arguments)))
                                     (file (car args))
                                     (extra-lib-search-dirs (cdr args)))
                                (write-to-file
                                 (hash-update
                                  (file->value file)
                                  'lib-search-dirs
                                  (lambda (dirs)
                                    (append dirs extra-lib-search-dirs))
                                  null)
                                 #:exists 'truncate/replace
                                 file)))
                    "--"
                    "../etc/config.rktd"
                    (filter-map (lambda (lib)
                                  (cond
                                   ((assoc-ref inputs lib)
                                    => (lambda (pth)
                                         (string-append pth "/lib")))
                                   (else
                                    #f)))
                                '("cairo"
                                  "fontconfig"
                                  "glib"
                                  "glu"
                                  "gmp"
                                  "gtk+"
                                  "libjpeg"
                                  "libpng"
                                  "libx11"
                                  "mesa"
                                  "mpfr"
                                  "openssl"
                                  "pango"
                                  "sqlite"
                                  "unixodbc"
                                  "libedit")))
             #t)))
       ;; XXX: how to run them?
       #:tests? #f))
    (inputs
     `(;; sqlite and libraries for `racket/draw' are needed to build the doc.
       ("sh" ,bash-minimal)
       ("zlib" ,zlib)
       ("zlib:static" ,zlib "static")
       ("lz4" ,lz4)
       ("lz4:static" ,lz4 "static")
       ("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("glib" ,glib)
       ("glu" ,glu)
       ("gmp" ,gmp)
       ("gtk+" ,gtk+)                   ; propagates gdk-pixbuf+svg
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("mesa" ,mesa)
       ("mpfr" ,mpfr)
       ("openssl" ,openssl)
       ("pango" ,pango)
       ("sqlite" ,sqlite)
       ("unixodbc" ,unixodbc)
       ("libedit" ,libedit)))
    (home-page "https://racket-lang.org")
    (synopsis "Implementation of Scheme and related languages")
    (description
     "Racket is a general-purpose programming language in the Scheme family,
with a large set of libraries and a compiler based on Chez Scheme.  Racket is
also a platform for language-oriented programming, from small domain-specific
languages to complete language implementations.

The main Racket distribution comes with many bundled packages, including
the DrRacket IDE, libraries for GUI and web programming, and implementations
of languages such as Typed Racket, R5RS and R6RS Scheme, and Datalog.")
    ;; https://download.racket-lang.org/license.html
    (license (list lgpl3+ asl2.0 expat))))

;; (define-public racket-minimal
;;   (package
;;     (inherit racket)
;;     (name "racket-minimal")
;;     (version (package-version racket))
;;     (source
;;      (origin
;;        (inherit (package-source racket))
;;        (uri (list (string-append "https://mirror.racket-lang.org/installers/"
;;                                  version "/racket-minimal-src.tgz")
;;                   ;; this mirror seems to have broken HTTPS:
;;                   (string-append
;;                    "http://mirror.informatik.uni-tuebingen.de/mirror/racket/"
;;                    version "/racket-minimal-src.tgz")))
;;        (sha256 "0mwyffw4gcci8wmzxa3j28h03h0gsz55aard8qrk3lri8r2xyg21")))
;;     (synopsis "Racket without bundled packages such as Dr. Racket")
;;     (inputs
;;      `(("openssl" ,openssl)
;;        ("sqlite" ,sqlite)
;;        ("sh" ,bash-minimal)
;;        ("zlib" ,zlib)
;;        ("zlib:static" ,zlib "static")
;;        ("lz4" ,lz4)
;;        ("lz4:static" ,lz4 "static")))
;;     (description
;;      "Racket is a general-purpose programming language in the Scheme family,
;; with a large set of libraries and a compiler based on Chez Scheme.  Racket is
;; also a platform for language-oriented programming, from small domain-specific
;; languages to complete language implementations.

;; The ``minimal Racket'' distribution includes just enough of Racket for you to
;; use @command{raco pkg} to install more.  Bundled packages, such as the
;; Dr. Racket IDE, are not included.")))
