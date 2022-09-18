;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
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
                          cc-by-sa4.0 non-copyleft expat public-domain))
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

(define-public my-gambit-c
  (let ((commit "eb96e81dbce81a1d7ee8ad6f21f7062b5b007ae1"))
   (package
     (name "my-gambit-c")
     (version "20220918")
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gambit/gambit/")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1gsdp7al7g4z4fgxcx3xv2822ycf5jm260jnvrn0kllxy978bl58"))))

     (build-system gnu-build-system)
     (arguments
      '(#:tests? #f
        #:configure-flags
        ;; According to the ./configure script, this makes the build slower and
        ;; use >= 1 GB memory, but makes Gambit much faster.
        '("--enable-single-host")))
     (home-page "http://dynamo.iro.umontreal.ca/wiki/index.php/Main_Page")
     (synopsis "Efficient Scheme interpreter and compiler")
     (description
      "Gambit consists of two main programs: gsi, the Gambit Scheme
interpreter, and gsc, the Gambit Scheme compiler.  The interpreter contains
the complete execution and debugging environment.  The compiler is the
interpreter extended with the capability of generating executable files.  The
compiler can produce standalone executables or compiled modules which can be
loaded at run time.  Interpreted code and compiled code can be freely
mixed.")
     ;; Dual license.
     (license (list lgpl2.1+ asl2.0)))))
