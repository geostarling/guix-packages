;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Andrew Miloradovsky <andrew@interpretmath.pw>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Dion Mendel <guix@dm9.info>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (personal packages cluster)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public cowsql-libraft
  (package
    (name "cowsql-libraft")
    (version "0.22.1")
    (home-page "https://github.com/cowsql/raft")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url "https://github.com/cowsql/raft.git")
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cs70jlc2hg3hrcwpc4h54l4zpwm287mlamdm7gxhpdw7c0kyv38"))))
    (arguments '(#:configure-flags '("--enable-uv")
                 #:phases
                 (modify-phases %standard-phases
                   (add-after 'unpack 'disable-failing-tests
                     (lambda _
                       (substitute* "Makefile.am"
                         ((".*test_uv_append.c.*") "")
                         ((".*test_uv_tcp_connect.c.*") ""))
                       #t)))))
    (inputs
     (list libuv lz4))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (build-system gnu-build-system)
    (synopsis "C implementation of the Raft consensus protocol")
    (description "The library has modular design: its core part implements only
the core Raft algorithm logic, in a fully platform independent way.  On top of
that, a pluggable interface defines the I/O implementation for networking
(send/receive RPC messages) and disk persistence (store log entries and
snapshots).")
    (license license:asl2.0)))

(define-public libcowsql
  (package
    (name "libcowsql")
    (version "1.15.6")
    (home-page "https://github.com/cowsql/cowsql")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url "https://github.com/cowsql/cowsql.git")
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v32i7a5zxqqmfy5sjwziy5hqaf0igqjcxxf33pa1zznz57q1gkj"))))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "Makefile.am"
               ;; Test client/query sometimes fails.
               ;; The actual tested asserts succeed, but there appears to be a
               ;; race condition when tearing down the test server.
               ((".*test_client.c.*") "")))))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list cowsql-libraft libuv))
    (propagated-inputs
     (list sqlite))  ; dqlite.h includes sqlite3.h
    (build-system gnu-build-system)
    (synopsis "Distributed SQLite")
    (description "dqlite is a C library that implements an embeddable and replicated
SQL database engine with high-availability and automatic failover.")
    (license license:lgpl3)))
