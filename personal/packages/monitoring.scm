;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2021, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018, 2019, 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Alex ter Weele <alex.ter.weele@gmail.com>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2021, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Raphaël Mélotte <raphael.melotte@mind.be>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Mathieu Laparie <mlaparie@disr.it>
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

(define-module (personal packages monitoring)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)               ;libnotify
  #:use-module (gnu packages golang)
  #:use-module (gnu packages image)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rrdtool)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages web))


(define-public riemann-c-client
  (package
    (name "riemann-c-client")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://git.madhouse-project.org/algernon/riemann-c-client/archive/riemann-c-client-"
                           version ".tar.gz"))
       (sha256
        (base32 "0bx7k1kmrszw4lx8mip4an9mlvv52fmisnh04wcmhph9nin2v326"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list protobuf protobuf-c))
    (inputs
     (list pkg-config protobuf protobuf-c))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://github.com/LemonBoy/bar")
    (synopsis "Featherweight status bar")
    (description
     "@code{lemonbar} (formerly known as @code{bar}) is a lightweight
bar entirely based on XCB.  Provides full UTF-8 support, basic
formatting, RandR and Xinerama support and EWMH compliance without
wasting your precious memory.")
    (license license:x11)))


(define-public collectd-with-plugins
  (package
    (inherit collectd)
    (name "collectd-with-plugins")
    (arguments
     `(#:configure-flags (list "--localstatedir=/var" "--sysconfdir=/etc" "--enable-write_riemann")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'autoreconf
                    (lambda _
                      ;; Required because of patched sources.
                      (invoke "autoreconf" "-vfi"))))))
    (inputs (modify-inputs (package-inputs collectd)
              (prepend riemann-c-client)))))
