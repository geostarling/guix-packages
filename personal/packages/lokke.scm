;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
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

(define-module (personal packages lokke)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages bash))


(define-public lokke
  (let ((commit "92d36370dc6d218ff3bf315e56ebef93808c1b79"))
    (package
     (name "lokke")
     (version "0.0.1")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://git.sr.ht/~rlb/lokke")
              (commit commit)))
        (file-name (git-file-name "lokke" version))
        (sha256
         (base32
          "1c913md4dcfb0x4n26wbx9wdw453wxg3c5rn49k3f6j8zjqv63yv"))))
     (build-system gnu-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'patch-setup-shebangs
            (lambda _
              (substitute* "dev/refresh"
                (("/usr/bin/env bash") (which "bash")))
              (substitute* "dev/gen-module-paths"
                (("/usr/bin/env bash") (which "bash")))
              (substitute* "gen-makefile"
                (("/usr/bin/env guile") (which "guile")))
              #t))
          (add-before 'bootstrap 'invoke-setup-script
            (lambda _
              ;; default unpack phase deletes .git/ directory required by setup script
              ;; so we have to recreate here
              (invoke "git" "init")
              (invoke "git" "add" ".")
              (invoke "git" "config" "user.email" "you@example.com")
              (invoke "git" "config" "user.name" "Your Name")
              (invoke "git" "commit" "-m" "dummy")
              (invoke (which "sh") "./setup")))
          (replace 'bootstrap
            (lambda _
              (invoke "autoreconf" "-fi")))
          (add-before 'configure 'setenv-vars
            (lambda _
              ;; compilation requires writeable ccache directory
              ;; default is not writeable
              (setenv "XDG_CACHE_HOME" "/tmp/ccache")))
          (delete 'strip))))
     (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("pkg-config" ,pkg-config)
        ("bash" ,bash)  ; for ./setup script
        ("git" ,git)  ; for ./setup script
        ("gettext" ,gettext-minimal)
        ("libtool" ,libtool)
        ("man-db" ,man-db)))
     (inputs
      `(("guile" ,guile-3.0-latest)
        ("pcre2" ,pcre2)))
     (synopsis "NGINX module for Lua programming language support")
     (description "This NGINX module provides a scripting support with Lua
programming language.")
     (license (list license:unlicense license:expat))
     (home-page "https://github.com/BurntSushi/ripgrep"))))
