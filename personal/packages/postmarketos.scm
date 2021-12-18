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

(define-module (personal packages postmarketos)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake))


(define-public pmbootstrap
  (let ((commit "fd7050835f88797d15ebdfbfa1424ebcf002de3d"))
    (package
     (name "pmbootstrap")
     (version  (git-version "1.40.0" "0" commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/postmarketOS/pmbootstrap.git")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m059k91s2m0f111p4jzk46w2sh0pc1c7nz2vgbh35q8g808fb6v"))))
     (build-system python-build-system)
     (arguments
      `(#:tests? #f))
     (native-inputs
      `(("python-pytest" ,python-pytest)))
     (propagated-inputs
      `(("openssl" ,openssl)
        ("git" ,git)))
     (home-page "https://github.com/attwad/python-osc")
     (synopsis "Open Sound Control server and client implementations")
     (description
      "@code{python-osc} is a pure Python library with no external
  dependencies.  It implements the @uref{http://opensoundcontrol.org/spec-1_0,
  Open Sound Control 1.0} specification.")
     (license license:unlicense))))
