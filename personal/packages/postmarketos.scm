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
  #:use-module (gnu packages check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake))


(define-public pmbootstrap
  (package
   (name "pmbootstrap")
   (version "1.18.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://gitlab.com/postmarketOS/pmbootstrap/-/archive/"
                                version
                                "/pmbootstrap-" version ".tar.gz"))
            (sha256
             (base32
              "18j0dkrhaqphbzlc0gh4vgpnal4rmlnf2whlds6nydkmk4vr4q57"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f))
   (native-inputs
    `(("python-pytest" ,python-pytest)))
   (home-page "https://github.com/attwad/python-osc")
   (synopsis "Open Sound Control server and client implementations")
   (description
    "@code{python-osc} is a pure Python library with no external
dependencies.  It implements the @uref{http://opensoundcontrol.org/spec-1_0,
Open Sound Control 1.0} specification.")
   (license license:unlicense)))
