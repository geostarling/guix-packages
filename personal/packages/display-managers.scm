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

(define-module (personal packages display-managers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system dub)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages file)
  #:use-module (gnu packages c)
  #:use-module (guix utils)

  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gnome)      ;; for vte and libpeas
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xorg)

  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages textutils))

(define-public my-lightdm-gtk-greeter
  (package
    (name "lightdm-gtk-greeter")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://launchpad.net/lightdm-gtk-greeter/"
                    (version-major+minor version) "/" version
                    "/+download/lightdm-gtk-greeter-" version ".tar.gz"))
              (sha256
               (base32
                "1436sdm83xqhxyr1rzqxhsl8if2xmidlvb341xcv6dv83lyxkrlf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("exo" ,exo)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("lightdm" ,lightdm)
       ("gtk+" ,gtk+)))
    (synopsis "GTK+ greeter for LightDM")
    (home-page "https://launchpad.net/lightdm-gtk-greeter")
    (description "This package provides a LightDM greeter implementation using
GTK+, lets you select a desktop session and log in to it.")
    (license license:gpl3+)))
