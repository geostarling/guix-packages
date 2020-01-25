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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
;  #:use-module (guix build utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (ice-9 match)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages xorg))

(define-public lightdm-with-vala
  (package
    (inherit lightdm)
    (name "lightdm-with-vala")
    (version "1.30.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/CanonicalLtd/lightdm/releases/download/"
                                 version "/lightdm-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "158zb2d0v1309a8v19hh32y4yj3v6yg4yg6m0l7v59d3a2b7f651"))))
    (arguments
     (substitute-keyword-arguments (package-arguments lightdm)
      ((#:configure-flags flags ''())
       `(cons* "--enable-vala"
               ,flags))
      ((#:phases phases)
       `(modify-phases ,phases
                       (delete 'check)))))
    (inputs
     (cons `("vala" ,vala)
           (package-inputs lightdm)))))
