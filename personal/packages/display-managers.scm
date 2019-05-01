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
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages xorg))

(define-public my-lightdm
  (package
    (name "lightdm")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/lightdm/"
                                  (version-major+minor version) "/"
                                  version "/+download/lightdm-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "18j33bm54i8k7ncxcs69zqi4105s62n58jrydqn3ikrb71s9nl6d"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:parallel-tests? #f ; fails when run in parallel
       #:configure-flags
       (list "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda _
             (substitute* "src/shared-data-manager.c"
               (("/bin/rm") (which "rm")))
             (substitute* '("data/users.conf"
                            "common/user-list.c")
               (("/bin/false") (which "false"))
               (("/usr/sbin/nologin") (which "nologin")))
             (substitute* "src/seat.c"
               (("/bin/sh") (which "sh")))
             #t))
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             (substitute* "tests/Makefile.in"
               (("test-sessions-gobject ") "")
               ((" test-sessions-python ") " "))
             #t))
         (add-before 'check 'pre-check
           ;; Run test-suite under a dbus session.
           (lambda* (#:key inputs #:allow-other-keys)
             (wrap-program "tests/src/test-python-greeter"
               `("PYTHONPATH"      ":" prefix (,(getenv "PYTHONPATH")))
               `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH"))))

             ;; Avoid printing locale warnings, which trip up the text
             ;; matching tests.
             (unsetenv "LC_ALL")
             #t)))))
    (inputs
     `(("audit" ,audit)
       ("linux-pam" ,linux-pam)
       ("shadow" ,shadow)                         ;for sbin/nologin
       ("libgcrypt" ,libgcrypt)
       ("libxcb" ,libxcb)))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("itstool" ,itstool)
       ("intltool" ,intltool)
       ;; For tests
       ("dbus" ,dbus)
       ("python" ,python-2)
       ("python-pygobject" ,python2-pygobject)))
    ;; Required by liblightdm-gobject-1.pc.
    (propagated-inputs
     `(("glib" ,glib)
       ("libx11" ,libx11)
       ("libxklavier" ,libxklavier)))
    (home-page "https://www.freedesktop.org/wiki/Software/LightDM/")
    (synopsis "Lightweight display manager")
    (description "The Light Display Manager (LightDM) is a cross-desktop
display manager which supports different greeters.")
    (license license:gpl3+)))


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
    (native-inputs
     `(("exo" ,exo)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("lightdm" ,my-lightdm)
       ("gtk+" ,gtk+)))
    (synopsis "GTK+ greeter for LightDM")
    (home-page "https://launchpad.net/lightdm-gtk-greeter")
    (description "This package provides a LightDM greeter implementation using
GTK+, lets you select a desktop session and log in to it.")
    (license license:gpl3+)))
