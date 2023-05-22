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

(define-module (personal packages pipewire)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages file)
  #:use-module (gnu packages image)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages c)
  #:use-module (gnu packages lua)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (gnu packages check)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (gnu packages linux))



(define-public pipewire-0.3.71
  (package
    (inherit pipewire)
    (name "pipewire")
    (version "0.3.71")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/pipewire/pipewire")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1shqc3ibwf2xmpbkxq5pgqaldj9a8185yiq3sbxzq8wywnbidxil"))))
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dudevrulesdir=" #$output "/lib/udev/rules.d")
              "-Dsystemd=disabled"
              "-Dsession-managers=[]"
              "-Dsysconfdir=/etc"
              "-Drlimits-install=false"
              "-Dman=enabled")))
    (native-inputs
     (list `(,glib "bin")
           pkg-config
           python-docutils))))


(define-public wireplumber-0.4.14
  (package
    (inherit wireplumber)
    (name "wireplumber")
    (version "0.4.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://gitlab.freedesktop.org/pipewire/wireplumber.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jmnd6000j4wx68lxgz5b4g4hxkf243ivi9swaaf8rnx99cbx91w"))))
    (inputs (list dbus elogind glib lua pipewire))))


(define-public xdg-desktop-portal-1.16.0
  (package
    (inherit xdg-desktop-portal)
    (name "xdg-desktop-portal")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/flatpak/xdg-desktop-portal/releases/download/"
                    version "/xdg-desktop-portal-" version ".tar.xz"))
              (sha256
               (base32
                "06cczlh39kc41rvav06v37sad827y61rffy3v29i918ibj8sahav"))))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("flatpak" ,flatpak)
       ("fontconfig" ,fontconfig)
       ("json-glib" ,json-glib)
       ("libportal" ,libportal)
       ("dbus" ,dbus)
       ("geoclue" ,geoclue)
       ("pipewire" ,pipewire-0.3.71)
       ("fuse" ,fuse-3)))))

;; (define-public gbm
;;   (package
;;     (name "gbm")
;;     (version "0.56")
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                     (url "https://git.kernel.org/pub/scm/libs/ell/ell.git")
;;                     (commit version)))
;;               (file-name (git-file-name name version))
;;               (sha256
;;                (base32
;;                 "084mc9377k2a61wyqnfnsgfrdvv1rinn9wzw8l8crip0hlikn938"))))
;;     (build-system gnu-build-system)
;;     (arguments
;;      ;; Tests launch dbus-daemon instances that all try to bind to
;;      ;; "/tmp/ell-test-bus".  Thus, we need to run them sequentially.
;;      '(#:parallel-tests? #f))
;;     (inputs
;;      (list dbus))
;;     (native-inputs
;;      (list autoconf automake libtool pkg-config))
;;     (home-page "https://01.org/ell")
;;     (synopsis "Embedded Linux Library")
;;     (description "The Embedded Linux* Library (ELL) provides core, low-level
;;      functionality for system daemons.  It typically has no dependencies other than
;;      the Linux kernel, C standard library, and libdl (for dynamic linking).  While
;;      ELL is designed to be efficient and compact enough for use on embedded Linux
;;      platforms, it is not limited to resource-constrained systems.")
;;     (license license:lgpl2.1+)))




(define-public xdg-desktop-portal-wlr-0.7.0
  (package
    (inherit xdg-desktop-portal-wlr)
    (name "xdg-desktop-portal-wlr")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/emersion/xdg-desktop-portal-wlr")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b3hpp3ybjgnnmnwsyb5bsnvz9q5nr3zz0j1alh02g24f68lf00k"))
              (patches (search-patches "xdg-desktop-portal-wlr-harcoded-length.patch"))))
    (inputs (list elogind
                  bash-minimal
                  grim
                  iniparser
                  libinih
                  mesa
                  pipewire-0.3.71
                  slurp
                  wayland
                  wayland-protocols))))
