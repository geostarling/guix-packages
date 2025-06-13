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

(define-module (personal packages kodi-xyz)
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
  #:use-module (gnu packages web)
  #:use-module (gnu packages c)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (gnu packages check)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages kodi)
  #:use-module (personal packages kodi)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages video)
  #:use-module (gnu packages python)
  #:use-module (gnu packages wget))


(define bento4
  (package
    (name "bento4")
    (version "1.6.0-641-3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xbmc/Bento4.git")
                    (commit (string-append version "-Omega"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xmjw36dbkm771y27zh3qncrfbpb48pxsfvqb8vk9m1bg2yr1if9"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags `("-DCMAKE_CXX_FLAGS=-O3 -fPIC"
                           "-DCMAKE_C_FLAGS=-O3 -fPIC")))

    ;;                        ;; override cmake-build-system libdir
    ;;                        "-DCMAKE_INSTALL_LIBDIR=lib/kodi"
    ;;                        ;; ugly hack: kodi from kodi pkg for some reason looks for addons.xml in lib/kodi instead
    ;;                        ;; of share/kodi, so we place all of the data resources alongside the shared lib
    ;;                        "-DCMAKE_INSTALL_DATADIR=lib/kodi")))
    (synopsis "Media center for home theater computers")
    (description "Kodi is a media center application for playing videos,
    music, games, etc.  Kodi is highly customizable and features a theme and
    plug-in system.")
    (home-page "https://kodi.tv")
    ;; XBMC is largely GPL2+, with some library components as LGPL2.1+, but
    ;; there are some other licenses spread throughout.
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kodi-pvr-hts
  (package
    (name "kodi-pvr-hts")
    (version "21.2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kodi-pvr/pvr.hts.git")
                    (commit (string-append version "-Omega"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1acb9nhvvvn051l450kpgqcghpyw43p7cwi7a7ihpqmv7qc6cvh4"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags `("-DOVERRIDE_PATHS=1"
                           "-DENABLE_INTERNAL_BENTO4=OFF"
                           ;; override cmake-build-system libdir
                           "-DCMAKE_INSTALL_LIBDIR=lib/kodi"
                           ;; ugly hack: kodi from kodi pkg for some reason looks for addons.xml in lib/kodi instead
                           ;; of share/kodi, so we place all of the data resources alongside the shared lib
                           "-DCMAKE_INSTALL_DATADIR=lib/kodi")))
    (inputs
     `(("kodi" ,kodi)))
    (synopsis "Media center for home theater computers")
    (description "Kodi is a media center application for playing videos,
    music, games, etc.  Kodi is highly customizable and features a theme and
    plug-in system.")
    (home-page "https://kodi.tv")
    ;; XBMC is largely GPL2+, with some library components as LGPL2.1+, but
    ;; there are some other licenses spread throughout.
    (license (list license:gpl2+ license:lgpl2.1+))))


(define-public kodi-inputstream-adaptive
  (package
    (name "kodi-inputstream-adaptive")
    (version "21.5.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xbmc/inputstream.adaptive.git")
                    (commit (string-append version "-Omega"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1444nvd29jkd43954ba7gbqbkrb79fk8dyl2iyxkd42la2f0ssc0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags `("-DOVERRIDE_PATHS=1"
                           ;; override cmake-build-system libdir
                           "-DCMAKE_INSTALL_LIBDIR=lib/kodi"
                           "-DBUILD_TESTING=OFF"
                           ;; ugly hack: kodi from kodi pkg for some reason looks for addons.xml in lib/kodi instead
                           ;; of share/kodi, so we place all of the data resources alongside the shared lib
                           "-DCMAKE_INSTALL_DATADIR=lib/kodi")))
    (inputs
     `(("kodi" ,kodi)
       ("pugixml" ,pugixml)
       ("rapidjson" ,rapidjson)
       ("bento4" ,bento4)))
    (synopsis "Media center for home theater computers")
    (description "Kodi is a media center application for playing videos,
    music, games, etc.  Kodi is highly customizable and features a theme and
    plug-in system.")
    (home-page "https://kodi.tv")
    ;; XBMC is largely GPL2+, with some library components as LGPL2.1+, but
    ;; there are some other licenses spread throughout.
    (license (list license:gpl2+ license:lgpl2.1+))))


;;
