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

(define-module (personal packages tv)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages file)
  #:use-module (gnu packages c)
  #:use-module (guix utils)

  #:use-module (gnu packages check)

  #:use-module (gnu packages version-control)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gettext)
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
  #:use-module (gnu packages tls)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages video)
  #:use-module (gnu packages python)
  #:use-module (gnu packages wget))


(define-public tvheadend
  (package
    (name "tvheadend")
    (version "4.2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/tvheadend/tvheadend/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1bgp05gl3px2d2w8g2yknrj4mqymsib924rgpnkx5ynmff9qivqs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f    ; there is no test target
       #:configure-flags `("--enable-libav"
                           "--disable-ffmpeg_static"
                           "--disable-libx264_static"
                           "--disable-libx265_static"
                           "--disable-libvpx_static"
                           "--disable-libtheora_static"
                           "--disable-libvorbis_static"
                           "--disable-libfdkaac_static"
                           "--disable-hdhomerun_static"
                           "--disable-libmfx_static"
                           "--disable-hdhomerun_client"
                           "--disable-dvbscan"
                           "--disable-pcloud_cache"
                           "--python=python3")
       #:phases (modify-phases %standard-phases
                               (add-before 'configure 'set-build-environment
                                           (lambda _
                                             (setenv "CC" "gcc")
                                             #t)))))

    (native-inputs `(("pkg-config" ,pkg-config)
                     ("intltool" ,intltool)
                     ("python" ,python)
                     ("which" ,which)
                     ("wget" ,wget)
                     ("git" ,git)))
    (inputs `(("ffmpeg" ,ffmpeg)
              ("openssl" ,openssl)))

    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
    (synopsis "Hardware sensors plugin for Xfce4")
    (description
     "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
    ;; The main plugin code is covered by gpl2+, but the files containing code
    ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
    (license (list license:gpl2+ license:lgpl2.0+))))
