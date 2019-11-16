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
  #:use-module (guix git-download)
  #:use-module (gnu packages check)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
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
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages kodi)
  #:use-module (gnu packages xml)
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


(define-public dtv-scan-tables
  (package
    (name "dtv-scan-tables")
    (version "20191116")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/crazycat69/dtv-scan-tables.git")
                   (commit "8767d01455e9d74eb954683fcde24009e15fd609")))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1rla37s45ckwc9x4ybii5kavprfln2i4mqizbfll0zh5j2y7l60q"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-recursively (assoc-ref %build-inputs "source")
                           (string-append (assoc-ref %outputs "out") "/share"))
         #t)))

    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
    (synopsis "Hardware sensors plugin for Xfce4")
    (description
     "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
    ;; The main plugin code is covered by gpl2+, but the files containing code
    ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
    (license (list license:gpl2+ license:lgpl2.0+))))

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
                                             #t))
                               (add-after 'install 'link-dtv-scan-tables
                                          (lambda* (#:key inputs outputs #:allow-other-keys)
                                            (let ((dvb-directory (string-append %output "/share/tvheadend/data/dvb")))
                                              (display (string-append (assoc-ref %build-inputs "dtv-scan-tables") "/share"))
                                              (display dvb-directory)
                                              (symlink (string-append (assoc-ref %build-inputs "dtv-scan-tables") "/share")
                                                       (string-append dvb-directory))
                                              #t))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("intltool" ,intltool)
                     ("python" ,python)
                     ("which" ,which)
                     ("wget" ,wget)
                     ("git" ,git)))
    (inputs `(("ffmpeg" ,ffmpeg)
              ("openssl" ,openssl)
              ("dtv-scan-tables" ,dtv-scan-tables)))

    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
    (synopsis "Hardware sensors plugin for Xfce4")
    (description
     "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
    ;; The main plugin code is covered by gpl2+, but the files containing code
    ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
    (license (list license:gpl2+ license:lgpl2.0+))))


(define-public libp8-platform
  (package
   (name "libp8-platform")
   (version "2.1.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/Pulse-Eight/platform/archive/p8-platform-" version ".tar.gz"))
            (sha256
             (base32
              "18381y54f7d18ckpzf9cfxbz1ws6imprbbm9pvhcg5c86ln8skq6"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags
      (list
       (string-append "-DCMAKE_INSTALL_LIBDIR=" (assoc-ref %outputs "out") "/lib"))
      #:tests? #f))
   (synopsis "Media center for home theater computers")
   (description "Kodi is a media center application for playing videos,
music, games, etc.  Kodi is highly customizable and features a theme and
plug-in system.")
   (home-page "https://kodi.tv")
   (license license:gpl2+)))

(define-public kodi-platform
  (package
    (name "kodi-platform")
    (version "20190726")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xbmc/kodi-platform.git")
                    (commit "809c5e9d711e378561440a896fcb7dbcd009eb3d")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i3j2f856qi4b1dmh9wiwj6kklzpxc4myl7vna3ysnlfw2qqyf5j"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list
        (string-append "-DCMAKE_INSTALL_LIBDIR=" (assoc-ref %outputs "out") "/lib"))
       #:tests? #f))
    (inputs
     `(("libp8-platform" ,libp8-platform)
       ("kodi" ,kodi)
       ("tinyxml" ,tinyxml)))
    (synopsis "Media center for home theater computers")
    (description "Kodi is a media center application for playing videos,
music, games, etc.  Kodi is highly customizable and features a theme and
plug-in system.")
    (home-page "https://kodi.tv")
    (license license:gpl2+)))

(define-public kodi-pvr-hts
  (package
   (name "kodi-pvr-hts")
   (version "4.4.20")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/kodi-pvr/pvr.hts.git")
                  (commit (string-append version "-Leia"))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "09icwncgvzfxngf6ln81yd5pxdl6ql6bing5zkvsmvfp2d4ammzz"))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f
      #:configure-flags '("-DOVERRIDE_PATHS=1")))
   (inputs
    `(("libp8-platform" ,libp8-platform)
      ("kodi-platform" ,kodi-platform)
      ("kodi" ,kodi)))
   (synopsis "Media center for home theater computers")
   (description "Kodi is a media center application for playing videos,
music, games, etc.  Kodi is highly customizable and features a theme and
plug-in system.")
   (home-page "https://kodi.tv")
   ;; XBMC is largely GPL2+, with some library components as LGPL2.1+, but
   ;; there are some other licenses spread throughout.
   (license (list license:gpl2+ license:lgpl2.1+))))

(define-public arduino-lirc-plugin
  (package
   (name "arduino-lirc-plugin")
   (version "20191114")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/geostarling/arduino-lirc-plugin.git")
                  (commit "0c3f83aa54225c35db58e5830da29cbbd7a4aba9")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "18pd3srhwqpvppasj27imydc369fa5d4ssik16zvlf67qgfxmw10"))))
   (build-system gnu-build-system)
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs `(("lirc" ,lirc)))
   (arguments
    `(#:make-flags (let ((out (assoc-ref %outputs "out")))
                     (list (string-append "PLUGINDIR=" out "/lib/lirc/plugins")
                           (string-append "CONFIGDIR=" out "/share/lirc/configs")
                           (string-append "PLUGINDOCS=" out "/share/doc/lirc/plugindocs")))
      #:phases
      (modify-phases %standard-phases
                     (delete 'configure))
      #:tests? #f))
   (home-page "http://neil.brown.name/blog/mdadm")
   (synopsis "Tool for managing Linux Software RAID arrays")
   (description
    "mdadm is a tool for managing Linux Software RAID arrays.  It can create,
assemble, report on, and monitor arrays.  It can also move spares between raid
arrays when needed.")
   (license license:gpl2+)))
