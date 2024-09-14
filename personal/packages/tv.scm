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
  #:use-module (gnu packages readline)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages file)
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


(define-public t2scan
  (package
    (name "t2scan")
    (version "0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mighty-p/t2scan/archive/refs/tags/v" version ".tar.gz"))
              (sha256
               (base32
                "1jrz8g67pa5xigdxh7yqfb3d3hj8vav90gdycjwqxvdyl8wbgr24"))))
    (build-system gnu-build-system)
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
    (synopsis "Hardware sensors plugin for Xfce4")
    (description
     "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
    ;; The main plugin code is covered by gpl2+, but the files containing code
    ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
    (license (list license:gpl2+ license:lgpl2.0+))))

(define-public dtv-scan-tables
  (package
    (name "dtv-scan-tables")
    (version "20210623")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/crazycat69/dtv-scan-tables.git")
                    (commit "a89c4055b0394ae50de3f425294f7b7d8929483a")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kwhwlmy29bbq7mkkndmr0l4nv3m0h34in7xzby7n1h1zxvfvax9"))))
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


(define-public libdvbcsa
  (package
    (name "libdvbcsa")
    (version "20210623")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/glenvt18/libdvbcsa.git")
                    (commit "2a1e61e569a621c55c2426f235f42c2398b7f18f")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09pcx2pygxazy2nyyl9ll2c8kgybvddarfkzgcyff4nabs6g7pwd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (synopsis "Library for handling PNG files")
    (description
     "Libpng is the official PNG (Portable Network Graphics) reference
     library.  It supports almost all PNG features and is extensible.")
    (license license:zlib)
    (home-page "http://www.libpng.org/pub/png/libpng.html")))


(define-public tvheadend
  (let ((commit "9ac57a0c1a4551012260008cfca6bfc2386f6dcf")
        (revision "0"))
    (package
      (name "tvheadend")
      (version (git-version "4.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tvheadend/tvheadend.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1j2f2wbhg7lm92rz6j88mb3299d7vn96sr4ba4nshdnljpjpq01b"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f    ; there is no test target
         #:configure-flags `("--enable-libav"
                             "--enable-debug=trace"
                             "--disable-dvbscan"
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
                       ("libdvbcsa" ,libdvbcsa)
                       ("git" ,git)))
      (inputs `(("ffmpeg" ,ffmpeg)
                ("openssl" ,openssl)
                ("libx264" ,libx264)
                ("x265" ,x265)
                ("opus" ,opus)
                ("libvpx" ,libvpx)
                ("tar" ,tar) ;; required for migrations
                ("dtv-scan-tables" ,dtv-scan-tables)))

      (home-page
       "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
      (synopsis "Hardware sensors plugin for Xfce4")
      (description
       "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
      ;; The main plugin code is covered by gpl2+, but the files containing code
      ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
      (license (list license:gpl2+ license:lgpl2.0+)))))

(define-public miraclecast
  (let ((commit "fdb8671c4087826541c4ffc14df5716c28acd62a")
        (revision "0"))
    (package
      (name "miraclecast")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/albfan/miraclecast")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1gswg06knxhg2044vdrhj8x0lkd7n8qgrrp80h7f3n0ny16c9z77"))
         (patches
          (search-patches "miraclecast-systemd.patch"))))
      (build-system meson-build-system)
      (arguments
       (list #:configure-flags #~(list "-Denable-systemd=false"
                                       "-Dbuild-tests=true"
                                       (string-append "-Dip-binary=" #$(this-package-input "iproute2") "/sbin/ip"))
         #:phases #~(modify-phases %standard-phases
                      (add-after 'unpack 'miraclecast-gst-launch-patch
                           (lambda* (#:key inputs #:allow-other-keys)
                             (substitute* "res/miracle-gst"
                                          (("/usr/bin/gst-launch-1.0")
                                           (string-append #$(this-package-input "gstreamer") "/bin/gst-launch-1.0")))))

                      (add-after 'install 'wrap-miracle-sinkctl
                        (lambda* (#:key inputs #:allow-other-keys)
                          (let ((gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
                            (wrap-program (string-append #$output "/bin/miracle-sinkctl")
                              `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path)))))))))

      (native-inputs `(("pkg-config" ,pkg-config)
                       ("cmake" ,cmake)))
      (inputs (list readline
                    elogind
                    glib
                    iproute
                    gstreamer
                    gst-libav
                    gst-plugins-base
                    gst-plugins-good
                    gst-plugins-bad
                    gst-plugins-ugly
                    eudev))
      (propagated-inputs (list wpa-supplicant
                               dbus))
      (synopsis "Miraclecast")
      (description "MrG is is a C API for creating user interfaces.  It can be
       used as an application writing environment or as an interactive canvas for part
       of a larger interface.")
      (home-page "https://github.com/hodefoting/mrg")
      (license license:lgpl2.0+))))


(define-public my-lirc
  (package
    (inherit lirc)
    (name "my-lirc")
    (inputs
     (modify-inputs (package-inputs lirc)
       (delete "libusb-compat")
       (append libusb)))))
