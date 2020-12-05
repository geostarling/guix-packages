 ;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2017, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (personal packages kodi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages kodi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (personal packages tv)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)

  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages assembly))

;; (define-public intel-graphics-compiler
;;   (package
;;     (name "intel-graphics-compiler")
;;     (version "1.0.3041")
;;     (source
;;      (origin
;;       (method url-fetch)
;;       (uri (string-append "https://github.com/intel/media-driver/archive/igc-" version ".tar.gz"))
;;       (sha256
;;        (base32 "1hly7m42vcfp54n0n60a32gmsmgg7wxp5cmawd2h5jz9xxgdffx0"))))
;;     (build-system cmake-build-system)
;;     ;;(native-inputs)
;;     ;; `(("pkg-config" ,pkg-config)))
;;     (inputs
;;      `(;;("libdrm" ,libdrm)
;;        ("clang" ,clang)))
;;        ;;("libx11" ,libx11)))
;;     (arguments
;;      `(#:configure-flags
;;        (list "-DENABLE_KERNELS=ON"
;;              "-DENABLE_NONFREE_KERNELS=OFF"
;;              "-DBUILD_KERNELS=ON")))
;; ;;       #:tests? #f))                    ; no test suite
;;     ;; (arguments
;;     ;;  `(#:phases
;;     ;;    ;;      ENABLE_KERNELS=ON ENABLE_NONFREE_KERNELS=OFF BUILD_KERNELS=ON
;;     ;;    (modify-phases %standard-phases
;;     ;;      (add-before 'configure 'set-target-directory
;;     ;;        (lambda* (#:key outputs #:allow-other-keys)
;;     ;;          (let ((out (assoc-ref outputs "out")))
;;     ;;            (setenv "LIBVA_DRIVERS_PATH" (string-append out "/lib/dri"))
;;     ;;            #t))))))
;;     ;; XXX Because of <https://issues.guix.gnu.org/issue/22138>, we need to add
;;     ;; this to all VA-API back ends instead of once to libva.
;;     ;; (native-search-paths
;;     ;;  (list (search-path-specification
;;     ;;         (variable "LIBVA_DRIVERS_PATH")
;;     ;;         (files '("lib/dri")))))
;;     ;;(supported-systems '("i686-linux" "x86_64-linux"))
;;     (home-page "https://01.org/linuxmedia/vaapi")
;;     (synopsis "VA-API video acceleration driver for Intel GEN Graphics devices")
;;     (description
;;      "This is the @acronym{VA-API, Video Acceleration API} back end required for
;; hardware-accelerated video processing on Intel GEN Graphics devices supported by
;; the i915 driver, such as integrated Intel HD Graphics.  It provides access to
;; both hardware and shader functionality for faster encoding, decoding, and
;; post-processing of video formats like MPEG2, H.264/AVC, and VC-1.")
;;     (license (list license:bsd-2        ; src/gen9_vp9_const_def.c
;;                    license:expat))))    ; the rest, excluding the test suite


;; (define-public libva-intel-media-driver
;;   (package
;;     (name "libva-intel-media-driver")
;;     (version "19.4.0")
;;     (source
;;      (origin
;;       (method url-fetch)

;;       (uri (string-append "https://github.com/intel/media-driver/archive/intel-media-" version "r.tar.gz"))
;;       (sha256
;;        (base32 "1hly7m42vcfp54n0n60a32gmsmgg7wxp5cmawd2h5jz9xxgdffx0"))))
;;     (build-system cmake-build-system)
;;     ;;(native-inputs)
;;     ;; `(("pkg-config" ,pkg-config)))
;;     (inputs
;;      `(;;("libdrm" ,libdrm)
;;        ("libva" ,libva)))
;;        ;;("libx11" ,libx11)))
;;     (arguments
;;      `(#:configure-flags
;;        (list "-DENABLE_KERNELS=ON"
;;              "-DENABLE_NONFREE_KERNELS=OFF"
;;              "-DBUILD_KERNELS=ON")))
;; ;;       #:tests? #f))                    ; no test suite
;;     ;; (arguments
;;     ;;  `(#:phases
;;     ;;    ;;      ENABLE_KERNELS=ON ENABLE_NONFREE_KERNELS=OFF BUILD_KERNELS=ON
;;     ;;    (modify-phases %standard-phases
;;     ;;      (add-before 'configure 'set-target-directory
;;     ;;        (lambda* (#:key outputs #:allow-other-keys)
;;     ;;          (let ((out (assoc-ref outputs "out")))
;;     ;;            (setenv "LIBVA_DRIVERS_PATH" (string-append out "/lib/dri"))
;;     ;;            #t))))))
;;     ;; XXX Because of <https://issues.guix.gnu.org/issue/22138>, we need to add
;;     ;; this to all VA-API back ends instead of once to libva.
;;     ;; (native-search-paths
;;     ;;  (list (search-path-specification
;;     ;;         (variable "LIBVA_DRIVERS_PATH")
;;     ;;         (files '("lib/dri")))))
;;     ;;(supported-systems '("i686-linux" "x86_64-linux"))
;;     (home-page "https://01.org/linuxmedia/vaapi")
;;     (synopsis "VA-API video acceleration driver for Intel GEN Graphics devices")
;;     (description
;;      "This is the @acronym{VA-API, Video Acceleration API} back end required for
;; hardware-accelerated video processing on Intel GEN Graphics devices supported by
;; the i915 driver, such as integrated Intel HD Graphics.  It provides access to
;; both hardware and shader functionality for faster encoding, decoding, and
;; post-processing of video formats like MPEG2, H.264/AVC, and VC-1.")
;;     (license (list license:bsd-2        ; src/gen9_vp9_const_def.c
;;                    license:expat))))    ; the rest, excluding the test suite


;; (define-public ffmpeg-with-vaapi
;;   (package
;;     (name "ffmpeg-with-vaapi")
;;     (version "4.2.1")
;;     (source (origin
;;              (method url-fetch)
;;              (uri (string-append "https://ffmpeg.org/releases/ffmpeg-"
;;                                  version ".tar.xz"))
;;              (sha256
;;               (base32
;;                "1m5nkc61ihgcf0b2wabm0zyqa8sj3c0w8fi6kr879lb0kdzciiyf"))))
;;     (build-system gnu-build-system)
;;     (inputs
;;      `(("dav1d" ,dav1d)
;;        ("fontconfig" ,fontconfig)
;;        ("freetype" ,freetype)
;;        ("frei0r-plugins" ,frei0r-plugins)
;;        ("gnutls" ,gnutls)
;;        ("opus" ,opus)
;;        ("ladspa" ,ladspa)
;;        ("lame" ,lame)
;;        ("libaom" ,libaom)
;;        ("libass" ,libass)
;;        ("libbluray" ,libbluray)
;;        ("libcaca" ,libcaca)
;;        ("libcdio-paranoia" ,libcdio-paranoia)
;;        ("libdrm" ,libdrm)
;;        ("libtheora" ,libtheora)
;;        ("libva" ,libva)
;;        ("intel-vaapi-driver" ,intel-vaapi-driver)
;;        ("libvdpau" ,libvdpau)
;;        ("libvorbis" ,libvorbis)
;;        ("libvpx" ,libvpx)
;;        ("libx11" ,libx11)
;;        ("libx264" ,libx264)
;;        ("mesa" ,mesa)
;;        ("openal" ,openal)
;;        ("pulseaudio" ,pulseaudio)
;;        ("sdl" ,sdl2)
;;        ("soxr" ,soxr)
;;        ("speex" ,speex)
;;        ("twolame" ,twolame)
;;        ("vidstab" ,vidstab)
;;        ("x265" ,x265)
;;        ("xvid" ,xvid)
;;        ("zlib" ,zlib)))
;;     (native-inputs
;;      `(("bc" ,bc)
;;        ("perl" ,perl)
;;        ("pkg-config" ,pkg-config)
;;        ("texinfo" ,texinfo)
;;        ("python" ,python-2) ; scripts use interpreter python2
;;        ("speex" ,speex)
;;        ("yasm" ,yasm)))
;;     (arguments
;;      `(#:test-target "fate"
;;        #:configure-flags
;;        ;; possible additional inputs:
;;        ;;   --enable-avisynth        enable reading of AviSynth script
;;        ;;                            files [no]
;;        ;;   --enable-libaacplus      enable AAC+ encoding via libaacplus [no]
;;        ;;   --enable-libcelt         enable CELT decoding via libcelt [no]
;;        ;;   --enable-libdc1394       enable IIDC-1394 grabbing using libdc1394
;;        ;;                            and libraw1394 [no]
;;        ;;   --enable-libfaac         enable AAC encoding via libfaac [no]
;;        ;;   --enable-libfdk-aac      enable AAC de/encoding via libfdk-aac [no]
;;        ;;   --enable-libflite        enable flite (voice synthesis) support via
;;        ;;                            libflite [no]
;;        ;;   --enable-libgme          enable Game Music Emu via libgme [no]
;;        ;;   --enable-libgsm          enable GSM de/encoding via libgsm [no]
;;        ;;   --enable-libiec61883     enable iec61883 via libiec61883 [no]
;;        ;;   --enable-libilbc         enable iLBC de/encoding via libilbc [no]
;;        ;;   --enable-libmodplug      enable ModPlug via libmodplug [no]
;;        ;;   --enable-libnut          enable NUT (de)muxing via libnut,
;;        ;;                            native (de)muxer exists [no]
;;        ;;   --enable-libopencore-amrnb    enable AMR-NB de/encoding via
;;        ;;                                 libopencore-amrnb [no]
;;        ;;   --enable-libopencore-amrwb    enable AMR-WB decoding via
;;        ;;                                 libopencore-amrwb [no]
;;        ;;   --enable-libopencv       enable video filtering via libopencv [no]
;;        ;;   --enable-libopenjpeg     enable JPEG 2000 de/encoding via
;;        ;;                            OpenJPEG [no]
;;        ;;   --enable-librtmp         enable RTMP[E] support via librtmp [no]
;;        ;;   --enable-libschroedinger enable Dirac de/encoding via
;;        ;;                            libschroedinger [no]
;;        ;;   --enable-libshine        enable fixed-point MP3 encoding via
;;        ;;                            libshine [no]
;;        ;;   --enable-libssh          enable SFTP protocol via libssh [no]
;;        ;;                            (libssh2 does not work)
;;        ;;   --enable-libstagefright-h264  enable H.264 decoding via
;;        ;;                                 libstagefright [no]
;;        ;;   --enable-libutvideo      enable Ut Video encoding and decoding via
;;        ;;                            libutvideo [no]
;;        ;;   --enable-libv4l2         enable libv4l2/v4l-utils [no]
;;        ;;   --enable-libvo-aacenc    enable AAC encoding via libvo-aacenc [no]
;;        ;;   --enable-libvo-amrwbenc  enable AMR-WB encoding via
;;        ;;                            libvo-amrwbenc [no]
;;        ;;   --enable-libwavpack      enable wavpack encoding via libwavpack [no]
;;        ;;   --enable-libxavs         enable AVS encoding via xavs [no]
;;        ;;   --enable-libzmq          enable message passing via libzmq [no]
;;        ;;   --enable-libzvbi         enable teletext support via libzvbi [no]
;;        ;;   --enable-opencl          enable OpenCL code
;;        '("--enable-avresample"
;;          "--enable-gpl" ; enable optional gpl licensed parts
;;          "--enable-shared"
;;          "--enable-frei0r"
;;          "--enable-fontconfig"
;;          "--enable-gnutls"
;;          "--enable-ladspa"
;;          "--enable-libaom"
;;          "--enable-libass"
;;          "--enable-libbluray"
;;          "--enable-libcaca"
;;          "--enable-libcdio"
;;          "--enable-libdav1d"
;;          "--enable-libfreetype"
;;          "--enable-libmp3lame"
;;          "--enable-libopus"
;;          "--enable-libpulse"
;;          "--enable-libsoxr"
;;          "--enable-libspeex"
;;          "--enable-libtheora"
;;          "--enable-libtwolame"
;;          "--enable-libvidstab"
;;          "--enable-libvorbis"
;;          "--enable-libvpx"
;;          "--enable-libxvid"
;;          "--enable-libx264"
;;          "--enable-libx265"
;;          "--enable-openal"
;;          "--enable-opengl"
;;          "--enable-libdrm"

;;          "--enable-runtime-cpudetect"

;;          ;; The HTML pages take 7.2 MiB
;;          "--disable-htmlpages"

;;          ;; The static libraries are 23 MiB
;;          "--disable-static"

;;          ;; Runtime cpu detection is not implemented on
;;          ;; MIPS, so we disable some features.
;;          "--disable-mips32r2"
;;          "--disable-mipsdsp"
;;          "--disable-mipsdspr2"
;;          "--disable-mipsfpu"
;;          "--enable-vaapi")
;;        #:phases
;;        (modify-phases %standard-phases
;;          (replace
;;           'configure
;;           ;; configure does not work followed by "SHELL=..." and
;;           ;; "CONFIG_SHELL=..."; set environment variables instead
;;           (lambda* (#:key outputs configure-flags #:allow-other-keys)
;;             (let ((out (assoc-ref outputs "out")))
;;               (substitute* "configure"
;;                 (("#! /bin/sh") (string-append "#!" (which "sh"))))
;;               (setenv "SHELL" (which "bash"))
;;               (setenv "CONFIG_SHELL" (which "bash"))
;;               (apply invoke
;;                      "./configure"
;;                      (string-append "--prefix=" out)
;;                      ;; Add $libdir to the RUNPATH of all the binaries.
;;                      (string-append "--extra-ldflags=-Wl,-rpath="
;;                                     out "/lib")
;;                      configure-flags))))
;;          (add-before
;;           'check 'set-ld-library-path
;;           (lambda _
;;             ;; Allow $(top_builddir)/ffmpeg to find its dependencies when
;;             ;; running tests.
;;             (let* ((dso  (find-files "." "\\.so$"))
;;                    (path (string-join (map dirname dso) ":")))
;;               (format #t "setting LD_LIBRARY_PATH to ~s~%" path)
;;               (setenv "LD_LIBRARY_PATH" path)
;;               #t))))))
;;     (home-page "https://www.ffmpeg.org/")
;;     (synopsis "Audio and video framework")
;;     (description "FFmpeg is a complete, cross-platform solution to record,
;; convert and stream audio and video.  It includes the libavcodec
;; audio/video codec library.")
;;     (license license:gpl2+)))



;; (define-public kodi-with-vaapi
;;   (package
;;     (name "kodi-with-vaapi")
;;     (version "18.6")
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                     (url "https://github.com/xbmc/xbmc.git")
;;                     (commit (string-append version "-Leia"))))
;;               (file-name (git-file-name name version))
;;               (sha256
;;                (base32
;;                 "0rwymipn5hljy5xrslzmrljmj6f9wb191wi7gjw20wl6sv44d0bk"))
;;               (patches (search-patches "kodi-skip-test-449.patch"
;;                                        "kodi-increase-test-timeout.patch"
;;                                        "kodi-set-libcurl-ssl-parameters.patch"))
;;               (snippet
;;                '(begin
;;                   (use-modules (guix build utils))
;;                   (for-each delete-file-recursively
;;                             '("project/BuildDependencies/"
;;                               ;; TODO: Purge these jars.
;;                               ;;"tools/codegenerator/groovy"
;;                               ;; And these sources:
;;                               ;; "tools/depend/native/JsonSchemaBuilder"
;;                               ;; "tools/depend/native/TexturePacker"
;;                               ;; "lib/gtest"
;;                               ;; "lib/cpluff"
;;                               ;; "lib/libUPnP"
;;                               "lib/libUPnP/Neptune/ThirdParty"
;;                               "project/Win32BuildSetup/tools/7z"))
;;                   #t))
;;               (modules '((guix build utils)))))
;;     (build-system cmake-build-system)
;;     (arguments
;;      '(#:modules ((srfi srfi-1)
;;                   (guix build cmake-build-system)
;;                   (guix build utils))
;;        #:configure-flags
;;        (list "-DENABLE_INTERNAL_FFMPEG=OFF"
;;              "-DENABLE_INTERNAL_CROSSGUID=OFF"
;;              (string-append "-Dlibdvdread_URL="
;;                             (assoc-ref %build-inputs "libdvdread-bootstrapped"))
;;              (string-append "-Dlibdvdnav_URL="
;;                             (assoc-ref %build-inputs "libdvdnav-bootstrapped"))
;;              (string-append "-Dlibdvdcss_URL="
;;                             (assoc-ref %build-inputs "libdvdcss-bootstrapped"))
;;              (string-append "-DSYSTEM_LDFLAGS=-Wl,-rpath="
;;                             (assoc-ref %build-inputs "curl") "/lib"))
;;        #:phases
;;        (modify-phases %standard-phases
;;          ;; The build system tries to bootstrap these bundled components
;;          ;; during the regular build phase, which causes serious issues
;;          ;; because there's no time for shebangs to be patched.  So, we
;;          ;; bootstrap it on our own instead.
;;          (add-after 'unpack 'bootstrap-bundled-software
;;            (lambda _
;;              (let ((dirs '("tools/depends/native/JsonSchemaBuilder/src"
;;                            "lib/cpluff")))
;;                (every (lambda (third-party)
;;                         (with-directory-excursion third-party
;;                           (invoke "autoreconf" "-vif")))
;;                       dirs))))
;;          (add-after 'bootstrap-bundled-software 'patch-stuff
;;            (lambda* (#:key inputs #:allow-other-keys)
;;              ;; Prevent the build scripts from calling autoreconf in the
;;              ;; build stage.  Otherwise, it would undo the bootstrapping
;;              ;; and shebang patching that we worked so hard for.
;;              (substitute* "cmake/modules/FindCpluff.cmake"
;;                (("autoreconf -vif") "true"))
;;              (substitute* "lib/cpluff/po/Makefile.in.in"
;;                (("/bin/sh") (which "sh")))
;;              (substitute* "cmake/modules/FindLibDvd.cmake"
;;                ;; The libdvd* sources that we bootstrapped separately are
;;                ;; unpacked in the build phase. This is our best opportunity
;;                ;; to make them writable before the build process starts.
;;                (("autoreconf -vif") "chmod -R u+w ."))

;;              (substitute* "xbmc/platform/linux/LinuxTimezone.cpp"
;;                (("/usr/share/zoneinfo")
;;                 (string-append (assoc-ref inputs "tzdata")
;;                                "/share/zoneinfo")))

;;              ;; Don't phone home to check for updates.
;;              (substitute* "system/addon-manifest.xml"
;;                (("<addon optional=\\\"true\\\">service.xbmc.versioncheck</addon>")
;;                 ""))

;;              ;; Let's disable some tests that are known not to work here.
;;              ;; Doing this later while in the cmake "../build" directory
;;              ;; is trickier.
;;              (substitute* '("xbmc/utils/test/TestSystemInfo.cpp")
;;                (("TEST_F\\(TestSystemInfo, GetOsPrettyNameWithVersion\\)")
;;                 "TEST_F(TestSystemInfo, DISABLED_GetOsPrettyNameWithVersion)")
;;                (("TEST_F\\(TestSystemInfo, GetOsName\\)")
;;                 "TEST_F(TestSystemInfo, DISABLED_GetOsName)")
;;                (("TEST_F\\(TestSystemInfo, GetOsVersion\\)")
;;                 "TEST_F(TestSystemInfo, DISABLED_GetOsVersion)"))
;;              #t))
;;          (add-before 'build 'set-build-environment
;;            (lambda _
;;              ;; Some bundled build scripts fall back to /bin/sh
;;              ;; if this is not set.
;;              (setenv "CONFIG_SHELL" (which "sh"))
;;              #t))
;;          (add-before 'check 'build-kodi-test
;;            (lambda _
;;              (invoke "make" "kodi-test")))
;;         (add-after 'install 'symlink-addons
;;              (lambda* (#:key inputs outputs #:allow-other-keys)
;;                (let ((lib-addons (string-append %output "/lib/kodi/addons"))
;;                      (share-addons (string-append %output "/share/kodi/addons/"))
;;                      (pvr-hts-addon (assoc-ref inputs "kodi-pvr-hts")))
;;                  (mkdir-p lib-addons)
;;                  (mkdir-p share-addons)

;;                  (symlink (string-append pvr-hts-addon "/lib/addons/pvr.hts")
;;                           (string-append lib-addons "/pvr.hts"))

;;                  (symlink (string-append pvr-hts-addon "/share/kodi/addons/pvr.hts")
;;                           (string-append share-addons "/pvr.hts")))
;;                #t)))
;;     ;; TODO: Add dependencies for:
;;     ;; - nfs
;;     ;; - cec
;;     ;; - plist
;;     ;; - shairplay
;;        #:tests? #f))
;;     (native-inputs
;;      `(("autoconf" ,autoconf)
;;        ("automake" ,automake)
;;        ("gettext" ,gettext-minimal)
;;        ("icedtea" ,icedtea) ; needed at build-time only, mandatory
;;        ("libdvdcss-bootstrapped" ,libdvdcss/kodi)
;;        ("libdvdnav-bootstrapped" ,libdvdnav/kodi)
;;        ("libdvdread-bootstrapped" ,libdvdread/kodi)
;;        ("libtool" ,libtool)
;;        ("pkg-config" ,pkg-config)
;;        ("swig" ,swig)
;;        ("yasm" ,yasm)))
;;     (inputs
;;      `(("alsa-lib" ,alsa-lib)
;;        ("avahi" ,avahi)
;;        ("bluez" ,bluez)
;;        ("crossguid" ,crossguid)
;;        ("curl" ,curl)
;;        ("dcadec" ,dcadec)
;;        ("dbus" ,dbus)
;;        ("eudev" ,eudev)
;;        ("ffmpeg" ,ffmpeg-with-vaapi)
;;        ("intel-vaapi-driver" ,intel-vaapi-driver)
;;        ("flac" ,flac)
;;        ("flatbuffers" ,flatbuffers)
;;        ("fmt" ,fmt)
;;        ("fontconfig" ,fontconfig)
;;        ("freetype" ,freetype)
;;        ("fribidi" ,fribidi)
;;        ("fstrcmp" ,fstrcmp)
;;        ("giflib" ,giflib)
;;        ("glew" ,glew)
;;        ("gnutls" ,gnutls)
;;        ("kodi-pvr-hts" ,kodi-pvr-hts)
;;        ("lame" ,lame)
;;        ("lcms" ,lcms)
;;        ("libass" ,libass)
;;        ("libbluray" ,libbluray)
;;        ("libcap" ,libcap)
;;        ("libcdio" ,libcdio)
;;        ("libdrm" ,libdrm)
;;        ("libgcrypt" ,libgcrypt)
;;        ("libjpeg" ,libjpeg)
;;        ("libltdl" ,libltdl)
;;        ("libmad" ,libmad)
;;        ("libmicrohttpd" ,libmicrohttpd)
;;        ("libmpeg2" ,libmpeg2)
;;        ("libogg" ,libogg)
;;        ("libpng" ,libpng)
;;        ("libssh" ,libssh)
;;        ("libtiff" ,libtiff)
;;        ("libva" ,libva)
;;        ("libvorbis" ,libvorbis)
;;        ("libxml2" ,libxml2)
;;        ("libxrandr" ,libxrandr)
;;        ("libxrender" ,libxrender)
;;        ("libxslt" ,libxslt)
;;        ("lirc" ,lirc)
;;        ("lzo" ,lzo)
;;        ("mariadb" ,mariadb "lib")
;;        ("mariadb-dev" ,mariadb "dev")
;;        ("openssl" ,openssl)
;;        ("pcre" ,pcre)
;;        ("pulseaudio" ,pulseaudio)
;;        ("python" ,python-2)
;;        ("rapidjson" ,rapidjson)
;;        ("samba" ,samba)
;;        ("sqlite" ,sqlite)
;;        ("taglib" ,taglib)
;;        ("tinyxml" ,tinyxml)
;;        ("tzdata" ,tzdata)
;;        ("util-linux" ,util-linux)
;;        ("zip" ,zip)
;;        ("zlib" ,zlib)))
;;     (synopsis "Media center for home theater computers")
;;     (description "Kodi is a media center application for playing videos,
;; music, games, etc.  Kodi is highly customizable and features a theme and
;; plug-in system.")
;;     (home-page "https://kodi.tv")
;;     ;; XBMC is largely GPL2+, with some library components as LGPL2.1+, but
;;     ;; there are some other licenses spread throughout.
;;     (license (list license:gpl2+ license:lgpl2.1+
;;                    license:gpl3+                  ;WiiRemote client
;;                    license:expat                  ;cpluff, dbwrappers
;;                    license:public-domain          ;cpluff/examples
;;                    license:bsd-3                  ;misc, gtest
;;                    license:bsd-2))))              ;xbmc/freebsd





;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; non-public imports:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Kodi requires using their own special forks of these libraries.
;; ;; In addition, it insists on downloading and building these as part
;; ;; of the standard build process. To make things easier, we bootstrap
;; ;; and patch shebangs here, so we don't have to worry about it later.
;; (define libdvdnav/kodi
;;   (let ((commit "6.0.0-Leia-Alpha-3"))
;;     (package
;;       (name "libdvdnav-bootstrapped")
;;       (version commit)
;;       (source (origin
;;                 (method git-fetch)
;;                 (uri (git-reference
;;                       (url "https://github.com/xbmc/libdvdnav.git")
;;                       (commit commit)))
;;                 (file-name (string-append name "-" version "-checkout"))
;;                 (sha256
;;                  (base32
;;                   "0qwlf4lgahxqxk1r2pzl866mi03pbp7l1fc0rk522sc0ak2s9jhb"))))
;;       (build-system gnu-build-system)
;;       (arguments
;;        '(#:tests? #f
;;          #:phases
;;          (modify-phases %standard-phases
;;            (delete 'configure)
;;            (delete 'build)
;;            (replace 'install
;;              (lambda* (#:key outputs #:allow-other-keys)
;;                (copy-recursively "." (assoc-ref outputs "out"))
;;                #t)))))
;;       (native-inputs
;;        `(("autoconf" ,autoconf)
;;          ("automake" ,automake)
;;          ("libtool" ,libtool)
;;          ("pkg-config" ,pkg-config)))
;;       (home-page "https://github.com/xbmc/libdvdnav")
;;       (synopsis (package-synopsis libdvdnav))
;;       (description (package-description libdvdnav))
;;       (license license:gpl2+))))

;; (define libdvdread/kodi
;;   (let ((commit "6.0.0-Leia-Alpha-3"))
;;     (package
;;       (name "libdvdread-bootstrapped")
;;       (version commit)
;;       (source (origin
;;                 (method git-fetch)
;;                 (uri (git-reference
;;                       (url "https://github.com/xbmc/libdvdread.git")
;;                       (commit commit)))
;;                 (file-name (string-append name "-" version "-checkout"))
;;                 (sha256
;;                  (base32
;;                   "1xxn01mhkdnp10cqdr357wx77vyzfb5glqpqyg8m0skyi75aii59"))))
;;       (build-system gnu-build-system)
;;       (arguments
;;        '(#:tests? #f
;;          #:phases
;;          (modify-phases %standard-phases
;;            (delete 'configure)
;;            (delete 'build)
;;            (replace 'install
;;              (lambda* (#:key outputs #:allow-other-keys)
;;                (copy-recursively "." (assoc-ref outputs "out"))
;;                #t)))))
;;       (native-inputs
;;        `(("autoconf" ,autoconf)
;;          ("automake" ,automake)
;;          ("libtool" ,libtool)
;;          ("pkg-config" ,pkg-config)))
;;       (home-page "https://github.com/xbmc/libdvdread")
;;       (synopsis (package-synopsis libdvdread))
;;       (description (package-description libdvdread))
;;       (license (list license:gpl2+ license:lgpl2.1+)))))

;; (define libdvdcss/kodi
;;   (let ((commit "1.4.2-Leia-Beta-5"))
;;     (package
;;       (name "libdvdcss-bootstrapped")
;;       (version commit)
;;       (source (origin
;;                 (method git-fetch)
;;                 (uri (git-reference
;;                       (url "https://github.com/xbmc/libdvdcss.git")
;;                       (commit commit)))
;;                 (file-name (string-append name "-" version "-checkout"))
;;                 (sha256
;;                  (base32
;;                   "0j41ydzx0imaix069s3z07xqw9q95k7llh06fc27dcn6f7b8ydyl"))))
;;       (build-system gnu-build-system)
;;       (arguments
;;        '(#:tests? #f
;;          #:phases
;;          (modify-phases %standard-phases
;;            (delete 'configure)
;;            (delete 'build)
;;            (replace 'install
;;              (lambda* (#:key outputs #:allow-other-keys)
;;                (copy-recursively "." (assoc-ref outputs "out"))
;;                #t)))))
;;       (native-inputs
;;        `(("autoconf" ,autoconf)
;;          ("automake" ,automake)
;;          ("libtool" ,libtool)
;;          ("pkg-config" ,pkg-config)))
;;       (home-page "https://github.com/xbmc/libdvdcss")
;;       (synopsis (package-synopsis libdvdcss))
;;       (description (package-description libdvdcss))
;;       (license license:gpl2+))))


;; Kodi requires using their own special forks of these libraries.
;; In addition, it insists on downloading and building these as part
;; of the standard build process. To make things easier, we bootstrap
;; and patch shebangs here, so we don't have to worry about it later.
(define libdvdnav/kodi
  (let ((commit "6.0.0-Leia-Alpha-3"))
    (package
      (name "libdvdnav-bootstrapped")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdnav")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0qwlf4lgahxqxk1r2pzl866mi03pbp7l1fc0rk522sc0ak2s9jhb"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/xbmc/libdvdnav")
      (synopsis (package-synopsis libdvdnav))
      (description (package-description libdvdnav))
      (license license:gpl2+))))

(define libdvdread/kodi
  (let ((commit "6.0.0-Leia-Alpha-3"))
    (package
      (name "libdvdread-bootstrapped")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdread")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1xxn01mhkdnp10cqdr357wx77vyzfb5glqpqyg8m0skyi75aii59"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/xbmc/libdvdread")
      (synopsis (package-synopsis libdvdread))
      (description (package-description libdvdread))
      (license (list license:gpl2+ license:lgpl2.1+)))))

(define libdvdcss/kodi
  (let ((commit "1.4.2-Leia-Beta-5"))
    (package
      (name "libdvdcss-bootstrapped")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdcss")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0j41ydzx0imaix069s3z07xqw9q95k7llh06fc27dcn6f7b8ydyl"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/xbmc/libdvdcss")
      (synopsis (package-synopsis libdvdcss))
      (description (package-description libdvdcss))
      (license license:gpl2+))))


(define-public my-gnutls/dane
  ;; GnuTLS with build libgnutls-dane, implementing DNS-based
  ;; Authentication of Named Entities.  This is required for GNS functionality
  ;; by GNUnet and gnURL.  This is done in an extra package definition
  ;; to have the choice between GnuTLS with Dane and without Dane.
  (package/inherit gnutls
    (name "my-gnutls-dane")
    (inputs `(("unbound" ,unbound)
              ,@(package-inputs gnutls)))))


(define-public my-libmicrohttpd
  (package
    (name "my-libmicrohttpd")
    (version "0.9.71")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/libmicrohttpd/libmicrohttpd-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10mii4mifmfs3v7kgciqml7f0fj7ljp0sngrx64pnwmgbzl4bx78"))))
    (build-system gnu-build-system)
    (inputs
     `(("curl" ,curl)
       ("gnutls" ,my-gnutls/dane)
       ("libgcrypt" ,libgcrypt)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (synopsis "C library implementing an HTTP 1.1 server")
    (description
     "GNU libmicrohttpd is a small, embeddable HTTP server implemented as a
C library.  It makes it easy to run an HTTP server as part of another
application.  The library is fully HTTP 1.1 compliant.  It can listen on
multiple ports, supports four different threading models, and supports
IPv6.  It also features security features such as basic and digest
authentication and support for SSL3 and TLS.")
    (license license:lgpl2.1+)
    (home-page "https://www.gnu.org/software/libmicrohttpd/")))



(define-public my-kodi
  (package
    (name "my-kodi")
    (version "18.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xbmc/xbmc")
                    (commit (string-append version "-Leia"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qpkpz43s207msvv3qkiy6vzqwcgmydxv3py7vc29mv6h30chrva"))
              (patches (search-patches "kodi-skip-test-449.patch"
                                       "kodi-increase-test-timeout.patch"
                                       "kodi-set-libcurl-ssl-parameters.patch"))
              (snippet
               '(begin
                  (use-modules (guix build utils))
                  (for-each delete-file-recursively
                            '("project/BuildDependencies/"
                              ;; TODO: Purge these jars.
                              ;;"tools/codegenerator/groovy"
                              ;; And these sources:
                              ;; "tools/depend/native/JsonSchemaBuilder"
                              ;; "tools/depend/native/TexturePacker"
                              ;; "lib/gtest"
                              ;; "lib/cpluff"
                              ;; "lib/libUPnP"
                              "lib/libUPnP/Neptune/ThirdParty"
                              "project/Win32BuildSetup/tools/7z"))
                  #t))
              (modules '((guix build utils)))))
    (build-system cmake-build-system)
    (arguments
     '(#:modules ((srfi srfi-1)
                  (guix build cmake-build-system)
                  (guix build utils))
       #:configure-flags
       (list "-DENABLE_INTERNAL_FFMPEG=OFF"
             "-DENABLE_INTERNAL_CROSSGUID=OFF"
             (string-append "-Dlibdvdread_URL="
                            (assoc-ref %build-inputs "libdvdread-bootstrapped"))
             (string-append "-Dlibdvdnav_URL="
                            (assoc-ref %build-inputs "libdvdnav-bootstrapped"))
             (string-append "-Dlibdvdcss_URL="
                            (assoc-ref %build-inputs "libdvdcss-bootstrapped"))
             (string-append "-DSYSTEM_LDFLAGS=-Wl,-rpath="
                            (assoc-ref %build-inputs "curl") "/lib"))
       #:phases
       (modify-phases %standard-phases
         ;; The build system tries to bootstrap these bundled components
         ;; during the regular build phase, which causes serious issues
         ;; because there's no time for shebangs to be patched.  So, we
         ;; bootstrap it on our own instead.
         (add-after 'unpack 'bootstrap-bundled-software
           (lambda _
             (let ((dirs '("tools/depends/native/JsonSchemaBuilder/src"
                           "lib/cpluff")))
               (every (lambda (third-party)
                        (with-directory-excursion third-party
                          (invoke "autoreconf" "-vif")))
                      dirs))))
         (add-after 'bootstrap-bundled-software 'patch-stuff
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Prevent the build scripts from calling autoreconf in the
             ;; build stage.  Otherwise, it would undo the bootstrapping
             ;; and shebang patching that we worked so hard for.
             (substitute* "cmake/modules/FindCpluff.cmake"
               (("autoreconf -vif") "true"))
             (substitute* "lib/cpluff/po/Makefile.in.in"
               (("/bin/sh") (which "sh")))
             (substitute* "cmake/modules/FindLibDvd.cmake"
               ;; The libdvd* sources that we bootstrapped separately are
               ;; unpacked in the build phase. This is our best opportunity
               ;; to make them writable before the build process starts.
               (("autoreconf -vif") "chmod -R u+w ."))

             (substitute* "xbmc/platform/linux/LinuxTimezone.cpp"
               (("/usr/share/zoneinfo")
                (string-append (assoc-ref inputs "tzdata")
                               "/share/zoneinfo")))

             ;; Don't phone home to check for updates.
             (substitute* "system/addon-manifest.xml"
               (("<addon optional=\\\"true\\\">service.xbmc.versioncheck</addon>")
                ""))

             ;; Let's disable some tests that are known not to work here.
             ;; Doing this later while in the cmake "../build" directory
             ;; is trickier.
             (substitute* '("xbmc/utils/test/TestSystemInfo.cpp")
               (("TEST_F\\(TestSystemInfo, GetOsPrettyNameWithVersion\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsPrettyNameWithVersion)")
               (("TEST_F\\(TestSystemInfo, GetOsName\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsName)")
               (("TEST_F\\(TestSystemInfo, GetOsVersion\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsVersion)"))
             #t))
         (add-before 'build 'set-build-environment
           (lambda _
             ;; Some bundled build scripts fall back to /bin/sh
             ;; if this is not set.
             (setenv "CONFIG_SHELL" (which "sh"))
             #t))
         (add-before 'check 'build-kodi-test
           (lambda _
             (invoke "make" "kodi-test"))))))
    ;; TODO: Add dependencies for:
    ;; - cec
    ;; - plist
    ;; - shairplay
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("icedtea" ,icedtea)     ; needed at build-time only, mandatory
       ("libdvdcss-bootstrapped" ,libdvdcss/kodi)
       ("libdvdnav-bootstrapped" ,libdvdnav/kodi)
       ("libdvdread-bootstrapped" ,libdvdread/kodi)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("swig" ,swig)
       ("yasm" ,yasm)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("avahi" ,avahi)
       ("bluez" ,bluez)
       ("crossguid" ,crossguid)
       ("curl" ,curl)
       ("dcadec" ,dcadec)
       ("dbus" ,dbus)
       ("eudev" ,eudev)
       ("ffmpeg" ,ffmpeg)
       ("flac" ,flac)
       ("flatbuffers" ,flatbuffers)
       ("fmt" ,fmt)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("fstrcmp" ,fstrcmp)
       ("giflib" ,giflib)
       ("glew" ,glew)
       ("gnutls" ,gnutls)
       ("lame" ,lame)
       ("lcms" ,lcms)
       ("libass" ,libass)
       ("libbluray" ,libbluray)
       ("libcap" ,libcap)
       ("libcdio" ,libcdio)
       ("libdrm" ,libdrm)
       ("libgcrypt" ,libgcrypt)
       ("libjpeg" ,libjpeg-turbo)
       ("libltdl" ,libltdl)
       ("libmad" ,libmad)
       ("libmicrohttpd" ,libmicrohttpd)
       ("libmpeg2" ,libmpeg2)
       ("libnfs" ,libnfs)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libssh" ,libssh)
       ("libtiff" ,libtiff)
       ("libva" ,libva)
       ("libvorbis" ,libvorbis)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxslt" ,libxslt)
       ("lzo" ,lzo)
       ("mariadb-dev" ,mariadb "lib")
       ("mariadb-dev" ,mariadb "dev")
       ("openssl" ,openssl)
       ("pcre" ,pcre)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python-2)
       ("rapidjson" ,rapidjson)
       ("samba" ,samba)
       ("sqlite" ,sqlite)
       ("taglib" ,taglib)
       ("tinyxml" ,tinyxml)
       ("tzdata" ,tzdata)
       ("util-linux" ,util-linux)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (synopsis "Media center for home theater computers")
    (description "Kodi is a media center application for playing videos,
music, games, etc.  Kodi is highly customizable and features a theme and
plug-in system.")
    (home-page "https://kodi.tv")
    ;; XBMC is largely GPL2+, with some library components as LGPL2.1+, but
    ;; there are some other licenses spread throughout.
    (license (list license:gpl2+ license:lgpl2.1+
                   license:gpl3+         ;WiiRemote client
                   license:expat         ;cpluff, dbwrappers
                   license:public-domain ;cpluff/examples
                   license:bsd-3         ;misc, gtest
                   license:bsd-2))))              ;xbmc/freebsd


(define-public my-kodi/wayland
  (package/inherit my-kodi
    (name "my-kodi-wayland")
    (arguments
     (substitute-keyword-arguments (package-arguments my-kodi)
       ((#:configure-flags flags)
        `(append '("-DCORE_PLATFORM_NAME=wayland"
                   "-DWAYLAND_RENDER_SYSTEM=gl")
                 ,flags))))
    (inputs
     `(("libinput" ,libinput)
       ("libxkbcommon" ,libxkbcommon)
       ("waylandpp" ,waylandpp)
       ("waylandp-protocols" ,wayland-protocols)
       ,@(package-inputs my-kodi)))
    (synopsis "Kodi with Wayland rendering backend")))

(define-public my-kodi-with-addons/wayland
  (package
   (inherit my-kodi/wayland)
   (name "my-kodi-with-addons-wayland")

   (source #f)
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils) (guix build union))
      #:builder
      (begin
        (use-modules (guix build utils) (guix build union) (srfi srfi-26))
        (union-build (assoc-ref %outputs "out")
                     (map (lambda (input) (cdr input)) %build-inputs)
                     #:symlink (lambda [input output]
                                 (if (file-is-directory? input)
                                     (copy-recursively input output)
                                     (copy-file input output))))
        #t)))
   (inputs
    `(("kodi" ,my-kodi/wayland)
      ,@(map (lambda (addon) (list "addon" addon))
             `(,kodi-pvr-hts))))))
