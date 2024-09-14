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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages groovy)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages kodi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
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
  #:use-module (personal packages kodi-xyz)
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages assembly))


(define-public libudfread
  (package
    (name "libudfread")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://code.videolan.org/videolan/libudfread/-/archive/" version "/libudfread.tar.gz"))
              (sha256
               (base32
                "1wva8hzadcpg9y1c665a7361pwl15p4f9qaf8wii4l7wayijmr8p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (invoke "autoreconf" "-vfi")
             (delete-file-recursively "autom4te.cache"))))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
    (synopsis "Hardware sensors plugin for Xfce4")
    (description
     "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
    ;; The main plugin code is covered by gpl2+, but the files containing code
    ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
    (license (list license:gpl2+ license:lgpl2.0+))))


;; Kodi requires using their own special forks of these libraries.
;; In addition, it insists on downloading and building these as part
;; of the standard build process. To make things easier, we bootstrap
;; and patch shebangs here, so we don't have to worry about it later.
(define libdvdnav/kodi
  (let ((commit "25229a54b2c4be9930d677f6abe3a0025514f3db")
        (revision "0"))
    (package
      (name "libdvdnav-bootstrapped")
      (version (git-version "6.1.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdnav")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1sfk4016aihqlgd1by68as6m0nm7vahn0pz5xl4vqm94xa685i4v"))))
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
  (let ((commit "329ec88257ecaf64a3144644a423289bb3da8ef6"))
    (package
      (name "libdvdread-bootstrapped")
      (version "6.1.3")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdread")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0xxrk7fhx45xlih6gab7fgkv7f75q8ycwxkfqaxfcxzf2m143602"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (delete-file-recursively "autom4te.cache")
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
  (let ((commit "84a7ba82a31f4ac73d93de108ee8eaa2d250cf5e")
        (revision "0"))
    (package
      (name "libdvdcss-bootstrapped")
      (version (git-version "1.4.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdcss")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "191fk6imrxkd2bksary564kia0m76b25p25vrm93gdmfi79psw68"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (delete-file-recursively "autom4te.cache")
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


(define-public kodi-21
  (package
    (name "kodi")
    (version "21.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xbmc/xbmc")
                    (commit (string-append version "-Omega"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07h928hmjcarjw2z2hvf99apa8wf48xl0xaws1jxghsw7paiscin"))
              (patches (search-patches "kodi-set-libcurl-ssl-parameters.patch"
                                       ;;"kodi-mesa-eglchromium.patch"
                                       ))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (use-modules (guix build utils))
                  (for-each delete-file-recursively
                            '("project/BuildDependencies/"

                              ;;/tmp/guix-build-kodi-21.0.drv-12/build/build/libdvdcss/src/libdvdcss/autom4te.cache
                             ;;"tools/depends/native/JsonSchemaBuilder/src/autom4te.cache"

                              ;; --no-cache

                              ;;"tools/codegenerator/groovy/commons-lang-2.6.jar"
                              ;;"tools/codegenerator/groovy/groovy-all-2.4.4.jar"
                              ;; Purge these sources:
                              ;; "tools/depend/native/JsonSchemaBuilder"
                              ;; "tools/depend/native/TexturePacker"
                              ;; "lib/libUPnP"
                              "lib/libUPnP/Neptune/ThirdParty"
                              "project/Win32BuildSetup/tools/7z"))))))
    (build-system cmake-build-system)
    (arguments
     '(#:modules ((srfi srfi-1)
                  (guix build cmake-build-system)
                  (guix build utils))
       #:configure-flags
       (list "-DCORE_PLATFORM_NAME=x11"
             "-DAPP_RENDER_SYSTEM=gl"
             "-DENABLE_INTERNAL_FFMPEG=OFF"
             "-DENABLE_INTERNAL_CROSSGUID=OFF"
             (string-append "-Dgroovy_SOURCE_DIR=" (assoc-ref %build-inputs "groovy"))
             (string-append "-Dapache-commons-lang_SOURCE_DIR=" (assoc-ref %build-inputs "java-commons-lang"))
             (string-append "-Dapache-commons-text_SOURCE_DIR=" (assoc-ref %build-inputs "java-commons-text"))
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
             (let ((dirs '("tools/depends/native/JsonSchemaBuilder/src")))
               (every (lambda (third-party)
                        (with-directory-excursion third-party
                          (invoke "autoreconf" "-vif")))
                      dirs))))
         (add-after 'bootstrap-bundled-software 'patch-stuff
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Run groovy executable directly.
             (substitute* "xbmc/interfaces/swig/CMakeLists.txt"
               (("COMMAND \\$\\{Java_JAVA_EXECUTABLE\\}")
                "COMMAND groovy")
               (("ARGS \\$\\{JAVA_OPEN_OPTS\\} -cp \"\\$\\{classpath\\}\" groovy.ui.GroovyMain")
                "ARGS -cp \"${classpath}\" ")
               (("classpath \\$\\{GROOVY_DIR\\}/groovy-all-\\$\\{GROOVY_VER\\}.jar")
                "classpath ")
               (("\\$\\{apache-commons-lang_SOURCE_DIR\\}/\\*")
                (search-input-file inputs "/lib/m2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"))
               ;;                (string-append (assoc-ref %build-inputs "java-commons-lang") "/m2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"))
                ;;(search-input-file inputs "/m2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"))
               (("\\$\\{apache-commons-text_SOURCE_DIR\\}/\\*")
                (search-input-file inputs "/lib/m2/org/apache/commons/commons-text/1.9/commons-text-1.9.jar"))
               (("^set\\(GROOVY_VER.*")
                (string-append "set(GROOVY_VER 3.0.5)\n")))

             ;; Prevent the build scripts from calling autoreconf in the
             ;; build stage.  Otherwise, it would undo the bootstrapping
             ;; and shebang patching that we worked so hard for.
             (for-each (lambda (file)
                         (substitute* file
                           ;; The libdvd* sources that we bootstrapped separately are
                           ;; unpacked in the build phase. This is our best opportunity
                           ;; to make them writable before the build process starts.
                           (("\\$\\{AUTORECONF\\} -vif") "chmod -R u+w .")))
                       '("cmake/modules/FindLibDvd.cmake"
                         "cmake/modules/FindLibDvdCSS.cmake"
                         "cmake/modules/FindLibDvdNav.cmake"
                         "cmake/modules/FindLibDvdRead.cmake"))

             (substitute* "xbmc/platform/posix/PosixTimezone.cpp"
               (("/usr/share/zoneinfo")
                (search-input-directory inputs "share/zoneinfo")))

             ;; Don't phone home to check for updates.
             (substitute* "system/addon-manifest.xml"
               (("<addon optional=\\\"true\\\">service.xbmc.versioncheck</addon>")
                ""))

             ;; Let's disable some tests that are known not to work here.
             ;; Doing this later while in the cmake "../build" directory
             ;; is trickier.
             (substitute* "xbmc/utils/test/TestSystemInfo.cpp"
               (("TEST_F\\(TestSystemInfo, GetOsPrettyNameWithVersion\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsPrettyNameWithVersion)")
               (("TEST_F\\(TestSystemInfo, GetOsName\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsName)")
               (("TEST_F\\(TestSystemInfo, GetOsVersion\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsVersion)"))
             (substitute* "xbmc/utils/test/TestCPUInfo.cpp"
               (("TEST_F\\(TestCPUInfo, GetCPUFrequency\\)")
                "TEST_F(TestCPUInfo, DISABLED_GetCPUFrequency)"))
             (substitute* "xbmc/network/test/TestNetwork.cpp"
               (("TEST_F\\(TestNetwork, PingHost\\)")
                "TEST_F(TestNetwork, DISABLED_PingHost)"))))
         (add-before 'build 'set-build-environment
           (lambda _
             ;; Some bundled build scripts fall back to /bin/sh
             ;; if this is not set.
             (setenv "CONFIG_SHELL" (which "sh"))))
         (add-before 'check 'build-kodi-test
           (lambda _
             (invoke "make" "kodi-test"))))))
    ;; TODO: Add dependencies for:
    ;; - cec
    ;; - plist
    ;; - shairplay
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           googletest
           groovy
           openjdk9                     ;like groovy
           java-commons-lang
           java-commons-text
           libdvdcss/kodi
           libdvdnav/kodi
           libdvdread/kodi
           libtool
           pkg-config
           swig
           yasm))
    (inputs
     (list alsa-lib
           avahi
           bluez
           crossguid
           curl
           dcadec
           dbus
           eudev
           ffmpeg
           flac
           flatbuffers
           fmt
           fontconfig
           freetype
           fribidi
           fstrcmp
           giflib
           glew
           gnutls
           lame
           lcms
           libass
           libbluray
           libcap
           libcdio
           libdrm
           libgcrypt
           libjpeg-turbo
           libltdl
           libmad
           libmicrohttpd
           libmpeg2
           libnfs
           libogg
           libpng
           libssh
           libtiff
           libudfread
           libva
           libvorbis
           libxml2
           libxrandr
           libxrender
           libxslt
           lzo
           (list mariadb "lib")
           (list mariadb "dev")
           openssl
           pcre
           pulseaudio
           python
           rapidjson
           samba
           spdlog
           sqlite
           taglib
           tinyxml
           tinyxml2
           tzdata
           util-linux
           zip
           zlib))
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
                   license:bsd-3         ;misc
                   license:bsd-2))))     ;xbmc/freebsd

(define-public kodi/wayland-21
  (package
    (inherit kodi-21)
    (name "kodi-wayland")
    (arguments
     (substitute-keyword-arguments (package-arguments kodi-21)
       ((#:configure-flags flags)
        `(cons "-DCORE_PLATFORM_NAME=wayland"
               (delete "-DCORE_PLATFORM_NAME=x11" ,flags)))))
    (inputs
     (modify-inputs (package-inputs kodi-21)
       (prepend libinput
                libxkbcommon
                waylandpp
                wayland-protocols)))
    (synopsis "Kodi with Wayland rendering backend")))


(define-public my-kodi/wayland
   (package
     (inherit kodi/wayland-21)
     (name "my-kodi-wayland")
     (inputs
      (modify-inputs (package-inputs kodi/wayland-21)
        (append lirc)))))

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
         (let ((kodi-input (assoc-ref %build-inputs "kodi"))
               (kodi-output (assoc-ref %outputs "out")))
           (union-build kodi-output
                        (map (lambda (input) (cdr input))
                             (filter (lambda (input)
                                       (member (car input) '("kodi" "addon")))
                                     %build-inputs))
                        #:symlink (lambda [input output]
                                    (if (file-is-directory? input)
                                        (copy-recursively input output)
                                        (copy-file input output))))
           (substitute* (string-append kodi-output "/bin/kodi")
             ((kodi-input) kodi-output))
           #t))))
    (inputs
     `(("kodi" ,my-kodi/wayland)
       ,@(map (lambda (addon) (list "addon" addon))
              `(,kodi-pvr-hts
                ,kodi-inputstream-adaptive))))))
