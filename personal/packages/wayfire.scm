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

(define-module (personal packages wayfire)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages file)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages c)
  #:use-module (gnu packages commencement)
  #:use-module (guix utils)
  #:use-module (ice-9 match)


  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gnome)      ;; for vte and libpeas
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages wm)
  #:use-module (guix build utils)

  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop))

(define-public wayland-protocols-24
  (package
    (name "wayland-protocols")
    (version "1.24")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://wayland.freedesktop.org/releases/"
                    "wayland-protocols-" version ".tar.xz"))
              (sha256
               (base32
                "1hlb6gvyqlmsdkv5179ccj07p04cn6xacjkgklakbszczv7xiw5z"))))
    (build-system meson-build-system)
    (inputs
     (list wayland))
    (native-inputs
     (list pkg-config python))
    (synopsis "Wayland protocols")
    (description "Wayland-Protocols contains Wayland protocols that add
functionality not available in the Wayland core protocol.  Such protocols either
add completely new functionality, or extend the functionality of some other
protocol either in Wayland core, or some other protocol in wayland-protocols.")
    (home-page "https://wayland.freedesktop.org")
    (properties
     '((release-monitoring-url
        . "https://wayland.freedesktop.org/releases.html")))
    (license license:expat)))


(define-public libdrm-109
  (package
    (name "libdrm")
    (version "2.4.109")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "09kzrdsd14zr0i3izvi5mck4vqccl3c9hr84r9i4is0zikh554v2"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       '(,@(match (%current-system)
             ((or "armhf-linux" "aarch64-linux")
              '("-Dexynos=true"
                "-Domap=true"
                "-Detnaviv=true"
                "-Dtegra=true"
                "-Dfreedreno-kgsl=true"))
             (_ '())))

       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "meson" "test" "--timeout-multiplier" "5")))))))
    (propagated-inputs
     (list libpciaccess))
    (native-inputs
     (list pkg-config))
    (home-page "https://dri.freedesktop.org/wiki/")
    (synopsis "Direct rendering userspace library")
    (description "The Direct Rendering Infrastructure, also known as the DRI,
is a framework for allowing direct access to graphics hardware under the
X Window System in a safe and efficient manner.  It includes changes to the
X server, to several client libraries, and to the kernel (DRM, Direct
Rendering Manager).  The most important use for the DRI is to create fast
OpenGL implementations providing hardware acceleration for Mesa.
Several 3D accelerated drivers have been written to the DRI specification,
including drivers for chipsets produced by 3DFX, AMD (formerly ATI), Intel
and Matrox.")
    (license license:x11)))

;; NOTE: I don't really get it. It should not be neccessary to specify many of dependencies (deps of wlroots for example) because they should be transitively resolved (eg. libinput is definitely dep of wlroots so why we have to specify it again?) is it meson-related?
(define-public wayfire
  (package
    (name "wayfire")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/WayfireWM/wayfire/releases/download/v" version "/wayfire-" version ".tar.xz"))
              (sha256
               (base32
                "0kaiy2kk8yc9zl6amdgv99chjwpfg7wrk54z7l1d8yqd6bvpbqw9"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
                      (add-before 'configure 'fix-meson-build
                                  (lambda _
                                    (substitute* "src/meson.build"
                                      (("^.*/bin/sh.*$")
                                       "cxx_flags_asan = run_command('sh', '-c', 'echo $CXXFLAGS $CPPFLAGS | grep fsanitize')\n")))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("cmake" ,cmake)
                     ("bash" ,bash)
                     ("git" ,git)
                     ("glibc" ,glibc)
                     ("gcc" ,gcc)))
    (inputs `(("wlroots" ,wlroots)
              ("glm" ,glm)
              ("libcap" ,libcap) ;; dep of wlroots
              ("elogind" ,elogind) ;; dep of wlroots
              ("wayland" ,wayland)
              ("wayland-protocols" ,wayland-protocols-24)
              ("cairo" ,cairo)
              ("libdrm" ,libdrm-109)
              ("mesa" ,mesa)
              ("pango" ,pango)
              ("libxml2" ,libxml2)
              ("libevdev" ,libevdev)
              ("libinput" ,libinput)
              ("libxkbcommon" ,libxkbcommon)))

    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
    (synopsis "3D wayland compositor")
    (description
     "")
    (license (list license:gpl2+ license:lgpl2.0+))))


(define-public wf-config
  (package
    (name "wf-config")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/WayfireWM/wf-config/releases/download/v" version "/wf-config-" version  ".tar.xz"))
              (sha256
               (base32
                "1w75yxhz0nvw4mlv38sxp8k8wb5h99b51x3fdvizc3yaxanqa8kx"))))
    (build-system meson-build-system)
    ;; (arguments
    ;;  `(#:phases
    ;;    (modify-phases %standard-phases
    ;;                   (add-before 'configure 'fixgcc7
    ;;                               (lambda _
    ;;                                 (unsetenv "C_INCLUDE_PATH")
    ;;                                 (unsetenv "CPLUS_INCLUDE_PATH"))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("cmake" ,cmake)
                     ("glibc" ,glibc)
                     ("gcc" ,gcc-7)))
    (inputs `(("wlroots" ,wlroots)
              ("wayland" ,wayland)
              ("wayland-protocols" ,wayland-protocols)
              ("mesa" ,mesa)
              ("libcap" ,libcap) ;; dep of wlroots
              ("elogind" ,elogind) ;; dep of wlroots
              ("libevdev" ,libevdev)
              ("libxml2" ,libxml2)
              ("cairo" ,cairo)
              ("libdrm" ,libdrm)
              ("libinput" ,libinput)
              ("libxkbcommon" ,libxkbcommon)
              ("glm" ,glm)))

    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
    (synopsis "3D wayland compositor")
    (description
     "")
    (license (list license:gpl2+ license:lgpl2.0+))))


(define-public wf-shell
  (package
    (name "wf-shell")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/WayfireWM/wf-shell/releases/download/v" version "/wf-shell-" version  ".tar.xz"))
              (sha256
               (base32
                "1isybm9lcpxwyf6zh2vzkwrcnw3q7qxm21535g4f08f0l68cd5bl"))))
    (build-system meson-build-system)
    ;; (arguments
    ;;  `(#:phases
    ;;    (modify-phases %standard-phases
    ;;                   (add-before 'configure 'fixgcc7
    ;;                               (lambda _
    ;;                                 (unsetenv "C_INCLUDE_PATH")
    ;;                                 (unsetenv "CPLUS_INCLUDE_PATH"))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("cmake" ,cmake)
                     ("gcc" ,gcc-7)))
    (inputs `(("wayland" ,wayland)
              ("wayland-protocols" ,wayland-protocols)
              ("wf-config" ,wf-config)
              ("wayfire" ,wayfire)
              ("gobject-introspection" ,gobject-introspection)
              ("libxml2" ,libxml2)
              ("gtkmm" ,gtkmm-3)
              ("mesa" ,mesa)
              ("libcap" ,libcap) ;; dep of wlroots
              ("elogind" ,elogind) ;; dep of wlroots
              ("libevdev" ,libevdev)
              ("cairo" ,cairo)
              ("libdrm" ,libdrm)
              ("libinput" ,libinput)
              ("libxkbcommon" ,libxkbcommon)
              ("glm" ,glm)
              ("wlroots" ,wlroots)))

    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
    (synopsis "3D wayland compositor")
    (description
     "")
    (license (list license:gpl2+ license:lgpl2.0+))))

;; (define-public bemenu
;;   (package
;;     (name "bemenu")
;;     (version "0.2")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;               (url "https://github.com/Cloudef/bemenu")
;;               (commit "442d2833f48590122e5ce54a2bca3a327ffa0311")))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32
;;          "00km3caxq8lzd4fv6xn504abms292d739pi021img20ip4rxyzxf"))))
;;     (build-system cmake-build-system)
;;     (native-inputs `(("pkg-config" ,pkg-config)
;;                      ("cmake" ,cmake)))
;;     (inputs `(("wayland" ,wayland)
;;               ("ncurses" ,ncurses)
;;               ("libx11" ,libx11)
;;               ("libxinerama" ,libxinerama)
;;               ("cairo" ,cairo)
;;               ("pango" ,pango)))
;;     (home-page
;;      "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
;;     (synopsis "3D wayland compositor")
;;     (description
;;      "")
;;     (license (list license:gpl2+ license:lgpl2.0+))))


(define-public wcm
  (package
    (name "wcm")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/WayfireWM/wcm/releases/download/v" version "/wcm-" version  ".tar.xz"))
              (sha256
               (base32
                "19za1fnlf5hz4n4mxxwqcr5yxp6mga9ah539ifnjnqrgvj19cjlj"))))
    (build-system meson-build-system)
    ;; (arguments
    ;;  `(#:phases
    ;;    (modify-phases %standard-phases
    ;;                   (add-before 'configure 'fixgcc7
    ;;                               (lambda _
    ;;                                 (unsetenv "C_INCLUDE_PATH")
    ;;                                 (unsetenv "CPLUS_INCLUDE_PATH"))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("cmake" ,cmake)
                     ("gcc" ,gcc-7)))
    (inputs `(("wf-shell" ,wf-shell)
              ("wayfire" ,wayfire)
              ("wlroots" ,wlroots)
              ("cairo" ,cairo)
              ("libxml2" ,libxml2)
              ("wayland" ,wayland)
              ("wayland-protocols" ,wayland-protocols)
              ("glm" ,glm)
              ("gtk+" ,gtk+)
              ("pango" ,pango)
              ("libevdev" ,libevdev)))

    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
    (synopsis "3D wayland compositor")
    (description
     "")
    (license (list license:gpl2+ license:lgpl2.0+))))
