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
  #:use-module (gnu packages c)
  #:use-module (gnu packages commencement)
  #:use-module (guix utils)

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
  #:use-module (gnu packages linux)
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


;; NOTE: I don't really get it. It should not be neccessary to specify many of dependencies (deps of wlroots for example) because they should be transitively resolved (eg. libinput is definitely dep of wlroots so why we have to specify it again?) is it meson-related?

(define-public wayfire
  (package
    (name "wayfire")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/WayfireWM/wayfire/releases/download/v" version "/wayfire-" version  ".tar.xz"))
              (sha256
               (base32
                "0kdkzmw8gwaw8k9ci5fx29i11bf7b2br73fi3rqsl67064xv9l7l"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
                      (add-before 'configure 'fixgcc7
                                  (lambda _
                                    (unsetenv "C_INCLUDE_PATH")
                                    (unsetenv "CPLUS_INCLUDE_PATH"))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("cmake" ,cmake)
                     ("gcc" ,gcc-7)))
    (inputs `(("wlroots" ,wlroots)
              ("glm" ,glm)
              ("libcap" ,libcap) ;; dep of wlroots
              ("elogind" ,elogind) ;; dep of wlroots
              ("wayland" ,wayland)
              ("wayland-protocols" ,wayland-protocols)
              ("cairo" ,cairo)
              ("libdrm" ,libdrm)
              ("mesa" ,mesa)
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
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/WayfireWM/wf-config/releases/download/v" version "/wf-config-" version  ".tar.xz"))
              (sha256
               (base32
                "1ma0b4gl0hing3g00kyckjpfl3g3qi4fm2mgfm6kflqs2xrwwknn"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
                      (add-before 'configure 'fixgcc7
                                  (lambda _
                                    (unsetenv "C_INCLUDE_PATH")
                                    (unsetenv "CPLUS_INCLUDE_PATH"))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("cmake" ,cmake)
                     ("gcc" ,gcc-7)))
    (inputs `(("wlroots" ,wlroots)
              ("wayland" ,wayland)
              ("wayland-protocols" ,wayland-protocols)
              ("mesa" ,mesa)
              ("libcap" ,libcap) ;; dep of wlroots
              ("elogind" ,elogind) ;; dep of wlroots
              ("libevdev" ,libevdev)
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
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/WayfireWM/wf-shell/releases/download/" version "/wf-shell-" version  ".tar.xz"))
              (sha256
               (base32
                "1jgmjclxjnfrxb9l06nm3slxrfsickrkycyvfz38aa5mq6nkqq82"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
                      (add-before 'configure 'fixgcc7
                                  (lambda _
                                    (unsetenv "C_INCLUDE_PATH")
                                    (unsetenv "CPLUS_INCLUDE_PATH"))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("cmake" ,cmake)
                     ("gcc" ,gcc-7)))
    (inputs `(("wayland" ,wayland)
              ("wayland-protocols" ,wayland-protocols)
              ("wf-config" ,wf-config)
              ("gtkmm" ,gtkmm)
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

(define-public bemenu
  (package
    (name "bemenu")
    (version "0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Cloudef/bemenu")
              (commit "442d2833f48590122e5ce54a2bca3a327ffa0311")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00km3caxq8lzd4fv6xn504abms292d739pi021img20ip4rxyzxf"))))
    (build-system cmake-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("cmake" ,cmake)))
    (inputs `(("wayland" ,wayland)
              ("ncurses" ,ncurses)
              ("libx11" ,libx11)
              ("libxinerama" ,libxinerama)
              ("cairo" ,cairo)
              ("pango" ,pango)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
    (synopsis "3D wayland compositor")
    (description
     "")
    (license (list license:gpl2+ license:lgpl2.0+))))



(define-public albert
  (package
   (name "albert")
   (version "0.16.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/albertlauncher/albert")
           (commit (string-append "v" version))
           (recursive? #t)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "04sr35fqz66i24lv7r2p9qfqxs55i8xpj7aam0v9yakcr33lf55a"))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (add-before 'configure 'fix-x11extras
                                 (lambda* (#:key outputs #:allow-other-keys)
                                   (substitute* "plugins/widgetboxmodel/CMakeLists.txt"
                                                (("^.*find_package.*$")
                                                 "find_package(Qt5 5.5.0 REQUIRED COMPONENTS Widgets Svg X11Extras)")
                                                (("^.*target_include_directories.*$")
                                                 "target_include_directories(${PROJECT_NAME} PRIVATE src/)\nfind_package(Qt5 5.5.0 REQUIRED X11Extras)")
                                                (("^.*Qt5::Svg.*$")
                                                 "Qt5::Svg\nQt5::X11Extras"))
                                   (substitute* "plugins/qmlboxmodel/CMakeLists.txt"
                                                (("^.*find_package.*$")
                                                 "find_package(Qt5 5.5.0 REQUIRED COMPONENTS Widgets Qml Quick X11Extras)")
                                                (("^.*target_include_directories.*$")
                                                 "target_include_directories(${PROJECT_NAME} PRIVATE src/)\nfind_package(Qt5 5.5.0 REQUIRED X11Extras)")
                                                (("^.*Qt5::Qml.*$")
                                                 "Qt5::Qml\nQt5::X11Extras"))

;                                   (substitute* "plugins/widgetboxmodel/src/frontend.ui"
;                                                (("^.*<header location=\"global\">actionlist.h</header>.*$")
;                                                 "<header>actionlist.h</header>"))
                                        ;"find_package(Qt5 5.5.0 REQUIRED X11Extras)\nfind_package(Qt5 5.5.0 REQUIRED COMPONENTS Widgets Svg)"))
                                   #t)))))
   (native-inputs `(("pkg-config" ,pkg-config)
                    ("cmake" ,cmake)))
   (inputs `(("qtbase" ,qtbase)
             ("qtx11extras" ,qtx11extras)
             ("qtcharts" ,qtcharts)
             ("qtsvg" ,qtsvg)
             ("qtdeclarative" ,qtdeclarative)
             ("muparser" ,muparser)
             ("python" ,python)))

   (home-page
    "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
   (synopsis "3D wayland compositor")
   (description
    "")
   (license (list license:gpl2+ license:lgpl2.0+))))
