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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
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
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gnome)      ;; for vte and libpeas
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages wm)

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
