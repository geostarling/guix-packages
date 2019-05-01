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

(define-module (personal packages xfce)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages file)
  #:use-module (gnu packages c)

  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gnome)      ;; for vte and libpeas
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xfce)

  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages textutils))

(define-public xfce4-sensors-plugin
  (package
    (name "xfce4-sensors-plugin")
    (version "1.3.92")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "04jibw23ibi61f19gc9xy400yhcdiya4px6zp8c7fjq65hyn9iix"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("intltool" ,intltool)))
    (inputs `(("gtk+" ,gtk+)
              ("lm-sensors" ,lm-sensors "lib")
              ("libxfce4ui" ,libxfce4ui)
              ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
    (synopsis "Hardware sensors plugin for Xfce4")
    (description
     "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
    ;; The main plugin code is covered by gpl2+, but the files containing code
    ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
    (license (list license:gpl2+ license:lgpl2.0+))))


;; (define-public xfce4-screenshooter
;;   (package
;;     (name "xfce4-screenshooter")
;;     (version "1.8.99")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append "https://archive.xfce.org/src/apps/"
;;                                   name "/" (version-major+minor version) "/"
;;                                   name "-" version ".tar.bz2"))
;;               (sha256
;;                (base32
;;                 "1injwvjzyl7jfp586xmhwpzhhmzfnb6r6pi3i3llsvp8daazsy1s"))))
;;     (build-system gnu-build-system)
;;     (native-inputs `(("pkg-config" ,pkg-config)
;;                      ("intltool" ,intltool)))
;;     (inputs `(("gtk+" ,gtk+)
;;               ("libxfce4ui" ,libxfce4ui)
;;               ("libxfce4util" ,libxfce4util)
;;               ("xfce4-panel" ,xfce4-panel)))
;;     (home-page
;;      "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
;;     (synopsis "Hardware sensors plugin for Xfce4")
;;     (description
;;      "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
;;     ;; The main plugin code is covered by gpl2+, but the files containing code
;;     ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
;;     (license (list license:gpl2+ license:lgpl2.0+))))
