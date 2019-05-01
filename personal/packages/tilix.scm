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

(define-module (personal tilix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system dub)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages file)
  #:use-module (gnu packages c)
  #:use-module (guix utils)

  #:use-module (gnu packages check)
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
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages textutils))


(define-public gtkd
  (package
   (name "gtkd")
   (version "3.8.5")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://gtkd.org/Downloads/sources/GtkD-" version ".zip"))
     (sha256
      (base32
       "1llnxm13a3k4vcsfmad0l3q1i65ff3qxk0dmy2jb4mp2pp59fr1g"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f    ; there is no test target
      #:make-flags '("shared")
      #:phases (modify-phases %standard-phases
                              (delete 'configure)
                              (replace 'unpack
                                       (lambda* (#:key source #:allow-other-keys)
                                         (invoke "unzip" source)
                                         (substitute* "GNUmakefile" ;; TODO: figure out the way to do this in the snippet part of the source, which currently fails
                                                      ((".*ldconfig -n.*") ""))))
                              (add-before 'build 'setup-env-vars
                                          (lambda* (#:key inputs #:allow-other-keys)
                                            (setenv "DC" "ldc2")))
                              (replace 'install
                                       (lambda* (#:key outputs #:allow-other-keys)
                                         (let* ((prefix (assoc-ref outputs "out")))
                                           (setenv "prefix" prefix)
                                           (invoke "make"
                                                   "install-shared"
                                                   "install-shared-gtkdgl"
                                                   "install-headers"
                                                   "install-headers-gtkdgl"
                                                   "install-headers-gstreamer"
                                                   "install-headers-vte"
                                                   "install-headers-peas")))))))
   (native-inputs
    `(("ldc" ,ldc)
      ("which" ,which)
      ("unzip" ,unzip)))
   (inputs
    `(("vte" ,vte)
      ("atk" ,atk)
      ("librsvg" ,librsvg)
      ("pango" ,pango)
      ("cairo" ,cairo)
      ("gdk-pixbuf" ,gdk-pixbuf)
      ("libpeas" ,libpeas)
      ("gstreamer" ,gstreamer)
      ("gtk+" ,gtk+)))
   (home-page "")
   (synopsis "gtkd")
   (description "gtkd")
   (license license:gpl2+)))


(define-public tilix
  (package
   (name "tilix")
   (version "1.9.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/gnunn1/tilix/archive/" version ".tar.gz"))
     (sha256
      (base32
       "1w5k99sdxi07lj886xb2yw4ikp4492swd93377z8k3hfddvqcaqr"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
                              (add-before 'configure 'setup-env-vars
                                          (lambda* (#:key inputs outputs #:allow-other-keys)
                                            (setenv "GTKD_CFLAGS"
                                                    (string-append "-I" (assoc-ref inputs "gtkd")  "/include/d/gtkd-3"))
                                            (setenv "GTKD_LIBS" "-L-ldl -L-lvted-3 -L-lgtkd-3 -L-latk-1.0 -L-lgio-2.0 -L-lgdk-3 -L-lgtk-3 -L-lrsvg-2 -L-lvte-2.91 -L-lsecret-1")
                                            (setenv "DCFLAGS" "-O -release -link-defaultlib-shared"))))))
   (native-inputs
    `(("ldc" ,ldc)
      ("autoconf" ,autoconf)
      ("automake" ,automake)
      ("gettext" ,gnu-gettext)
      ("pkg-config", pkg-config)
      ("glib" ,glib "bin")        ; for glib-compile-resources
      ("gdk-pixbuf" ,gdk-pixbuf)
      ("po4a" ,po4a)))
   (inputs
    `(("gtkd" ,gtkd)
      ("libx11", libx11)
      ("atk", atk)
      ("librsvg" ,librsvg)
      ("libsecret", libsecret)
      ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
      ("dconf" ,dconf)
      ("vte" ,vte)))
   (home-page "")
   (synopsis "")
   (description
    "")
   (license license:expat)))
