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

(define-module (personal packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system dub)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system meson)

  #:use-module (gnu packages texinfo)
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
  #:use-module (gnu packages video)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages man)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gnome)      ;; for vte and libpeas
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xorg)

  #:use-module (gnu packages lisp-xyz)

  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages guile-wm))

(define-public sbcl-swm-lirc
  (let ((commit "72bc09eecbcdb989b8d80fa2be8361ead058b2ea")
        (revision "1"))
    (package
      (name "sbcl-swm-lirc")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/geostarling/swm-lirc.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0i90nf1limn2n50r62a3fsw6jbvm1g45a5fdjc8rm3ixdwmr3lks"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:tests? #f))
      (inputs
       `(("stumpwm" ,stumpwm)
         ("iolib" ,sbcl-iolib)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)))
      (native-inputs
       `(("trivial-features" ,sbcl-trivial-features)
         ("stumpwm" ,cl-stumpwm)))
      (synopsis "Coroutine library for Common Lisp")
      (description
       "This is a coroutine library for Common Lisp implemented using the
continuations of the @code{cl-cont} library.")
      (home-page "https://github.com/takagi/cl-coroutine")
      (license license:llgpl))))


(define stumpwm-contrib-source
  (let ((commit "dd5b037923ec7d3cc27c55806bcec5a1b8cf4e91")
        (revision "1"))
    (package
      (name "stumpwm-contrib")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stumpwm/stumpwm-contrib.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0ahxdj9f884afpzxczx6mx7l4nwg4kw6afqaq7lwhf7lxcwylldn"))))
      (build-system trivial-build-system)
      (synopsis "Coroutine library for Common Lisp")
      (description
       "This is a coroutine library for Common Lisp implemented using the
continuations of the @code{cl-cont} library.")
      (home-page "https://github.com/takagi/cl-coroutine")
      (license license:llgpl))))


(define-public sbcl-kbd-layouts
  (let ((commit "420a47ebba1f54f3e9c618164dbdb73d54ebb404")
        (revision "1"))
    (package
      (name "sbcl-kbd-layouts")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/geostarling/swm-kbd-layouts.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "085i9i3wras1sj9ik5znlvia6ac97sxdp1ajlrj3y6x0p2zagm9a"))))
      (build-system asdf-build-system/sbcl)
      (propagated-inputs
       `(("setxkbmap" ,setxkbmap)
         ("bash" ,bash)))
      (native-inputs
       `(("stumpwm" ,cl-stumpwm)))
      (synopsis "Coroutine library for Common Lisp")
      (description
       "This is a coroutine library for Common Lisp implemented using the
continuations of the @code{cl-cont} library.")
      (home-page "https://github.com/takagi/cl-coroutine")
      (license license:llgpl))))


(define-public sbcl-clipboard-history
  (package
   (name "sbcl-clipboard-history")
   (version (package-version stumpwm-contrib-source))
   (source (package-source stumpwm-contrib-source))
   (build-system asdf-build-system/sbcl)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'enter-subdirectory
                                (lambda _ (chdir "util/clipboard-history") #t)))))
   (inputs
    `(("stumpwm" ,stumpwm)))
   (native-inputs
    `(("stumpwm" ,cl-stumpwm)))
   (synopsis "Coroutine library for Common Lisp")
   (description
    "This is a coroutine library for Common Lisp implemented using the
continuations of the @code{cl-cont} library.")
   (home-page "https://github.com/takagi/cl-coroutine")
   (license license:llgpl)))

(define-public stumpwm+slynk+modules
  (package
    (inherit stumpwm)
    (name "stumpwm-with-slynk-and-modules")
    (outputs '("out"))
    (inputs
     `(("stumpwm" ,stumpwm "lib")
       ("slynk" ,sbcl-slynk)
       ("swm-lirc" ,sbcl-swm-lirc)
       ("kbd-layouts" ,sbcl-kbd-layouts)
       ("clipboard-history" ,sbcl-clipboard-history)
       ("trivial-features" ,sbcl-trivial-features)))
    (arguments
     (substitute-keyword-arguments (package-arguments stumpwm)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (program (string-append out "/bin/stumpwm")))
                 (build-program program outputs
                                #:entry-program '((stumpwm:stumpwm) 0)
                                #:dependencies '("stumpwm"
                                                 ,@(@@ (gnu packages lisp-xyz) slynk-systems)
                                                 "swm-lirc"
                                                 "kbd-layouts"
                                                 "clipboard-history"
                                                 "trivial-features")

                                #:dependency-prefixes
                                (map (lambda (input) (assoc-ref inputs input))
                                     '("stumpwm" "slynk" "swm-lirc" "kbd-layouts" "clipboard-history" "trivial-features")))
                 ;; Remove unneeded file.
                 (delete-file (string-append out "/bin/stumpwm-exec.fasl"))
                 #t)))
           (delete 'copy-source)
           (delete 'build)
           (delete 'check)
           (delete 'create-asd-file)
           (delete 'cleanup)
           (delete 'create-symlinks)))))))

(define-public kanshi
  (package
    (name "kanshi")
    (version "1.1.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/emersion/kanshi/releases/download/v"
                                 version "/kanshi-" version ".tar.gz"))

             (sha256
              (base32
               "0rk5w8c4nps7g68w3cwlvkchds4zmxs8sfr1f39sx04knk985rzp"))))
    (build-system meson-build-system)
    (inputs `(("wayland" ,wayland)))
    (native-inputs `(("scdoc" ,scdoc)
                     ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/swaywm/wlroots")
    (synopsis "Pluggable, composable, unopinionated modules for building a
Wayland compositor")
    (description "wlroots is a set of pluggable, composable, unopinionated
modules for building a Wayland compositor.")
    (license license:expat)))  ; MIT license
