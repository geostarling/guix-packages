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
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)

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

;; (define-public sbcl-osicat
;;   (let ((commit "9a4e0414110981778e9b92fb50c547c573c9bbf7")
;;         (revision "1"))
;;     (package
;;       (name "sbcl-osicat")
;;       (version (git-version "0.7.0" revision commit))
;;       (source
;;        (origin
;;          (method git-fetch)
;;          (uri (git-reference
;;                (url "https://github.com/osicat/osicat.git")
;;                (commit commit)))
;;          (file-name (git-file-name name version))
;;          (sha256
;;           (base32
;;            "0l8l608x2mjbbpzy83l9da2h02c2yka15irzs44qi2x9wyn0hdqb"))))
;;       (build-system asdf-build-system/sbcl)
;;       (arguments
;;        '(#:tests? #f))
;;       (inputs
;;        `(("cffi-grovel" ,sbcl-cffi-grovel)
;;          ("trivial-features" ,sbcl-trivial-features)))
;;       (synopsis "Common Lisp bindings to libuv")
;;       (description
;;        "This library provides low-level libuv bindings for Common Lisp.")
;;       (home-page "https://github.com/orthecreedence/cl-libuv")
;;       (license license:expat))))


;; (define-public sbcl-cepl
;;   (let ((commit "c868bb3da3ce5a44ff6a90abfa475b2d908f4fcc")
;;         (revision "1"))
;;     (package
;;       (name "sbcl-cepl")
;;       (version (git-version "0.1.0" revision commit))
;;       (source
;;        (origin
;;          (method git-fetch)
;;          (uri (git-reference
;;                (url "https://github.com/cbaggers/cepl.git")
;;                (commit commit)))
;;          (file-name (git-file-name name version))
;;          (sha256
;;           (base32
;;            "0g7dihjx7jfdh66lfdl58yb4adi09zb47076k86n7b20gdsjwbzj"))))
;;       (build-system asdf-build-system/sbcl)
;;       (arguments
;;        '(#:tests? #f))
;;       (inputs
;;        `(("cffi" ,sbcl-cffi)))
;;       (synopsis "Common Lisp bindings to libuv")
;;       (description
;;        "This library provides low-level libuv bindings for Common Lisp.")
;;       (home-page "https://github.com/orthecreedence/cl-libuv")
;;       (license license:expat))))


;; (define-public sbcl-opengl
;;   (let ((commit "4aacad022ed099458c00b55cdab68a6a")
;;         (revision "1"))
;;     (package
;;       (name "sbcl-opengl")
;;       (version (git-version "0.0.0" revision commit))
;;       (source
;;        (origin
;;          (method git-fetch)
;;          (uri
;;           (git-reference
;;            (url (string-append "https://gitlab.common-lisp.net/"
;;                                "cl-opengl/cl-opengl.git"))
;;            (commit commit)))
;;          (file-name (git-file-name name version))
;;          (sha256
;;           (base32
;;            "1jz27gz8gvqdmvp3k9bxschs6d5b3qgk94qp2bj6nv1d0jc3m1l1"))))
;;       (arguments
;;        ;; Guix incorrectly assumes the "8" is part of the version
;;        ;; number and lobs it off.
;;        `(#:asd-file "cl-opengl.asd"
;;          #:asd-system-name "cl-opengl"))
;;       (build-system asdf-build-system/sbcl)
;;       (synopsis "UTF-8 input/output library")
;;       (description
;;        "The Babel library solves a similar problem while understanding more
;; encodings.  Trivial UTF-8 was written before Babel existed, but for new
;; projects you might be better off going with Babel.  The one plus that Trivial
;; UTF-8 has is that it doesn't depend on any other libraries.")
;;       (home-page "https://common-lisp.net/project/trivial-utf-8/")
;;       (license license:bsd-3))))



;; (define-public ulubis
;;   (package
;;     (name "ulubis")
;;     (version "0.3")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/malcolmstill/ulubis.git")
;;              (commit "master")))
;;        (file-name (git-file-name "ulubis" version))
;;        (sha256
;;         (base32 "17spxb5ajhfsxx4zpmhkr7n2y000jnm3dgcb7q0r44rf4g89yv1i"))))
;;     (build-system asdf-build-system/sbcl)
;;     (native-inputs `());("fiasco" ,sbcl-fiasco)
;;                        ;("texinfo" ,texinfo)))
;;     (inputs `(;("cffi-toolchain", sbcl-cffi-toolchain)
;;               ("cffi" ,sbcl-cffi)
;;               ("osicat" ,sbcl-osicat)
;;               ("slime-swank" ,cl-slime-swank)))
;;     (outputs '("out" "lib"))
;;     (arguments
;;      '(#:asd-system-name "ulubis"))
;;        ;; #:phases
;;        ;; (modify-phases %standard-phases
;;        ;;   (add-after 'create-symlinks 'build-program
;;        ;;     (lambda* (#:key outputs #:allow-other-keys)
;;        ;;       (build-program
;;        ;;        (string-append (assoc-ref outputs "out") "/bin/stumpwm")
;;        ;;        outputs
;;        ;;        #:entry-program '((stumpwm:stumpwm) 0))))
;;        ;;   (add-after 'build-program 'create-desktop-file
;;        ;;     (lambda* (#:key outputs #:allow-other-keys)
;;        ;;       (let* ((out (assoc-ref outputs "out"))
;;        ;;              (xsessions (string-append out "/share/xsessions")))
;;        ;;         (mkdir-p xsessions)
;;        ;;         (call-with-output-file
;;        ;;             (string-append xsessions "/stumpwm.desktop")
;;        ;;           (lambda (file)
;;        ;;             (format file
;;        ;;              "[Desktop Entry]~@
;;        ;;               Name=stumpwm~@
;;        ;;               Comment=The Stump Window Manager~@
;;        ;;               Exec=~a/bin/stumpwm~@
;;        ;;               TryExec=~@*~a/bin/stumpwm~@
;;        ;;               Icon=~@
;;        ;;               Type=Application~%"
;;        ;;              out)))
;;        ;;         #t)))
;;        ;;   (add-after 'install 'install-manual
;;        ;;     (lambda* (#:key outputs #:allow-other-keys)
;;        ;;       ;; The proper way to the manual is bootstrapping a full autotools
;;        ;;       ;; build system and running ‘./configure && make stumpwm.info’ to
;;        ;;       ;; do some macro substitution.  We can get away with much less.
;;        ;;       (let* ((out  (assoc-ref outputs "out"))
;;        ;;              (info (string-append out "/share/info")))
;;        ;;         (invoke "makeinfo" "stumpwm.texi.in")
;;        ;;         (install-file "stumpwm.info" info)
;;        ;;         #t))))))
;;     (synopsis "Window manager written in Common Lisp")
;;     (description "Stumpwm is a window manager written entirely in Common Lisp.
;; It attempts to be highly customizable while relying entirely on the keyboard
;; for input.  These design decisions reflect the growing popularity of
;; productive, customizable lisp based systems.")
;;     (home-page "https://github.com/stumpwm/stumpwm")
;;     (license license:gpl2+)))
;; ;    (properties `((cl-source-variant . ,(delay cl-stumpwm))))))

(define-public sbcl-swm-lirc
  (let ((commit "63749cbba313c33e742ba934cebc6cf1287820c7")
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
           "1pihglspzp10984ca2fmh5snsl68sbbbxc3406g37xix8gd8dyg7"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("stumpwm" ,stumpwm)
         ("iolib" ,sbcl-iolib)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)))
      (native-inputs
       `(("trivial-features" ,sbcl-trivial-features)
         ("stumpwm" ,cl-stumpwm)))
      (arguments
       `(#:tests? #f))
      (synopsis "Coroutine library for Common Lisp")
      (description
       "This is a coroutine library for Common Lisp implemented using the
continuations of the @code{cl-cont} library.")
      (home-page "https://github.com/takagi/cl-coroutine")
      (license license:llgpl))))


(define-public stumpwm+slynk+modules
  (package
    (inherit stumpwm)
    (name "stumpwm-with-slynk-and-modules")
    (outputs '("out"))
    (inputs
     `(("stumpwm" ,stumpwm "lib")
       ("slynk" ,sbcl-slynk)
       ("swm-lirc" ,sbcl-swm-lirc)
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
                                                 "trivial-features")

                                #:dependency-prefixes
                                (map (lambda (input) (assoc-ref inputs input))
                                     '("stumpwm" "slynk" "swm-lirc" "trivial-features")))
                 ;; Remove unneeded file.
                 (delete-file (string-append out "/bin/stumpwm-exec.fasl"))
                 #t)))
           (delete 'copy-source)
           (delete 'build)
           (delete 'check)
           (delete 'create-asd-file)
           (delete 'cleanup)
           (delete 'create-symlinks)))))))
