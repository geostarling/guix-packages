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

(define-module (personal packages haskell)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages file)
  #:use-module (gnu packages c)

  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages python)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages linux)

  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages textutils))

(define-public ghc-composition
  (package
   (name "ghc-composition")
   (version "1.0.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/composition/composition-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0smdyzcnfwiab1wnazmk4szali1ckh7dqcdp9vn7qnnabq7k08vi"))))
   (build-system haskell-build-system)
   (home-page
    "http://hackage.haskell.org/package/composition")
   (synopsis
    "Combinators for unorthodox function composition")
   (description "")
   (license license:bsd-3)))


(define-public ghc-env-locale
  (package
    (name "ghc-env-locale")
    (version "1.0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/env-locale/env-locale-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1wgw8z144w5l9sns44jf9acld5zx06jw2yg7m2yq868lwfwzxwgj"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-old-locale" ,ghc-old-locale)))
    (home-page "https://github.com/Ongy/locale-hs")
    (synopsis
      "A (non-forking) interface to the current locale")
    (description
      "A sane way to get the time locale defined by environment")
    (license license:lgpl3)))

(define-public ghc-formatting
  (package
    (name "ghc-formatting")
    (version "6.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/formatting/formatting-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "06jwb1pmh15f2b9dfplm64y9yszazg26m4h7rl4dn4inqg14znqc"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-text" ,ghc-text)
        ("ghc-old-locale" ,ghc-old-locale)
        ("ghc-scientific" ,ghc-scientific)
        ("ghc-clock" ,ghc-clock)
        ("ghc-semigroups" ,ghc-semigroups)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)))
    (home-page
      "http://hackage.haskell.org/package/formatting")
    (synopsis
      "Combinator-based type-safe formatting (like printf() or FORMAT)")
    (description
      "Combinator-based type-safe formatting (like printf() or FORMAT), modelled from the HoleyMonoids package.")
    (license license:bsd-3)))

(define-public ghc-netlink
  (package
    (name "ghc-netlink")
    (version "1.1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/netlink/netlink-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1q8sxycv93sap6dgbw70scklnpjj5vav6qlvsxm5500jlvb3jnf0"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-cereal" ,ghc-cereal)
        ("ghc-monad-loops" ,ghc-monad-loops)
        ("ghc-pretty-hex" ,ghc-pretty-hex)
        ("ghc-language-c" ,ghc-language-c)
        ("ghc-regex-pcre" ,ghc-regex-pcre-builtin)))
    (home-page "https://github.com/Ongy/netlink-hs")
    (synopsis "Netlink communication for Haskell")
    (description
      "Library to comminicate with Linux kernel via Netlink")
    (license license:bsd-3)))

(define-public ghc-statvfs
  (package
    (name "ghc-statvfs")
    (version "0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/statvfs/statvfs-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "16z9fddgvf5sl7zy7p74fng9lkdw5m9i5np3q4s2h8jdi43mwmg1"))))
    (build-system haskell-build-system)
    (home-page
      "http://hackage.haskell.org/package/statvfs")
    (synopsis
      "Get unix filesystem statistics with statfs, statvfs")
    (description
      "C FFI wrapper for POSIX statvfs and fstatvfs calls.")
    (license license:bsd-3)))


(define-public ghc-pulseaudio
  (package
    (name "ghc-pulseaudio")
    (version "0.0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/pulseaudio/pulseaudio-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "12xqclasgqwkwq0wx96qhd6xl98i1amchan71p0r55x5rw2ji0hv"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-stm" ,ghc-stm)
              ("pulseaudio" ,pulseaudio)))
    (home-page
      "http://hackage.haskell.org/package/pulseaudio")
    (synopsis
      "A low-level (incomplete) wrapper around the pulseaudio client asynchronous api")
    (description
      "This package mainly exists, because I wanted to query the current system volume from the pulse audio server. . Doing this required me to build at least the core of the pulse api. . This package is provided as is, with very little testing and only a small subset of the pulse api implemented. Expanding this package *should* be rather easy. At least for query/control. If you are interested in doing this, but don't know how, feel free to contact me. . . In package is a bit unrefined and rough. If you have better structure for modules feel free to reorganize them and making a PR. This also means, don't see this structur as fixed yet, it may change if someone has a better idea.")
    (license license:lgpl3)))

(define-public ghc-mtl
  (package
    (name "ghc-mtl")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/mtl/mtl-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1xmy5741h8cyy0d91ahvqdz2hykkk20l8br7lg1rccnkis5g80w8"))))
    (build-system haskell-build-system)
    (home-page "http://github.com/haskell/mtl")
    (synopsis
      "Monad classes, using functional dependencies")
    (description
      "Monad classes using functional dependencies, with instances for various monad transformers, inspired by the paper /Functional Programming with Overloading and Higher-Order Polymorphism/, by Mark P Jones, in /Advanced School of Functional Programming/, 1995 (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>).")
    (license license:bsd-3)))

(define-public ghc-monky
  (package
   (name "ghc-monky")
   (version "master")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/monky-hs/monky.git")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1p1xlfqp630iw68xzwaa0799a0jsh4jkbc522as8s0igi4s27qgd"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-old-time" ,ghc-old-time)
      ("ghc-text" ,ghc-text)
      ("ghc-network" ,ghc-network)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-stm" ,ghc-stm)
      ("ghc-statvfs" ,ghc-statvfs)
      ("ghc-netlink" ,ghc-netlink)
      ("ghc-cereal" ,ghc-cereal)
      ("ghc-formatting" ,ghc-formatting)
      ("ghc-composition" ,ghc-composition)
      ("ghc-env-locale" ,ghc-env-locale)
      ("ghc-pulseaudio" ,ghc-pulseaudio)
      ("ghc-optparse-applicative" ,ghc-optparse-applicative)))
   (propagated-inputs
    `(("linux-libre-headers" ,linux-libre-headers) ; FIXME required for errno.h
      ("gcc-toolchain" ,gcc-toolchain)     ; FIXME: required for crt1.o
      ("ghc" ,ghc))) ; required for config compilation
   (home-page
    "http://hackage.haskell.org/package/monky")
   (synopsis
    "A system state collecting library and application")
   (description
    "monky started as an alternative to conky, i3-status or similar, that's fully containing in one process. Also making an effort to keep file descriptors or handles as long as possible. monky 2.0 is the first version on hackage. . monky consists of multiple parts. A number of collection modules, output modules, \"examples\" and a helper application. . * collection modules . The collection modules are the core library. Collection modules export a handle that can be used to get some detail about the system. They can be used without the other parts of this package, but they are designed with monky in mind. . * output modules . Output modules take a monky specific output type and transform it into something that can be displayed by some external application. That may be a statusbar (dzen2), the terminal, a network port, that makes it accessible on another machine, or any other thing. . * examples . The examples are a group of modules, that use collection modules to create the output used by the output modules. The flexibility of the examples varies greatly, some may are really flexible, some are rather static. The intended usecase is for users to create their own examples and use them, if they don't want to use the provided ones. . Later on, I want to create something like xmonad-contrib or a collection of user examples, to provide better usability for users with few to no experience with haskell. . * helper application . The helper application is used to compile the actual output generator and can generate an example configuration. . To generate an example configuration in /~\\/.monky/ simply run `monky`. Then modify /~\\/.monky\\/monky.hs/ to create your own configuration. . Modules can have two types. 'PollModule' and 'EvtModule'. 'PollModule's work by the main loop asking the module to generate new output, while 'EvtModule's block until some event is received and update their output on demand. Some handles are an instance of both, 'PollModule' and 'EvtModule'. 'EvtModule' should be preferred, since they induce less load on your system. The monky main-loop does one \"tick\" every second. 'PollModules' are updated each /N/ ticks, where /N/ is passed to 'pollPack'.")
   (license license:lgpl3)))


(define-public dbusmenu
  ;; TODO TODO TODO TODO needs https://developer.gnome.org/gobject/stable/glib-mkenums.html
  (package
   (name "dbusmenu")
   (version "16.04.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://launchpad.net/libdbusmenu/16.04/" version "/+download/libdbusmenu-" version ".tar.gz"))
            (sha256
             (base32
              "12l7z8dhl917iy9h02sxmpclnhkdjryn08r8i4sr8l3lrlm4mk5r"))))
   (build-system gnu-build-system)
   (native-inputs `(("pkg-config" ,pkg-config)
                    ("intltool" ,intltool)
                    ("python" ,python)))
   (inputs `(("glib" ,glib)
             ("gtk+" ,gtk+)
             ("gtk+-2" ,gtk+-2)
             ("json-glib" ,json-glib)
             ("gobject-introspection" ,gobject-introspection)
             ("atk" ,atk)))

   (home-page
    "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
   (synopsis "Hardware sensors plugin for Xfce4")
   (description
    "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
   ;; The main plugin code is covered by gpl2+, but the files containing code
   ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
   (license (list license:gpl2+ license:lgpl2.0+))))


(define-public ghc-gi-dbusmenu
  (package
   (name "ghc-gi-dbusmenu")
   (version "0.4.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/gi-dbusmenu/gi-dbusmenu-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0fi07jf6bsrxsk101ffpyv17lirjgyx4afz26lhbpkqadnpc3kp4"))))
   (build-system haskell-build-system)
   (arguments
    '(#:phases (modify-phases
                %standard-phases
                (replace 'configure
                         (lambda* (#:key outputs inputs tests? (configure-flags '())
                                   #:allow-other-keys)
                           (let* ((out (assoc-ref outputs "out"))
                                  (doc (assoc-ref outputs "doc"))
                                  (lib (assoc-ref outputs "lib"))
                                  (bin (assoc-ref outputs "bin"))
                                  (run-setuphs (lambda (command params)
                                                 (let ((setup-file (cond
                                                                    ((file-exists? "Setup.hs")
                                                                     "Setup.hs")
                                                                    ((file-exists? "Setup.lhs")
                                                                     "Setup.lhs")
                                                                    (else
                                                                     #f))))
                                                   (if setup-file
                                                       (begin
                                                         (format #t "running \"runhaskell Setup.hs\" with command ~s \
and parameters ~s~%"
                                                                 command params)
                                                         ;; on following line i;ve added package-db as an arg for runhaskell and not Setup.hs, cause package-db is required for Setup.hs compilation, this should be maybe present in haskell build system?
                                                         (apply invoke "runhaskell" "-v" "-package-db=../package.conf.d" setup-file command params))
                                                       (error "no Setup.hs nor Setup.lhs found")))))

                                  (tmp-db-dir (string-append (or (getenv "TMP") "/tmp")
                                                             "/package.conf.d"))

                                  (name-version (strip-store-file-name out))
                                  (input-dirs (map cdr inputs))
                                  (gir-dir (string-append (assoc-ref inputs "gobject-introspection")
                                                          "/share/gir-1.0"))
                                  (ghc-path (getenv "GHC_PACKAGE_PATH"))
                                  (params (append `(,(string-append "--prefix=" out))
                                                  `(,(string-append "--libdir=" (or lib out) "/lib"))
                                                  `(,(string-append "--bindir=" (or bin out) "/bin"))
                                                  `(,(string-append
                                                      "--docdir=" (or doc out)
                                                      "/share/doc/" name-version))
                                                  '("--libsubdir=$compiler/$pkg-$version")
                                                  `(,(string-append "--package-db=" tmp-db-dir))
                                                  '("--global")
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-include-dirs=" x))
                                                       (search-path-as-list '("include") input-dirs)))
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-lib-dirs=" x))
                                                       (search-path-as-list '("lib") input-dirs)))
                                                  (if tests?
                                                      '("--enable-tests")
                                                      '())
                                                  configure-flags)))
                             ;; Cabal errors if GHC_PACKAGE_PATH is set during 'configure', so unset
                             ;; and restore it.
                             (unsetenv "GHC_PACKAGE_PATH")

                             ;; For packages where the Cabal build-type is set to "Configure",
                             ;; ./configure will be executed.  In these cases, the following
                             ;; environment variable is needed to be able to find the shell executable.
                             ;; For other package types, the configure script isn't present.  For more
                             ;; information, see the Build Information section of
                             ;; <https://www.haskell.org/cabal/users-guide/developing-packages.html>.
                             (when (file-exists? "configure")
                               (setenv "CONFIG_SHELL" "sh"))
                             (setenv "HASKELL_GI_GIR_SEARCH_PATH" gir-dir)
                             (setenv "LD_LIBRARY_PATH" (getenv "LIBRARY_PATH")) ;; for libcairo-gobject.so lookup
                             (run-setuphs "configure" params)

                             (setenv "GHC_PACKAGE_PATH" ghc-path)
                             #t))))))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs
    `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
      ("ghc-haskell-gi" ,ghc-haskell-gi)
      ("ghc-haskell-gi-overloading"
       ,ghc-haskell-gi-overloading)
      ("ghc-gi-glib" ,ghc-gi-glib)
      ("gobject-introspection" ,gobject-introspection)
      ("ghc-gi-gobject" ,ghc-gi-gobject)
      ("ghc-text" ,ghc-text)))
   (home-page
    "https://github.com/haskell-gi/haskell-gi")
   (synopsis "Dbusmenu bindings")
   (description
    "Bindings for libdbusmenu, autogenerated by haskell-gi.")
   (license license:lgpl2.1)))

(define-public ghc-gi-dbusmenugtk3
  (package
    (name "ghc-gi-dbusmenugtk3")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gi-dbusmenugtk3/gi-dbusmenugtk3-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0wflxyicav2p1z2sqdrjpvkf4blsilg7psvbr0cfl0r7vmy6nx4w"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-haskell-gi" ,ghc-haskell-gi)
        ("ghc-haskell-gi-overloading"
         ,ghc-haskell-gi-overloading)
        ("ghc-gi-glib" ,ghc-gi-glib)
        ("ghc-gi-gobject" ,ghc-gi-gobject)
        ("ghc-gi-gtk" ,ghc-gi-gtk)
        ("ghc-gi-dbusmenu" ,ghc-gi-dbusmenu)
        ("ghc-gi-atk" ,ghc-gi-atk)
        ("ghc-gi-gdk" ,ghc-gi-gdk)
        ("ghc-gi-gdkpixbuf" ,ghc-gi-gdkpixbuf)
        ("ghc-text" ,ghc-text)))
    (home-page
      "https://github.com/haskell-gi/haskell-gi")
    (synopsis "DbusmenuGtk bindings")
    (description
      "Bindings for libdbusgtk3, autogenerated by haskell-gi.")
    (license license:lgpl2.1)))

(define-public ghc-gi-gtk-hs
  (package
    (name "ghc-gi-gtk-hs")
    (version "0.3.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gi-gtk-hs/gi-gtk-hs-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0xnrssnfaz57akrkgpf1cm3d4lg3cmlh0b8yp6w9pdsbp0lld2ay"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-base-compat" ,ghc-base-compat)
        ("ghc-mtl" ,ghc-mtl)
        ("ghc-text" ,ghc-text)
        ("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-gi-glib" ,ghc-gi-glib)
        ("ghc-gi-gobject" ,ghc-gi-gobject)
        ("ghc-gi-gdk" ,ghc-gi-gdk)
        ("ghc-gi-gtk" ,ghc-gi-gtk)
        ("ghc-gi-gdkpixbuf" ,ghc-gi-gdkpixbuf)))
    (home-page
      "https://github.com/haskell-gi/gi-gtk-hs")
    (synopsis
      "A wrapper for gi-gtk, adding a few more idiomatic API parts on top")
    (description
      "A wrapper for gi-gtk, adding a few more idiomatic API parts on top")
    (license license:lgpl2.1)))

(define-public ghc-gi-gio
  (package
    (name "ghc-gi-gio")
    (version "2.0.19")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gi-gio/gi-gio-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1xyg1hmxp408npri8ydm5iaphfwdq7jdgdhbwgbxiyia2ymxfhqc"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-haskell-gi" ,ghc-haskell-gi)
        ("ghc-haskell-gi-overloading"
         ,ghc-haskell-gi-overloading)
        ("ghc-gi-gobject" ,ghc-gi-gobject)
        ("ghc-gi-glib" ,ghc-gi-glib)
        ("ghc-text" ,ghc-text)))
    (home-page
      "https://github.com/haskell-gi/haskell-gi")
    (synopsis "Gio bindings")
    (description
      "Bindings for Gio, autogenerated by haskell-gi.")
    (license license:lgpl2.1)))

(define-public ghc-gi-xlib
  (package
   (name "ghc-gi-xlib")
   (version "2.0.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/gi-xlib/gi-xlib-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0w9dwnd7a9hh1qn3swa48i8hp4gx9kznc92zjf198lrmrbkamp22"))))
   (build-system haskell-build-system)
   (arguments
    '(#:phases (modify-phases
                %standard-phases
                (replace 'configure
                         (lambda* (#:key outputs inputs tests? (configure-flags '())
                                   #:allow-other-keys)
                           (let* ((out (assoc-ref outputs "out"))
                                  (doc (assoc-ref outputs "doc"))
                                  (lib (assoc-ref outputs "lib"))
                                  (bin (assoc-ref outputs "bin"))
                                  (run-setuphs (lambda (command params)
                                                 (let ((setup-file (cond
                                                                    ((file-exists? "Setup.hs")
                                                                     "Setup.hs")
                                                                    ((file-exists? "Setup.lhs")
                                                                     "Setup.lhs")
                                                                    (else
                                                                     #f))))
                                                   (if setup-file
                                                       (begin
                                                         (format #t "running \"runhaskell Setup.hs\" with command ~s \
and parameters ~s~%"
                                                                 command params)
                                                         ;; on following line i;ve added package-db as an arg for runhaskell and not Setup.hs, cause package-db is required for Setup.hs compilation, this should be maybe present in haskell build system?
                                                         (apply invoke "runhaskell" "-v" "-package-db=../package.conf.d" setup-file command params))
                                                       (error "no Setup.hs nor Setup.lhs found")))))

                                  (tmp-db-dir (string-append (or (getenv "TMP") "/tmp")
                                                             "/package.conf.d"))

                                  (name-version (strip-store-file-name out))
                                  (input-dirs (map cdr inputs))
                                  (gir-dir (string-append (assoc-ref inputs "gobject-introspection")
                                                          "/share/gir-1.0"))
                                  (ghc-path (getenv "GHC_PACKAGE_PATH"))
                                  (params (append `(,(string-append "--prefix=" out))
                                                  `(,(string-append "--libdir=" (or lib out) "/lib"))
                                                  `(,(string-append "--bindir=" (or bin out) "/bin"))
                                                  `(,(string-append
                                                      "--docdir=" (or doc out)
                                                      "/share/doc/" name-version))
                                                  '("--libsubdir=$compiler/$pkg-$version")
                                                  `(,(string-append "--package-db=" tmp-db-dir))
                                                  '("--global")
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-include-dirs=" x))
                                                       (search-path-as-list '("include") input-dirs)))
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-lib-dirs=" x))
                                                       (search-path-as-list '("lib") input-dirs)))
                                                  (if tests?
                                                      '("--enable-tests")
                                                      '())
                                                  configure-flags)))
                             ;; Cabal errors if GHC_PACKAGE_PATH is set during 'configure', so unset
                             ;; and restore it.
                             (unsetenv "GHC_PACKAGE_PATH")

                             ;; For packages where the Cabal build-type is set to "Configure",
                             ;; ./configure will be executed.  In these cases, the following
                             ;; environment variable is needed to be able to find the shell executable.
                             ;; For other package types, the configure script isn't present.  For more
                             ;; information, see the Build Information section of
                             ;; <https://www.haskell.org/cabal/users-guide/developing-packages.html>.
                             (when (file-exists? "configure")
                               (setenv "CONFIG_SHELL" "sh"))
                             (setenv "HASKELL_GI_GIR_SEARCH_PATH" gir-dir)
                             (setenv "LD_LIBRARY_PATH" (getenv "LIBRARY_PATH")) ;; for libcairo-gobject.so lookup
                             (run-setuphs "configure" params)

                             (setenv "GHC_PACKAGE_PATH" ghc-path)
                             #t))))))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs
    `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
      ("ghc-haskell-gi" ,ghc-haskell-gi)
      ("gobject-introspection" ,gobject-introspection)
      ("ghc-haskell-gi-overloading"
       ,ghc-haskell-gi-overloading)
      ("ghc-text" ,ghc-text)))
   (home-page
    "https://github.com/haskell-gi/haskell-gi")
   (synopsis "xlib bindings")
   (description
    "Bindings for xlib, autogenerated by haskell-gi.")
   (license license:lgpl2.1)))

(define-public ghc-gtk-sni-tray
  (package
    (name "ghc-gtk-sni-tray")
    (version "0.1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gtk-sni-tray/gtk-sni-tray-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0i8k6jk6jq97cahlgbj8acqdqw4zkh0cyy8i6clznbknl02qqp2i"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-dbus" ,ghc-dbus)
        ("ghc-enclosed-exceptions"
         ,ghc-enclosed-exceptions)
        ("ghc-gi-cairo" ,ghc-gi-cairo)
        ("ghc-gi-cairo-connector"
         ,ghc-gi-cairo-connector)
        ("ghc-gi-cairo-render" ,ghc-gi-cairo-render)
        ("ghc-gi-dbusmenugtk3" ,ghc-gi-dbusmenugtk3)
        ("ghc-gi-gdk" ,ghc-gi-gdk)
        ("ghc-gi-gdkpixbuf" ,ghc-gi-gdkpixbuf)
        ("ghc-gi-glib" ,ghc-gi-glib)
        ("ghc-gi-gtk" ,ghc-gi-gtk)
        ("ghc-gtk-strut" ,ghc-gtk-strut)
        ("ghc-haskell-gi" ,ghc-haskell-gi)
        ("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-hslogger" ,ghc-hslogger)
        ("ghc-status-notifier-item"
         ,ghc-status-notifier-item)
        ("ghc-text" ,ghc-text)
        ("ghc-transformers-base" ,ghc-transformers-base)
        ("ghc-dbus-hslogger" ,ghc-dbus-hslogger)
        ("ghc-optparse-applicative"
         ,ghc-optparse-applicative)))
    (home-page
      "https://github.com/IvanMalison/gtk-sni-tray#readme")
    (synopsis
      "A standalone StatusNotifierItem/AppIndicator tray")
    (description
      "Please see the README on Github at <https://github.com/IvanMalison/gtk-sni-tray#readme>")
    (license license:bsd-3)))

(define-public ghc-gi-pango
  (package
   (name "ghc-gi-pango")
   (version "1.0.16")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/gi-pango/gi-pango-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1x3q1q4ww1v6v42p1wcaghxsja8cigqaqvklkfg4gxyp2f2cdg57"))))
   (build-system haskell-build-system)
   (arguments
    '(#:phases (modify-phases
                %standard-phases
                (replace 'configure
                         (lambda* (#:key outputs inputs tests? (configure-flags '())
                                   #:allow-other-keys)
                           (let* ((out (assoc-ref outputs "out"))
                                  (doc (assoc-ref outputs "doc"))
                                  (lib (assoc-ref outputs "lib"))
                                  (bin (assoc-ref outputs "bin"))
                                  (run-setuphs (lambda (command params)
                                                 (let ((setup-file (cond
                                                                    ((file-exists? "Setup.hs")
                                                                     "Setup.hs")
                                                                    ((file-exists? "Setup.lhs")
                                                                     "Setup.lhs")
                                                                    (else
                                                                     #f))))
                                                   (if setup-file
                                                       (begin
                                                         (format #t "running \"runhaskell Setup.hs\" with command ~s \
and parameters ~s~%"
                                                                 command params)
                                                         ;; on following line i;ve added package-db as an arg for runhaskell and not Setup.hs, cause package-db is required for Setup.hs compilation, this should be maybe present in haskell build system?
                                                         (apply invoke "runhaskell" "-v" "-package-db=../package.conf.d" setup-file command params))
                                                       (error "no Setup.hs nor Setup.lhs found")))))

                                  (tmp-db-dir (string-append (or (getenv "TMP") "/tmp")
                                                             "/package.conf.d"))

                                  (name-version (strip-store-file-name out))
                                  (input-dirs (map cdr inputs))
                                  (gir-dir (string-append (assoc-ref inputs "gobject-introspection")
                                                          "/share/gir-1.0"))
                                  (ghc-path (getenv "GHC_PACKAGE_PATH"))
                                  (params (append `(,(string-append "--prefix=" out))
                                                  `(,(string-append "--libdir=" (or lib out) "/lib"))
                                                  `(,(string-append "--bindir=" (or bin out) "/bin"))
                                                  `(,(string-append
                                                      "--docdir=" (or doc out)
                                                      "/share/doc/" name-version))
                                                  '("--libsubdir=$compiler/$pkg-$version")
                                                  `(,(string-append "--package-db=" tmp-db-dir))
                                                  '("--global")
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-include-dirs=" x))
                                                       (search-path-as-list '("include") input-dirs)))
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-lib-dirs=" x))
                                                       (search-path-as-list '("lib") input-dirs)))
                                                  (if tests?
                                                      '("--enable-tests")
                                                      '())
                                                  configure-flags)))
                             ;; Cabal errors if GHC_PACKAGE_PATH is set during 'configure', so unset
                             ;; and restore it.
                             (unsetenv "GHC_PACKAGE_PATH")

                             ;; For packages where the Cabal build-type is set to "Configure",
                             ;; ./configure will be executed.  In these cases, the following
                             ;; environment variable is needed to be able to find the shell executable.
                             ;; For other package types, the configure script isn't present.  For more
                             ;; information, see the Build Information section of
                             ;; <https://www.haskell.org/cabal/users-guide/developing-packages.html>.
                             (when (file-exists? "configure")
                               (setenv "CONFIG_SHELL" "sh"))
                             (setenv "HASKELL_GI_GIR_SEARCH_PATH" gir-dir)
                             (setenv "LD_LIBRARY_PATH" (getenv "LIBRARY_PATH")) ;; for libcairo-gobject.so lookup
                             (run-setuphs "configure" params)

                             (setenv "GHC_PACKAGE_PATH" ghc-path)
                             #t))))))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs
    `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
      ("ghc-haskell-gi" ,ghc-haskell-gi)
      ("ghc-haskell-gi-overloading"
       ,ghc-haskell-gi-overloading)
      ("ghc-gi-gobject" ,ghc-gi-gobject)
      ("ghc-gi-glib" ,ghc-gi-glib)
      ("pango" ,pango)
      ("gobject-introspection" ,gobject-introspection)
      ("ghc-text" ,ghc-text)))
   (home-page
    "https://github.com/haskell-gi/haskell-gi")
   (synopsis "Pango bindings")
   (description
    "Bindings for Pango, autogenerated by haskell-gi.")
   (license license:lgpl2.1)))

(define-public ghc-gi-atk
  (package
   (name "ghc-gi-atk")
   (version "2.0.15")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/gi-atk/gi-atk-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1vmzby12nvbrka6f44pr1pjwccl0p6s984pxvibajzp72x2knxc9"))))
   (build-system haskell-build-system)
   (arguments
    '(#:phases (modify-phases
                %standard-phases
                (replace 'configure
                         (lambda* (#:key outputs inputs tests? (configure-flags '())
                                   #:allow-other-keys)
                           (let* ((out (assoc-ref outputs "out"))
                                  (doc (assoc-ref outputs "doc"))
                                  (lib (assoc-ref outputs "lib"))
                                  (bin (assoc-ref outputs "bin"))
                                  (run-setuphs (lambda (command params)
                                                 (let ((setup-file (cond
                                                                    ((file-exists? "Setup.hs")
                                                                     "Setup.hs")
                                                                    ((file-exists? "Setup.lhs")
                                                                     "Setup.lhs")
                                                                    (else
                                                                     #f))))
                                                   (if setup-file
                                                       (begin
                                                         (format #t "running \"runhaskell Setup.hs\" with command ~s \
and parameters ~s~%"
                                                                 command params)
                                                         ;; on following line i;ve added package-db as an arg for runhaskell and not Setup.hs, cause package-db is required for Setup.hs compilation, this should be maybe present in haskell build system?
                                                         (apply invoke "runhaskell" "-v" "-package-db=../package.conf.d" setup-file command params))
                                                       (error "no Setup.hs nor Setup.lhs found")))))

                                  (tmp-db-dir (string-append (or (getenv "TMP") "/tmp")
                                                             "/package.conf.d"))

                                  (name-version (strip-store-file-name out))
                                  (input-dirs (map cdr inputs))
                                  (gir-dir (string-append (assoc-ref inputs "gobject-introspection")
                                                          "/share/gir-1.0"))
                                  (ghc-path (getenv "GHC_PACKAGE_PATH"))
                                  (params (append `(,(string-append "--prefix=" out))
                                                  `(,(string-append "--libdir=" (or lib out) "/lib"))
                                                  `(,(string-append "--bindir=" (or bin out) "/bin"))
                                                  `(,(string-append
                                                      "--docdir=" (or doc out)
                                                      "/share/doc/" name-version))
                                                  '("--libsubdir=$compiler/$pkg-$version")
                                                  `(,(string-append "--package-db=" tmp-db-dir))
                                                  '("--global")
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-include-dirs=" x))
                                                       (search-path-as-list '("include") input-dirs)))
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-lib-dirs=" x))
                                                       (search-path-as-list '("lib") input-dirs)))
                                                  (if tests?
                                                      '("--enable-tests")
                                                      '())
                                                  configure-flags)))
                             ;; Cabal errors if GHC_PACKAGE_PATH is set during 'configure', so unset
                             ;; and restore it.
                             (unsetenv "GHC_PACKAGE_PATH")

                             ;; For packages where the Cabal build-type is set to "Configure",
                             ;; ./configure will be executed.  In these cases, the following
                             ;; environment variable is needed to be able to find the shell executable.
                             ;; For other package types, the configure script isn't present.  For more
                             ;; information, see the Build Information section of
                             ;; <https://www.haskell.org/cabal/users-guide/developing-packages.html>.
                             (when (file-exists? "configure")
                               (setenv "CONFIG_SHELL" "sh"))
                             (setenv "HASKELL_GI_GIR_SEARCH_PATH" gir-dir)
                             (setenv "LD_LIBRARY_PATH" (getenv "LIBRARY_PATH")) ;; for libcairo-gobject.so lookup
                             (run-setuphs "configure" params)

                             (setenv "GHC_PACKAGE_PATH" ghc-path)
                             #t))))))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs
    `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
      ("ghc-haskell-gi" ,ghc-haskell-gi)
      ("ghc-haskell-gi-overloading"
       ,ghc-haskell-gi-overloading)
      ("ghc-gi-gobject" ,ghc-gi-gobject)
      ("gobject-introspection" ,gobject-introspection)
      ("ghc-gi-glib" ,ghc-gi-glib)
      ("atk" ,atk)
      ("ghc-text" ,ghc-text)))
   (home-page
    "https://github.com/haskell-gi/haskell-gi")
   (synopsis "Atk bindings")
   (description
    "Bindings for Atk, autogenerated by haskell-gi.")
   (license license:lgpl2.1)))

(define-public ghc-gi-gdkx11
  (package
    (name "ghc-gi-gdkx11")
    (version "3.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gi-gdkx11/gi-gdkx11-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0pm1jnmal4vy53icndzzs76vcvxzn3lm31dfwg6nb6fnch5p4036"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-haskell-gi" ,ghc-haskell-gi)
        ("ghc-haskell-gi-overloading"
         ,ghc-haskell-gi-overloading)
        ("ghc-gi-gobject" ,ghc-gi-gobject)
        ("ghc-gi-gdk" ,ghc-gi-gdk)
        ("ghc-gi-gio" ,ghc-gi-gio)
        ("ghc-gi-cairo" ,ghc-gi-cairo)
        ("ghc-gi-xlib" ,ghc-gi-xlib)
        ("ghc-text" ,ghc-text)))
    (home-page
      "https://github.com/haskell-gi/haskell-gi")
    (synopsis "GdkX11 bindings")
    (description
      "Bindings for GdkX11, autogenerated by haskell-gi.")
    (license license:lgpl2.1)))

(define-public ghc-gi-gtk
  (package
    (name "ghc-gi-gtk")
    (version "3.0.27")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gi-gtk/gi-gtk-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1i8xrq56lp8ha87zykr3hgp13yp8amsxal320mknr2s29x6iw1kr"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-haskell-gi" ,ghc-haskell-gi)
        ("ghc-haskell-gi-overloading"
         ,ghc-haskell-gi-overloading)
        ("ghc-gi-cairo" ,ghc-gi-cairo)
        ("ghc-gi-pango" ,ghc-gi-pango)
        ("ghc-gi-gio" ,ghc-gi-gio)
        ("ghc-gi-gdkpixbuf" ,ghc-gi-gdkpixbuf)
        ("ghc-gi-gdk" ,ghc-gi-gdk)
        ("ghc-gi-gobject" ,ghc-gi-gobject)
        ("ghc-gi-glib" ,ghc-gi-glib)
        ("ghc-gi-atk" ,ghc-gi-atk)
        ("ghc-text" ,ghc-text)))
    (home-page
      "https://github.com/haskell-gi/haskell-gi")
    (synopsis "Gtk bindings")
    (description
      "Bindings for Gtk, autogenerated by haskell-gi.")
    (license license:lgpl2.1)))

(define-public ghc-gi-gdkpixbuf
  (package
    (name "ghc-gi-gdkpixbuf")
    (version "2.0.18")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gi-gdkpixbuf/gi-gdkpixbuf-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1b9ypv07siyr9gry471skc3qlaiwqf055ywz8nib5x39vs6rfcpj"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-haskell-gi" ,ghc-haskell-gi)
        ("ghc-haskell-gi-overloading"
         ,ghc-haskell-gi-overloading)
        ("ghc-gi-gio" ,ghc-gi-gio)
        ("ghc-gi-gobject" ,ghc-gi-gobject)
        ("ghc-gi-glib" ,ghc-gi-glib)
        ("ghc-text" ,ghc-text)))
    (home-page
      "https://github.com/haskell-gi/haskell-gi")
    (synopsis "GdkPixbuf bindings")
    (description
      "Bindings for GdkPixbuf, autogenerated by haskell-gi.")
    (license license:lgpl2.1)))

(define-public ghc-gi-gobject
  (package
   (name "ghc-gi-gobject")
   (version "2.0.16")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/gi-gobject/gi-gobject-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1bgn4ywx94py0v213iv7mbjjvvy3y7gvpgw4wpn38s2np7al8y65"))))
   (build-system haskell-build-system)
   (arguments
    '(#:phases (modify-phases
                %standard-phases
                (replace 'configure
                         (lambda* (#:key outputs inputs tests? (configure-flags '())
                                   #:allow-other-keys)
                           (let* ((out (assoc-ref outputs "out"))
                                  (doc (assoc-ref outputs "doc"))
                                  (lib (assoc-ref outputs "lib"))
                                  (bin (assoc-ref outputs "bin"))
                                  (run-setuphs (lambda (command params)
                                                 (let ((setup-file (cond
                                                                    ((file-exists? "Setup.hs")
                                                                     "Setup.hs")
                                                                    ((file-exists? "Setup.lhs")
                                                                     "Setup.lhs")
                                                                    (else
                                                                     #f))))
                                                   (if setup-file
                                                       (begin
                                                         (format #t "running \"runhaskell Setup.hs\" with command ~s \
and parameters ~s~%"
                                                                 command params)
                                                         ;; on following line i;ve added package-db as an arg for runhaskell and not Setup.hs, cause package-db is required for Setup.hs compilation, this should be maybe present in haskell build system?
                                                         (apply invoke "runhaskell" "-v" "-package-db=../package.conf.d" setup-file command params))
                                                       (error "no Setup.hs nor Setup.lhs found")))))

                                  (tmp-db-dir (string-append (or (getenv "TMP") "/tmp")
                                                             "/package.conf.d"))

                                  (name-version (strip-store-file-name out))
                                  (input-dirs (map cdr inputs))
                                  (gir-dir (string-append (assoc-ref inputs "gobject-introspection")
                                                          "/share/gir-1.0"))
                                  (ghc-path (getenv "GHC_PACKAGE_PATH"))
                                  (params (append `(,(string-append "--prefix=" out))
                                                  `(,(string-append "--libdir=" (or lib out) "/lib"))
                                                  `(,(string-append "--bindir=" (or bin out) "/bin"))
                                                  `(,(string-append
                                                      "--docdir=" (or doc out)
                                                      "/share/doc/" name-version))
                                                  '("--libsubdir=$compiler/$pkg-$version")
                                                  `(,(string-append "--package-db=" tmp-db-dir))
                                                  '("--global")
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-include-dirs=" x))
                                                       (search-path-as-list '("include") input-dirs)))
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-lib-dirs=" x))
                                                       (search-path-as-list '("lib") input-dirs)))
                                                  (if tests?
                                                      '("--enable-tests")
                                                      '())
                                                  configure-flags)))
                             ;; Cabal errors if GHC_PACKAGE_PATH is set during 'configure', so unset
                             ;; and restore it.
                             (unsetenv "GHC_PACKAGE_PATH")

                             ;; For packages where the Cabal build-type is set to "Configure",
                             ;; ./configure will be executed.  In these cases, the following
                             ;; environment variable is needed to be able to find the shell executable.
                             ;; For other package types, the configure script isn't present.  For more
                             ;; information, see the Build Information section of
                             ;; <https://www.haskell.org/cabal/users-guide/developing-packages.html>.
                             (when (file-exists? "configure")
                               (setenv "CONFIG_SHELL" "sh"))
                             (setenv "HASKELL_GI_GIR_SEARCH_PATH" gir-dir)
                             (setenv "LD_LIBRARY_PATH" (getenv "LIBRARY_PATH")) ;; for libcairo-gobject.so lookup
                             (run-setuphs "configure" params)

                             (setenv "GHC_PACKAGE_PATH" ghc-path)
                             #t))))))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs
    `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
      ("ghc-haskell-gi" ,ghc-haskell-gi)
      ("ghc-haskell-gi-overloading"
       ,ghc-haskell-gi-overloading)
      ("ghc-gi-glib" ,ghc-gi-glib)
      ("gobject-introspection" ,gobject-introspection)
      ("glib" ,glib)
      ("ghc-text" ,ghc-text)))
   (home-page
    "https://github.com/haskell-gi/haskell-gi")
   (synopsis "GObject bindings")
   (description
    "Bindings for GObject, autogenerated by haskell-gi.")
   (license license:lgpl2.1)))

(define-public ghc-gi-glib
  (package
   (name "ghc-gi-glib")
   (version "2.0.17")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/gi-glib/gi-glib-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0rxbkrwlwnjf46z0qpw0vjw1nv9kl91xp7k2098rqs36kl5bwylx"))))
   (build-system haskell-build-system)
   (arguments
    '(#:phases (modify-phases
                %standard-phases
                (replace 'configure
                         (lambda* (#:key outputs inputs tests? (configure-flags '())
                                   #:allow-other-keys)
                           (let* ((out (assoc-ref outputs "out"))
                                  (doc (assoc-ref outputs "doc"))
                                  (lib (assoc-ref outputs "lib"))
                                  (bin (assoc-ref outputs "bin"))
                                  (run-setuphs (lambda (command params)
                                                 (let ((setup-file (cond
                                                                    ((file-exists? "Setup.hs")
                                                                     "Setup.hs")
                                                                    ((file-exists? "Setup.lhs")
                                                                     "Setup.lhs")
                                                                    (else
                                                                     #f))))
                                                   (if setup-file
                                                       (begin
                                                         (format #t "running \"runhaskell Setup.hs\" with command ~s \
and parameters ~s~%"
                                                                 command params)
                                                         ;; on following line i;ve added package-db as an arg for runhaskell and not Setup.hs, cause package-db is required for Setup.hs compilation, this should be maybe present in haskell build system?
                                                         (apply invoke "runhaskell" "-v" "-package-db=../package.conf.d" setup-file command params))
                                                       (error "no Setup.hs nor Setup.lhs found")))))

                                  (tmp-db-dir (string-append (or (getenv "TMP") "/tmp")
                                                             "/package.conf.d"))

                                  (name-version (strip-store-file-name out))
                                  (input-dirs (map cdr inputs))
                                  (gir-dir (string-append (assoc-ref inputs "gobject-introspection")
                                                          "/share/gir-1.0"))
                                  (ghc-path (getenv "GHC_PACKAGE_PATH"))
                                  (params (append `(,(string-append "--prefix=" out))
                                                  `(,(string-append "--libdir=" (or lib out) "/lib"))
                                                  `(,(string-append "--bindir=" (or bin out) "/bin"))
                                                  `(,(string-append
                                                      "--docdir=" (or doc out)
                                                      "/share/doc/" name-version))
                                                  '("--libsubdir=$compiler/$pkg-$version")
                                                  `(,(string-append "--package-db=" tmp-db-dir))
                                                  '("--global")
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-include-dirs=" x))
                                                       (search-path-as-list '("include") input-dirs)))
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-lib-dirs=" x))
                                                       (search-path-as-list '("lib") input-dirs)))
                                                  (if tests?
                                                      '("--enable-tests")
                                                      '())
                                                  configure-flags)))
                             ;; Cabal errors if GHC_PACKAGE_PATH is set during 'configure', so unset
                             ;; and restore it.
                             (unsetenv "GHC_PACKAGE_PATH")

                             ;; For packages where the Cabal build-type is set to "Configure",
                             ;; ./configure will be executed.  In these cases, the following
                             ;; environment variable is needed to be able to find the shell executable.
                             ;; For other package types, the configure script isn't present.  For more
                             ;; information, see the Build Information section of
                             ;; <https://www.haskell.org/cabal/users-guide/developing-packages.html>.
                             (when (file-exists? "configure")
                               (setenv "CONFIG_SHELL" "sh"))
                             (setenv "HASKELL_GI_GIR_SEARCH_PATH" gir-dir)
                             (setenv "LD_LIBRARY_PATH" (getenv "LIBRARY_PATH")) ;; for libcairo-gobject.so lookup
                             (run-setuphs "configure" params)

                             (setenv "GHC_PACKAGE_PATH" ghc-path)
                             #t))))))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs
    `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
      ("ghc-haskell-gi" ,ghc-haskell-gi)
      ("gobject-introspection" ,gobject-introspection)
      ("ghc-haskell-gi-overloading"
       ,ghc-haskell-gi-overloading)
      ("ghc-text" ,ghc-text)
      ("glib" ,glib)))
   (home-page
    "https://github.com/haskell-gi/haskell-gi")
   (synopsis "GLib bindings")
   (description
    "Bindings for GLib, autogenerated by haskell-gi.")
   (license license:lgpl2.1)))

(define-public ghc-gtk-strut
  (package
    (name "ghc-gtk-strut")
    (version "0.1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gtk-strut/gtk-strut-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "19p3w1zvnaazfd01yy4cl00sl53xc7kqgqhsw7l3psadmwk6x4w1"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-gi-gdk" ,ghc-gi-gdk)
        ("ghc-gi-gtk" ,ghc-gi-gtk)
        ("ghc-text" ,ghc-text)))
    (home-page
      "https://github.com/IvanMalison/gtk-strut#readme")
    (synopsis
      "Libary for creating strut windows with gi-gtk")
    (description
      "Please see the README on Github at <https://github.com/IvanMalison/gtk-strut#readme>")
    (license license:bsd-3)))

(define-public ghc-multimap
  (package
    (name "ghc-multimap")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/multimap/multimap-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0d3l5q4yvmywl6i9ip96zz0fvhjdh00mfbbniphbjxsi8wlwack3"))))
    (build-system haskell-build-system)
    (home-page
      "http://hub.darcs.net/scravy/multimap")
    (synopsis "A multimap.")
    (description
      "This is a simple implementation of a multimap, based on \"Data.Map\". . [@v1.1@] @!@ had its arguments flipped. Fixed. Also added @fromMap@. . [@v1.2@] Added \"Data.SetMap\", renamed @Multimap@ to \"Data.MultiMap\". Fixed the type of @delete@. Derive instances for @Data@ and @Typeable@. . [@v1.2.1@] Fixed typos in the documentation.")
    (license license:expat)))

(define-public ghc-rate-limit
  (package
    (name "ghc-rate-limit")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/rate-limit/rate-limit-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0gm5jmi779niqsbgmkqqx6dsfw6yvfp4wlfibg9fzzmcc4i968g2"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-stm" ,ghc-stm)
        ("ghc-time-units" ,ghc-time-units)))
    (home-page "http://github.com/acw/rate-limit")
    (synopsis
      "A basic library for rate-limiting IO actions.")
    (description
      "In many cases, it is useful, necessary, or simply nice to limit how frequently you perform some action. For example, you may want to limit how often your program makes a request of some web site. This library is intended as a general-purpose mechanism for rate-limiting IO actions.")
    (license license:bsd-3)))

(define-public ghc-hspec-wai
  (package
    (name "ghc-hspec-wai")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/hspec-wai/hspec-wai-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0gr8j8x8vvzygxyqc0likam63f3427x4p73g95a387aksr5l2ph5"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-quickcheck" ,ghc-quickcheck)
        ("ghc-base-compat" ,ghc-base-compat)
        ("ghc-case-insensitive" ,ghc-case-insensitive)
        ("ghc-hspec-core" ,ghc-hspec-core)
        ("ghc-hspec-expectations"
         ,ghc-hspec-expectations)
        ("ghc-http-types" ,ghc-http-types)
        ("ghc-text" ,ghc-text)
        ("ghc-wai" ,ghc-wai)
        ("ghc-wai-extra" ,ghc-wai-extra)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)))
    (home-page
      "https://github.com/hspec/hspec-wai#readme")
    (synopsis
      "Experimental Hspec support for testing WAI applications")
    (description
      "Experimental Hspec support for testing WAI applications")
    (license license:expat)))

(define-public ghc-gi-gdk
  (package
    (name "ghc-gi-gdk")
    (version "3.0.16")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gi-gdk/gi-gdk-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0jp3d3zfm20b4ax1g5k1wzh8fxxzsw4ssw7zqx0d13167m4smc3y"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-haskell-gi" ,ghc-haskell-gi)
        ("ghc-haskell-gi-overloading"
         ,ghc-haskell-gi-overloading)
        ("ghc-gi-cairo" ,ghc-gi-cairo)
        ("ghc-gi-pango" ,ghc-gi-pango)
        ("ghc-gi-gio" ,ghc-gi-gio)
        ("ghc-gi-gdkpixbuf" ,ghc-gi-gdkpixbuf)
        ("ghc-gi-gobject" ,ghc-gi-gobject)
        ("ghc-gi-glib" ,ghc-gi-glib)
        ("ghc-text" ,ghc-text)))
    (home-page
      "https://github.com/haskell-gi/haskell-gi")
    (synopsis "Gdk bindings")
    (description
      "Bindings for Gdk, autogenerated by haskell-gi.")
    (license license:lgpl2.1)))

(define-public ghc-gi-cairo-connector
  (package
    (name "ghc-gi-cairo-connector")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gi-cairo-connector/gi-cairo-connector-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0lhaki2qjk8f6bn78sag4g38g549sjzbjbah27j2i46xj7j08png"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-mtl" ,ghc-mtl)
        ("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-gi-cairo" ,ghc-gi-cairo)
        ("ghc-gi-cairo-render" ,ghc-gi-cairo-render)))
    (home-page
      "https://github.com/cohomology/gi-cairo-render")
    (synopsis
      "GI friendly Binding to the Cairo library.")
    (description
      "This library contains glue code used to interconnect Haskell GI and Cairo")
    (license license:lgpl2.1)))

(define-public ghc-gi-cairo-render
  (package
    (name "ghc-gi-cairo-render")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gi-cairo-render/gi-cairo-render-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0arbynn7ilrc3shddff1rxcvlg6k3m617lrq4fdsqfas3amxarm4"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-utf8-string" ,ghc-utf8-string)
        ("ghc-text" ,ghc-text)
        ("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-mtl" ,ghc-mtl)
        ("ghc-c2hs" ,ghc-c2hs)))
    (home-page
      "https://github.com/cohomology/gi-cairo-render")
    (synopsis
      "GI friendly Binding to the Cairo library.")
    (description
      "Cairo is a library to render high quality vector graphics. There exist various backends that allows rendering to Gtk windows, PDF, PS, PNG and SVG documents, amongst others.")
    (license license:bsd-3)))

(define-public ghc-haskell-gi-base
  (package
    (name "ghc-haskell-gi-base")
    (version "0.21.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/haskell-gi-base/haskell-gi-base-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1pxnwljicxyxr83c7d8xvla7zbp2krv1n6fp4i2zh8bqwln3fkgh"))))
    (build-system haskell-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("ghc-text" ,ghc-text)
              ("glib" ,glib)))
    (home-page
      "https://github.com/haskell-gi/haskell-gi-base")
    (synopsis
      "Foundation for libraries generated by haskell-gi")
    (description
      "Foundation for libraries generated by haskell-gi")
    (license license:lgpl2.1)))

(define-public ghc-scotty
  (package
    (name "ghc-scotty")
    (version "0.11.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/scotty/scotty-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "13z0zmginaa1y5iywbbygvb9q3cmfgjkv6n2drs8gfbv3sirrf7i"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-aeson" ,ghc-aeson)
        ("ghc-blaze-builder" ,ghc-blaze-builder)
        ("ghc-case-insensitive" ,ghc-case-insensitive)
        ("ghc-data-default-class"
         ,ghc-data-default-class)
        ("ghc-exceptions" ,ghc-exceptions)
        ("ghc-fail" ,ghc-fail)
        ("ghc-http-types" ,ghc-http-types)
        ("ghc-monad-control" ,ghc-monad-control)
        ("ghc-mtl" ,ghc-mtl)
        ("ghc-nats" ,ghc-nats)
        ("ghc-network" ,ghc-network)
        ("ghc-regex-compat" ,ghc-regex-compat)
        ("ghc-text" ,ghc-text)
        ("ghc-transformers-base" ,ghc-transformers-base)
        ("ghc-transformers-compat"
         ,ghc-transformers-compat)
        ("ghc-wai" ,ghc-wai)
        ("ghc-wai-extra" ,ghc-wai-extra)
        ("ghc-warp" ,ghc-warp)))
    (native-inputs
      `(("ghc-async" ,ghc-async)
        ("ghc-hspec" ,ghc-hspec)
        ("ghc-hspec-wai" ,ghc-hspec-wai)
        ("ghc-lifted-base" ,ghc-lifted-base)))
    (home-page
      "https://github.com/scotty-web/scotty")
    (synopsis
      "Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp")
    (description
      "A Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp. . @ &#123;-&#35; LANGUAGE OverloadedStrings &#35;-&#125; . import Web.Scotty . import Data.Monoid (mconcat) . main = scotty 3000 $ &#32;&#32;get &#34;/:word&#34; $ do &#32;&#32;&#32;&#32;beam <- param &#34;word&#34; &#32;&#32;&#32;&#32;html $ mconcat [&#34;&#60;h1&#62;Scotty, &#34;, beam, &#34; me up!&#60;/h1&#62;&#34;] @ . . Scotty is the cheap and cheerful way to write RESTful, declarative web applications. . * A page is as simple as defining the verb, url pattern, and Text content. . * It is template-language agnostic. Anything that returns a Text value will do. . * Conforms to WAI Application interface. . * Uses very fast Warp webserver by default. . As for the name: Sinatra + Warp = Scotty. . [WAI] <http://hackage.haskell.org/package/wai> . [Warp] <http://hackage.haskell.org/package/warp>")
    (license license:bsd-3)))

(define-public ghc-spool
  (package
    (name "ghc-spool")
    (version "0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/spool/spool-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1svkz3cxkyi6f3akakjfk1cvij85xy69v52d88gh97xgiawp5346"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-vector" ,ghc-vector)))
    (home-page
      "http://hackage.haskell.org/package/spool")
    (synopsis
      "Convert between ByteString and Vector.Storable without copying")
    (description
      "This library allows conversion between the types from @Data.ByteString@ (package @bytestring@) and @Data.Vector.Storable@ (package @vector@) without copying the underlying data.  This is useful, for example, when @ByteString@ IO produces or consumes vectors of numbers in native byte order. . This trick relies on the fact that @ByteString@ and @Vector@ use their respective @ForeignPtr@s in compatible ways.  It works with @bytestring-0.9.1.10@ and @vector-0.9@ on GHC 7.0.  It may break with future releases of these packages.  Depending on this library should be seen as a way to document and standardize an existing hack, and not as an absolute guarantee of correct behavior.")
    (license license:bsd-3)))

(define-public ghc-haskell-gi
  (package
    (name "ghc-haskell-gi")
    (version "0.21.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/haskell-gi/haskell-gi-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1rvi9bmgxq7q6js8yb5yb156yxmnm9px9amgjwzxmr7sxz31dl8j"))))
    (build-system haskell-build-system)
    (arguments
     '(#:tests? #f)) ;; FIXME: reenable tests later
    (inputs
      `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-attoparsec" ,ghc-attoparsec)
        ("ghc-mtl" ,ghc-mtl)
        ("ghc-pretty-show" ,ghc-pretty-show)
        ("ghc-safe" ,ghc-safe)
        ("ghc-xdg-basedir" ,ghc-xdg-basedir)
        ("ghc-xml-conduit" ,ghc-xml-conduit)
        ("ghc-regex-tdfa" ,ghc-regex-tdfa)
        ("ghc-text" ,ghc-text)
        ("gobject-introspection" ,gobject-introspection)
        ("glib" ,glib)))
    (native-inputs `(("ghc-doctest" ,ghc-doctest)
                     ("pkg-config" ,pkg-config)))
    (home-page
      "https://github.com/haskell-gi/haskell-gi")
    (synopsis
      "Generate Haskell bindings for GObject Introspection capable libraries")
    (description
      "Generate Haskell bindings for GObject Introspection capable libraries. This includes most notably Gtk+, but many other libraries in the GObject ecosystem provide introspection data too.")
    (license license:lgpl2.1)))

(define-public ghc-haskell-gi-overloading
  (package
    (name "ghc-haskell-gi-overloading")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/haskell-gi-overloading/haskell-gi-overloading-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0ak8f79ia9zlk94zr02sq8bqi5n5pd8ria8w1dj3adcdvpw9gmry"))))
    (build-system haskell-build-system)
    (arguments
     '(#:haddock? #f))
    (home-page
      "https://github.com/haskell-gi/haskell-gi")
    (synopsis "Overloading support for haskell-gi")
    (description
      "Control overloading support in haskell-gi generated bindings")
    (license license:bsd-3)))

(define-public ghc-status-notifier-item
  (package
    (name "ghc-status-notifier-item")
    (version "0.3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/status-notifier-item/status-notifier-item-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0wrw635r7c2qdb90hpm5lg3kb16c3dkw88ypbszf18m02f4dsk8h"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-dbus" ,ghc-dbus)
        ("ghc-hslogger" ,ghc-hslogger)
        ("ghc-lens" ,ghc-lens)
        ("ghc-network" ,ghc-network)
        ("ghc-spool" ,ghc-spool)
        ("ghc-text" ,ghc-text)
        ("ghc-vector" ,ghc-vector)
        ("ghc-optparse-applicative"
         ,ghc-optparse-applicative)
        ("ghc-dbus-hslogger" ,ghc-dbus-hslogger)))
    (home-page
      "https://github.com/IvanMalison/status-notifier-item#readme")
    (synopsis
      "A wrapper over the StatusNotifierItem/libappindicator dbus specification")
    (description
      "Please see the README on Github at <https://github.com/IvanMalison/status-notifier-item#readme>")
    (license license:bsd-3)))

(define-public ghc-time-units
  (package
    (name "ghc-time-units")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/time-units/time-units-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "16g0i6r6vj9w4lbn12jqrhgbbjjca8wbzq6546dz08aks1yrk0g1"))))
    (build-system haskell-build-system)
    (home-page "http://github.com/acw/time-units")
    (synopsis
      "A basic library for defining units of time as types.")
    (description
      "In many cases, it is useful (either for error checking or documentation reasons) to define input and output types as having a particular unit of time. In addition, by creating a type class defining type units, this library should make it easier to separate the units of time the developer wants to think in versus the units of time the library author wants to think in.")
    (license license:bsd-3)))

(define-public ghc-onetuple
  (package
    (name "ghc-onetuple")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/OneTuple/OneTuple-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1p14cvjk3rgfc0xxcn7ffaajd2ii1ljnlayil2yyzgdwhlj70bnq"))))
    (build-system haskell-build-system)
    (home-page
      "http://hackage.haskell.org/package/OneTuple")
    (synopsis "Singleton Tuple")
    (description
      "This package provides a singleton tuple data type . > data OneTuple = OneTuple a . Note: it's not a @newtype@")
    (license license:bsd-3)))

(define-public ghc-gi-cairo
  (package
   (name "ghc-gi-cairo")
   (version "1.0.17")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/gi-cairo/gi-cairo-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1ax7aly9ahvb18m3zjmy0dk47qfdx5yl15q52c3wp4wa0c5aggax"))))
   (build-system haskell-build-system)
   (arguments
    '(#:phases (modify-phases
                %standard-phases
                (replace 'configure
                         (lambda* (#:key outputs inputs tests? (configure-flags '())
                                   #:allow-other-keys)
                           (let* ((out (assoc-ref outputs "out"))
                                  (doc (assoc-ref outputs "doc"))
                                  (lib (assoc-ref outputs "lib"))
                                  (bin (assoc-ref outputs "bin"))
                                  (run-setuphs (lambda (command params)
                                                 (let ((setup-file (cond
                                                                    ((file-exists? "Setup.hs")
                                                                     "Setup.hs")
                                                                    ((file-exists? "Setup.lhs")
                                                                     "Setup.lhs")
                                                                    (else
                                                                     #f))))
                                                   (if setup-file
                                                       (begin
                                                         (format #t "running \"runhaskell Setup.hs\" with command ~s \
and parameters ~s~%"
                                                                 command params)
                                                         ;; on following line i;ve added package-db as an arg for runhaskell and not Setup.hs, cause package-db is required for Setup.hs compilation, this should be maybe present in haskell build system?
                                                         (apply invoke "runhaskell" "-v" "-package-db=../package.conf.d" setup-file command params))
                                                       (error "no Setup.hs nor Setup.lhs found")))))

                                  (tmp-db-dir (string-append (or (getenv "TMP") "/tmp")
                                                             "/package.conf.d"))

                                  (name-version (strip-store-file-name out))
                                  (input-dirs (map cdr inputs))
                                  (gir-dir (string-append (assoc-ref inputs "gobject-introspection")
                                                          "/share/gir-1.0"))
                                  (ghc-path (getenv "GHC_PACKAGE_PATH"))
                                  (params (append `(,(string-append "--prefix=" out))
                                                  `(,(string-append "--libdir=" (or lib out) "/lib"))
                                                  `(,(string-append "--bindir=" (or bin out) "/bin"))
                                                  `(,(string-append
                                                      "--docdir=" (or doc out)
                                                      "/share/doc/" name-version))
                                                  '("--libsubdir=$compiler/$pkg-$version")
                                                  `(,(string-append "--package-db=" tmp-db-dir))
                                                  '("--global")
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-include-dirs=" x))
                                                       (search-path-as-list '("include") input-dirs)))
                                                  `(,@(map
                                                       (lambda (x) (string-append "--extra-lib-dirs=" x))
                                                       (search-path-as-list '("lib") input-dirs)))
                                                  (if tests?
                                                      '("--enable-tests")
                                                      '())
                                                  configure-flags)))
                             ;; Cabal errors if GHC_PACKAGE_PATH is set during 'configure', so unset
                             ;; and restore it.
                             (unsetenv "GHC_PACKAGE_PATH")

                             ;; For packages where the Cabal build-type is set to "Configure",
                             ;; ./configure will be executed.  In these cases, the following
                             ;; environment variable is needed to be able to find the shell executable.
                             ;; For other package types, the configure script isn't present.  For more
                             ;; information, see the Build Information section of
                             ;; <https://www.haskell.org/cabal/users-guide/developing-packages.html>.
                             (when (file-exists? "configure")
                               (setenv "CONFIG_SHELL" "sh"))
                             (setenv "HASKELL_GI_GIR_SEARCH_PATH" gir-dir)
                             (setenv "LD_LIBRARY_PATH" (getenv "LIBRARY_PATH")) ;; for libcairo-gobject.so lookup
                             (run-setuphs "configure" params)

                             (setenv "GHC_PACKAGE_PATH" ghc-path)
                             #t))))))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (inputs
    `(("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
      ("ghc-haskell-gi" ,ghc-haskell-gi)
      ("ghc-haskell-gi-overloading"
       ,ghc-haskell-gi-overloading)
      ("ghc-text" ,ghc-text)
      ("cairo" ,cairo)
      ("gobject-introspection" ,gobject-introspection)))
   (home-page
    "https://github.com/haskell-gi/haskell-gi")
   (synopsis "Cairo bindings")
   (description
    "Bindings for Cairo, autogenerated by haskell-gi.")
   (license license:lgpl2.1)))

(define-public ghc-io-storage
  (package
    (name "ghc-io-storage")
    (version "0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/io-storage/io-storage-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1ga9bd7iri6vlsxnjx765yy3bxc4lbz644wyw88yzvpjgz6ga3cs"))))
    (build-system haskell-build-system)
    (home-page
      "http://github.com/willdonnelly/io-storage")
    (synopsis "A key-value store in the IO monad.")
    (description
      "This library allows an application to extend the 'global state' hidden inside the IO monad with semi-arbitrary data. Data is required to be 'Typeable'. The library provides an essentially unbounded number of key-value stores indexed by strings, with each key within the stores also being a string.")
    (license license:bsd-3)))

(define-public ghc-tuple
  (package
    (name "ghc-tuple")
    (version "0.3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/tuple/tuple-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "094nx29aahyrvbcn7yca9zs2a5rxz1is7510w1q43rpvza7hdjrg"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-onetuple" ,ghc-onetuple)))
    (home-page
      "http://hackage.haskell.org/package/tuple")
    (synopsis "Various functions on tuples")
    (description
      "Various useful functions on tuples, overloaded on tuple size.")
    (license license:bsd-3)))

(define-public ghc-xml-helpers
  (package
    (name "ghc-xml-helpers")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/xml-helpers/xml-helpers-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0rrk0j7m8ws86hbjw0l4ryq4m9i8llhsag2sfisy5r1iv2zwa0lv"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-xml" ,ghc-xml)))
    (home-page "http://github.com/acw/xml-helpers")
    (synopsis
      "Some useful helper functions for the xml library.")
    (description
      "Included are some folds and maps I've found useful in parsing XML data.")
    (license license:bsd-3)))

(define-public ghc-dyre
  (package
    (name "ghc-dyre")
    (version "0.8.12")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/dyre/dyre-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "10hnlysy4bjvvznk8v902mlk4jx95qf972clyi1l32xkqrf30972"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-paths" ,ghc-paths)
        ("ghc-executable-path" ,ghc-executable-path)
        ("ghc-xdg-basedir" ,ghc-xdg-basedir)
        ("ghc-io-storage" ,ghc-io-storage)))
    (home-page "http://github.com/willdonnelly/dyre")
    (synopsis "Dynamic reconfiguration in Haskell")
    (description
      "Dyre implements dynamic reconfiguration facilities after the style of Xmonad. Dyre aims to be as simple as possible without sacrificing features, and places an emphasis on simplicity of integration with an application. A full introduction with a complete example project can be found in the documentation for 'Config.Dyre'")
    (license license:bsd-3)))

(define-public ghc-dbus-hslogger
  (package
    (name "ghc-dbus-hslogger")
    (version "0.1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/dbus-hslogger/dbus-hslogger-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0i2y69kagp53cmlb7p3y6ysr9k5wvfd0vcnpwsasyn1jpk6g80zi"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-dbus" ,ghc-dbus)
        ("ghc-hslogger" ,ghc-hslogger)
        ("ghc-optparse-applicative"
         ,ghc-optparse-applicative)))
    (home-page
      "https://github.com/IvanMalison/dbus-hslogger#readme")
    (synopsis
      "Expose a dbus server to control hslogger")
    (description
      "Please see the README on Github at <https://github.com/IvanMalison/dbus-hslogger#readme>")
    (license license:bsd-3)))

(define-public ghc-network-2.8.0.1
  (package
   (inherit ghc-network)
   (version "2.8.0.1")
   (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/network/network-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "198mam7ahny48p9fajznbqq16a8ya2gw0xm3gnm1si1rmc4hdplv"))))))


(define-public ghc-dbus
  (package
    (name "ghc-dbus")
    (version "1.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/dbus/dbus-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0ypkjlw9fn65g7p28kb3p82glk7qs7p7vyffccw7qxa3z57s12w5"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-cereal" ,ghc-cereal)
        ("ghc-conduit" ,ghc-conduit)
        ("ghc-exceptions" ,ghc-exceptions)
        ("ghc-lens" ,ghc-lens)
        ("ghc-network" ,ghc-network-2.8.0.1)
        ("ghc-parsec" ,ghc-parsec)
        ("ghc-random" ,ghc-random)
        ("ghc-split" ,ghc-split)
        ("ghc-text" ,ghc-text)
        ("ghc-th-lift" ,ghc-th-lift)
        ("ghc-vector" ,ghc-vector)
        ("ghc-xml-conduit" ,ghc-xml-conduit)
        ("ghc-xml-types" ,ghc-xml-types)))
    (native-inputs
      `(("ghc-extra" ,ghc-extra)
        ("ghc-quickcheck" ,ghc-quickcheck)
        ("ghc-resourcet" ,ghc-resourcet)
        ("ghc-tasty" ,ghc-tasty)
        ("ghc-tasty-hunit" ,ghc-tasty-hunit)
        ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page
      "https://github.com/rblaze/haskell-dbus#readme")
    (synopsis
      "A client library for the D-Bus IPC system.")
    (description
      "D-Bus is a simple, message-based protocol for inter-process communication, which allows applications to interact with other parts of the machine and the user's session using remote procedure calls. . D-Bus is a essential part of the modern Linux desktop, where it replaces earlier protocols such as CORBA and DCOP. . This library is an implementation of the D-Bus protocol in Haskell. It can be used to add D-Bus support to Haskell applications, without the awkward interfaces common to foreign bindings. . Example: connect to the session bus, and get a list of active names. . @ &#x7b;-\\# LANGUAGE OverloadedStrings \\#-&#x7d; . import Data.List (sort) import DBus import DBus.Client . main = do &#x20;   client <- connectSession &#x20;   // &#x20;   \\-- Request a list of connected clients from the bus &#x20;   reply <- call_ client (methodCall \\\"\\/org\\/freedesktop\\/DBus\\\" \\\"org.freedesktop.DBus\\\" \\\"ListNames\\\") &#x20;       &#x7b; methodCallDestination = Just \\\"org.freedesktop.DBus\\\" &#x20;       &#x7d; &#x20;   // &#x20;   \\-- org.freedesktop.DBus.ListNames() returns a single value, which is &#x20;   \\-- a list of names (here represented as [String]) &#x20;   let Just names = fromVariant (methodReturnBody reply !! 0) &#x20;   // &#x20;   \\-- Print each name on a line, sorted so reserved names are below &#x20;   \\-- temporary names. &#x20;   mapM_ putStrLn (sort names) @ . >$ ghc --make list-names.hs >$ ./list-names >:1.0 >:1.1 >:1.10 >:1.106 >:1.109 >:1.110 >ca.desrt.dconf >org.freedesktop.DBus >org.freedesktop.Notifications >org.freedesktop.secrets >org.gnome.ScreenSaver")
    (license license:asl2.0)))

(define-public ghc-hstringtemplate
  (package
    (name "ghc-hstringtemplate")
    (version "0.8.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/HStringTemplate/HStringTemplate-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "03kbmyh0713j3qhhrl7jqbmsvyq1q82h2yxq45cc9rs55sma8kjg"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-syb" ,ghc-syb)
        ("ghc-parsec" ,ghc-parsec)
        ("ghc-text" ,ghc-text)
        ("ghc-blaze-builder" ,ghc-blaze-builder)
        ("ghc-void" ,ghc-void)
        ("ghc-mtl" ,ghc-mtl)
        ("ghc-old-locale" ,ghc-old-locale)
        ("ghc-semigroups" ,ghc-semigroups)))
    (home-page
      "http://hackage.haskell.org/package/HStringTemplate")
    (synopsis
      "StringTemplate implementation in Haskell.")
    (description
      "A port of the Java library by Terrence Parr.")
    (license license:bsd-3)))

(define-public ghc-configfile
  (package
    (name "ghc-configfile")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/ConfigFile/ConfigFile-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "057mw146bip9wzs7j4b5xr1x24d8w0kr4i3inri5m57jkwspn25f"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-parsec" ,ghc-parsec)
        ("ghc-mtl" ,ghc-mtl)
        ("ghc-missingh" ,ghc-missingh)))
    (home-page
      "http://software.complete.org/configfile")
    (synopsis "Configuration file reading & writing")
    (description
      "Parser and writer for handling sectioned config files in Haskell. . The ConfigFile module works with configuration files in a standard format that is easy for the user to edit, easy for the programmer to work with, yet remains powerful and flexible.  It is inspired by, and compatible with, Python's ConfigParser module.  It uses files that resemble Windows .INI-style files, but with numerous improvements. . ConfigFile provides simple calls to both read and write config files. It's possible to make a config file parsable by this module, the Unix shell, and make.")
    (license license:bsd-3)))

(define-public ghc-taffybar
  (package
    (name "ghc-taffybar")
    (version "3.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/taffybar/taffybar-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "11k34kcxh2v8k7pr2nm1kib097n4l3klza6q8w9qp2dm31iww8y1"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-configfile" ,ghc-configfile)
        ("ghc-hstringtemplate" ,ghc-hstringtemplate)
        ("ghc-x11" ,ghc-x11)
        ("ghc-dbus" ,ghc-dbus)
        ("ghc-dbus-hslogger" ,ghc-dbus-hslogger)
        ("ghc-dyre" ,ghc-dyre)
        ("ghc-either" ,ghc-either)
        ("ghc-enclosed-exceptions"
         ,ghc-enclosed-exceptions)
        ("ghc-gi-cairo" ,ghc-gi-cairo)
        ("ghc-gi-cairo-render" ,ghc-gi-cairo-render)
        ("ghc-gi-cairo-connector"
         ,ghc-gi-cairo-connector)
        ("ghc-gi-gdk" ,ghc-gi-gdk)
        ("ghc-gi-gdkpixbuf" ,ghc-gi-gdkpixbuf)
        ("ghc-gi-gdkx11" ,ghc-gi-gdkx11)
        ("ghc-gi-glib" ,ghc-gi-glib)
        ("ghc-gi-gtk" ,ghc-gi-gtk)
        ("ghc-gi-gtk-hs" ,ghc-gi-gtk-hs)
        ("ghc-gi-pango" ,ghc-gi-pango)
        ("ghc-gtk-sni-tray" ,ghc-gtk-sni-tray)
        ("ghc-gtk-strut" ,ghc-gtk-strut)
        ("ghc-haskell-gi" ,ghc-haskell-gi)
        ("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
        ("ghc-hslogger" ,ghc-hslogger)
        ("ghc-http-client" ,ghc-http-client)
        ("ghc-http-client-tls" ,ghc-http-client-tls)
        ("ghc-http-types" ,ghc-http-types)
        ("ghc-multimap" ,ghc-multimap)
        ("ghc-old-locale" ,ghc-old-locale)
        ("ghc-parsec" ,ghc-parsec)
        ("ghc-rate-limit" ,ghc-rate-limit)
        ("ghc-regex-compat" ,ghc-regex-compat)
        ("ghc-safe" ,ghc-safe)
        ("ghc-scotty" ,ghc-scotty)
        ("ghc-split" ,ghc-split)
        ("ghc-status-notifier-item"
         ,ghc-status-notifier-item)
        ("ghc-stm" ,ghc-stm)
        ("ghc-text" ,ghc-text)
        ("ghc-time-locale-compat"
         ,ghc-time-locale-compat)
        ("ghc-time-units" ,ghc-time-units)
        ("ghc-transformers-base" ,ghc-transformers-base)
        ("ghc-tuple" ,ghc-tuple)
        ("ghc-utf8-string" ,ghc-utf8-string)
        ("ghc-xdg-basedir" ,ghc-xdg-basedir)
        ("ghc-xml" ,ghc-xml)
        ("ghc-xml-helpers" ,ghc-xml-helpers)
        ("xmonad" ,xmonad)
        ("ghc-network-uri" ,ghc-network-uri)
        ("ghc-network" ,ghc-network)
        ("ghc-optparse-applicative"
         ,ghc-optparse-applicative)))
    (home-page "http://github.com/taffybar/taffybar")
    (synopsis
      "A desktop bar similar to xmobar, but with more GUI")
    (description
      "Taffybar is a gtk+3 (through gtk2hs) based desktop information bar, intended primarily for use with XMonad, though it can also function alongside other EWMH compliant window managers. It is similar in spirit to xmobar, but it differs in that it gives up some simplicity for a reasonable helping of eye candy.")
    (license license:bsd-3)))
