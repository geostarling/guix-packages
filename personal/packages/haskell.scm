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
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages file)
  #:use-module (gnu packages c)

  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
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
       ("gcc-toolchain" ,gcc-toolchain)))     ; FIXME: required for crt1.o
    (home-page
      "http://hackage.haskell.org/package/monky")
    (synopsis
      "A system state collecting library and application")
    (description
      "monky started as an alternative to conky, i3-status or similar, that's fully containing in one process. Also making an effort to keep file descriptors or handles as long as possible. monky 2.0 is the first version on hackage. . monky consists of multiple parts. A number of collection modules, output modules, \"examples\" and a helper application. . * collection modules . The collection modules are the core library. Collection modules export a handle that can be used to get some detail about the system. They can be used without the other parts of this package, but they are designed with monky in mind. . * output modules . Output modules take a monky specific output type and transform it into something that can be displayed by some external application. That may be a statusbar (dzen2), the terminal, a network port, that makes it accessible on another machine, or any other thing. . * examples . The examples are a group of modules, that use collection modules to create the output used by the output modules. The flexibility of the examples varies greatly, some may are really flexible, some are rather static. The intended usecase is for users to create their own examples and use them, if they don't want to use the provided ones. . Later on, I want to create something like xmonad-contrib or a collection of user examples, to provide better usability for users with few to no experience with haskell. . * helper application . The helper application is used to compile the actual output generator and can generate an example configuration. . To generate an example configuration in /~\\/.monky/ simply run `monky`. Then modify /~\\/.monky\\/monky.hs/ to create your own configuration. . Modules can have two types. 'PollModule' and 'EvtModule'. 'PollModule's work by the main loop asking the module to generate new output, while 'EvtModule's block until some event is received and update their output on demand. Some handles are an instance of both, 'PollModule' and 'EvtModule'. 'EvtModule' should be preferred, since they induce less load on your system. The monky main-loop does one \"tick\" every second. 'PollModules' are updated each /N/ ticks, where /N/ is passed to 'pollPack'.")
    (license license:lgpl3)))
