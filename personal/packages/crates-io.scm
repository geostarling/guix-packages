;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
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

(define-module (personal packages crates-io)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages llvm))

(define-public rust-emacs-module
  (package
    (name "rust-emacs-module")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "emacs_module" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gf9lz735xbkyir53dyv362drfx3nin5an5cx39kd8q8kjjwix5g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bindgen" ,rust-bindgen-0.48))))
    (home-page
      "https://github.com/ubolonton/emacs-module-rs")
    (synopsis "Raw FFI for emacs-module")
    (description "Raw FFI for emacs-module")
    (license license:bsd-3)))

(define-public rust-const-random-macro
  (package
    (name "rust-const-random-macro")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "const-random-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ncc88bzfkm9y7djnvmiqh35p2cfw1d7z9fm21qn6xrkp09fql67"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
         ("rust-rand" ,rust-rand-0.6))))
    (home-page
      "https://github.com/tkaitchuck/constrandom")
    (synopsis
      "Provides the procedural macro used by const-random")
    (description
      "This package provides the procedural macro used by const-random")
    (license (list license:expat license:asl2.0))))

(define-public rust-const-random
  (package
    (name "rust-const-random")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "const-random" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1znp5r6qrxs1g406w4mndk7bh3i53hxj0r2m57rl3qv7k261lr3v"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-const-random-macro"
          ,rust-const-random-macro)
         ("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5))))
    (home-page
      "https://github.com/tkaitchuck/constrandom")
    (synopsis
      "Provides compile time random number generation.")
    (description
      "This package provides compile time random number generation.")
    (license (list license:expat license:asl2.0))))


(define-public rust-no-panic
  (package
    (name "rust-no-panic")
    (version "0.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "no-panic" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xan5v9ac1aklinc8aw16raq36pb4idjrl502np8gy32gfs6s751"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn-1))
        #:cargo-development-inputs
        (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/dtolnay/no-panic")
    (synopsis
      "Attribute macro to require that the compiler prove a function can't ever panic.")
    (description
      "Attribute macro to require that the compiler prove a function can't ever panic.")
    (license (list license:expat license:asl2.0))))


(define-public rust-ahash
  (package
    (name "rust-ahash")
    (version "0.2.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ahash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lxmn0igyizs10fasgjr4mki97wa4d7ijygjvk0lc28jiw0vacvg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-const-random" ,rust-const-random))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.2)
         ("rust-fnv" ,rust-fnv)
         ("rust-fxhash" ,rust-fxhash-0.2)
         ("rust-hex" ,rust-hex-0.3)
         ("rust-no-panic" ,rust-no-panic)
         ("rust-rand" ,rust-rand-0.6)
         ("rust-seahash" ,rust-seahash-3))))
    (home-page "https://github.com/tkaitchuck/ahash")
    (synopsis
      "A non-cryprographic hash function using AES-NI for high performance")
    (description
      "This package provides a non-cryprographic hash function using AES-NI for high performance")
    (license (list license:expat license:asl2.0))))


(define-public rust-rustc-std-workspace-alloc
  (package
    (name "rust-rustc-std-workspace-alloc")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-std-workspace-alloc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11psmqk6glglxl3zwh8slz6iynfxaifh4spd2wcnws552dqdarpz"))))
    (build-system cargo-build-system)
    (home-page "")
    (synopsis "workspace hack")
    (description "workspace hack")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustc-std-workspace-core
  (package
    (name "rust-rustc-std-workspace-core")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-std-workspace-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1309xhwyai9xpz128xrfjqkmnkvgjwddznmj7brbd8i8f58zamhr"))))
    (build-system cargo-build-system)
    (home-page "")
    (synopsis
      "Explicitly empty crate for rust-lang/rust integration
")
    (description
      "Explicitly empty crate for rust-lang/rust integration
")
    (license (list license:expat license:asl2.0))))


(define-public rust-rustc-hash
  (package
    (name "rust-rustc-hash")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-hash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1f4cnbcmz2c3zjidqszc9c4fip37ch4xl74nkkp9dw291j5zqh3m"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1))))
    (home-page
      "https://github.com/rust-lang-nursery/rustc-hash")
    (synopsis
      "speed, non-cryptographic hash used in rustc")
    (description
      "speed, non-cryptographic hash used in rustc")
    (license (list license:asl2.0 license:expat))))

(define-public rust-scopeguard
  (package
    (name "rust-scopeguard")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03aay84r1f6w87ckbpj6cc4rnsxkxcfs13n5ynxjia0qkgjiabml"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/scopeguard")
    (synopsis
      "A RAII scope guard that will run a given closure when it goes out of scope,
even if the code between panics (assuming unwinding panic).

Defines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as
shorthands for guards with one of the implemented strategies.
")
    (description
      "This package provides a RAII scope guard that will run a given closure when it goes out of scope,
even if the code between panics (assuming unwinding panic).

Defines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as
shorthands for guards with one of the implemented strategies.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-rand-0.5
  (package
    (name "rust-rand")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0w9nvc7nnln71fflamay3p489b5q4r1vdggjw3cqgqrjmxlzinfh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cloudabi" ,rust-cloudabi-0.0)
         ("rust-fuchsia-zircon" ,rust-fuchsia-zircon-0.3)
         ("rust-libc" ,rust-libc)
         ("rust-log" ,rust-log-0.4)
         ("rust-rand-core" ,rust-rand-core-0.5)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-derive" ,rust-serde-derive-1)
         ("rust-stdweb" ,rust-stdweb-0.4)
         ("rust-winapi" ,rust-winapi-0.3))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode-1))))
    (home-page "https://crates.io/crates/rand")
    (synopsis
      "Random number generators and other randomness functionality.
")
    (description
      "Random number generators and other randomness functionality.
")
    (license (list license:expat license:asl2.0))))


(define-public rust-hashbrown-0.1
  (package
    (name "rust-hashbrown")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hashbrown" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1np350nrzysy021ndn2135q5vpzrp5nli78ywz114d1vcnv2kbiv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-rayon" ,rust-rayon-1)
         ("rust-scopeguard" ,rust-scopeguard-0.3)
         ("rust-serde" ,rust-serde-1))
        #:cargo-development-inputs
        (("rust-lazy-static" ,rust-lazy-static-1.3)
         ("rust-rand" ,rust-rand-0.5)
         ("rust-rayon" ,rust-rayon-1)
         ("rust-rustc-hash" ,rust-rustc-hash)
         ("rust-serde-test" ,rust-serde-test-1))))
    (home-page
      "https://github.com/rust-lang/hashbrown")
    (synopsis
      "A Rust port of Google's SwissTable hash map")
    (description
      "This package provides a Rust port of Google's SwissTable hash map")
    (license (list license:asl2.0 license:expat))))


(define-public rust-remove-dir-all
  (package
    (name "rust-remove-dir-all")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "remove_dir_all" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bkrlyg26mgizpiy1yb2hhpgscxcag8r5fnckqsvk25608vzm0sa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-winapi" ,rust-winapi-0.3))
        #:cargo-development-inputs
        (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page
      "https://github.com/XAMPPRocky/remove_dir_all.git")
    (synopsis
      "A safe, reliable implementation of remove_dir_all for Windows")
    (description
      "This package provides a safe, reliable implementation of remove_dir_all for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-tempdir
  (package
    (name "rust-tempdir")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tempdir" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n5n86zxpgd85y0mswrp5cfdisizq2rv3la906g6ipyc03xvbwhm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rand" ,rust-rand-0.5)
         ("rust-remove-dir-all" ,rust-remove-dir-all))))
    (home-page
      "https://github.com/rust-lang/tempdir")
    (synopsis
      "A library for managing a temporary directory and deleting all contents when it's
dropped.
")
    (description
      "This package provides a library for managing a temporary directory and deleting all contents when it's
dropped.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-glob
  (package
    (name "rust-glob")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glob" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ysvi72slkw784fcsymgj4308c3y03gwjjzqxp80xdjnkbh8vqcb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))))
    (home-page "https://github.com/rust-lang/glob")
    (synopsis
      "Support for matching file paths against Unix shell style patterns.
")
    (description
      "Support for matching file paths against Unix shell style patterns.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-clang-sys
  (package
    (name "rust-clang-sys")
    (version "0.26.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clang-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1r50dwy5hj5gq07dn0qf8222d07qv0970ymx0j8n9779yayc3w3f"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-glob" ,rust-glob)
         ("rust-libc" ,rust-libc)
         ("rust-libloading" ,rust-libloading))
        #:cargo-development-inputs
        (("rust-glob" ,rust-glob))))
    (home-page
      "https://github.com/KyleMayes/clang-sys")
    (synopsis "Rust bindings for libclang.")
    (description "Rust bindings for libclang.")
    (license license:asl2.0)))



(define-public rust-bindgen-0.48
  (package
    (inherit rust-bindgen-0.50)
    (name "rust-bindgen")
    (version "0.48.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jpk0p4l4dg1lpvciq9q8wm94sjsflb1vb5x2gk9dlizv4gl2gcx"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cexpr" ,rust-cexpr)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-clang-sys" ,rust-clang-sys)
        ("rust-clap" ,rust-clap-2)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-fxhash" ,rust-fxhash-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-hashbrown" ,rust-hashbrown-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-peeking-take-while" ,rust-peeking-take-while-0.1)
        ("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote)
        ("rust-regex" ,rust-regex-1)
        ("rust-shlex" ,rust-shlex-0.1)
        ("rust-which" ,rust-which-2.0))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-diff" ,rust-diff-0.1)
        ("rust-shlex" ,rust-shlex-0.1))))))

(define-public rust-cexpr
  (package
    (name "rust-cexpr")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cexpr" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1by64ini3f058pwad3immx5cc12wr0m0kwgaxa8apzym03mj9ym7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-nom" ,rust-nom))
       #:cargo-development-inputs
       (("rust-clang-sys" ,rust-clang-sys-0.28))))
    (home-page "https://github.com/jethrogb/rust-cexpr")
    (synopsis "C expression parser and evaluator")
    (description
     "This package provides a C expression parser and evaluator.")
    (license (list license:asl2.0 license:expat))))


(define-public rust-nom
  (package
    (name "rust-nom")
    (version "4.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nom" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1mkvby8b4m61p4g1px0pwr58yfkphyp1jcfbp4qfp7l6iqdaklia"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-lexical-core" ,rust-lexical-core-0.4)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-regex" ,rust-regex-1)
        ("rust-version-check" ,rust-version-check-0.1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-jemallocator" ,rust-jemallocator-0.3)
        ("rust-version-check" ,rust-version-check-0.1))))
    (home-page "https://github.com/Geal/nom")
    (synopsis
     "Byte-oriented, zero-copy, parser combinators library")
    (description
     "This package provides a byte-oriented, zero-copy, parser
combinators library.")
    (license license:expat)))

(define-public rust-darling-macro
  (package
    (name "rust-darling-macro")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling_macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lcq9418w0vmvncg4a3n9k64zjvqz0048aviqi0rmlpiqv0xmn66"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-darling-core" ,rust-darling-core)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://github.com/TedDriggs/darling")
    (synopsis
      "Internal support for a proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.
")
    (description
      "Internal support for a proc-macro library for reading attributes into structs when
implementing custom derives.  Use https://crates.io/crates/darling in your code.
")
    (license license:expat)))

(define-public rust-strsim
  (package
    (name "rust-strsim")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0l7mkwvdk4vgnml67b85mczk466074aj8yf25gjrjslj4l0khkxv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dguo/strsim-rs")
    (synopsis
      "Implementations of string similarity metrics.
Includes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro, and Jaro-Winkler.
")
    (description
      "Implementations of string similarity metrics.
Includes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro, and Jaro-Winkler.
")
    (license license:expat)))

(define-public rust-ident-case
  (package
    (name "rust-ident-case")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ident_case" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/TedDriggs/ident_case")
    (synopsis
      "Utility for applying case rules to Rust identifiers.")
    (description
      "Utility for applying case rules to Rust identifiers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fnv
  (package
    (name "rust-fnv")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fnv" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ww56bi1r5b8id3ns9j3qxbi7w5h005rzhiryy0zi9h97raqbb9g"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/rust-fnv")
    (synopsis
      "Fowlerâ\x80\x93Nollâ\x80\x93Vo hash function")
    (description
      "Fowlerâ\x80\x93Nollâ\x80\x93Vo hash function")
    (license (list license:asl2.0 license:expat))))

(define-public rust-darling-core
  (package
    (name "rust-darling-core")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b201dx4m58l5ac7gmbjvbf4z2xipnk5d4pqa7mz7gy3f21h3z3a"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-fnv" ,rust-fnv)
         ("rust-ident-case" ,rust-ident-case)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote)
         ("rust-strsim" ,rust-strsim)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://github.com/TedDriggs/darling")
    (synopsis
      "Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.
")
    (description
      "Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives.  Use https://crates.io/crates/darling in your code.
")
    (license license:expat)))

(define-public rust-darling
  (package
    (name "rust-darling")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mnksf0i47pb7sxvi1iqfwmqy9iny0x8w56ilybpb431b46cpyzw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-darling-core" ,rust-darling-core)
         ("rust-darling-macro" ,rust-darling-macro))
        #:cargo-development-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://github.com/TedDriggs/darling")
    (synopsis
      "A proc-macro library for reading attributes into structs when
implementing custom derives.
")
    (description
      "This package provides a proc-macro library for reading attributes into structs when
implementing custom derives.
")
    (license license:expat)))

(define-public rust-emacs-macros
  (package
    (name "rust-emacs-macros")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "emacs-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0390y8vafxdi334hhgrzvcqjq3n5ckcmvilqcfp8vajjq8irrly6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-darling" ,rust-darling)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://github.com/ubolonton/emacs-module-rs")
    (synopsis "Proc macros for emacs modules")
    (description "Proc macros for emacs modules")
    (license license:bsd-3)))

(define-public rust-rustc-std-workspace-core
  (package
    (name "rust-rustc-std-workspace-core")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-std-workspace-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1309xhwyai9xpz128xrfjqkmnkvgjwddznmj7brbd8i8f58zamhr"))))
    (build-system cargo-build-system)
    (home-page "")
    (synopsis
      "Explicitly empty crate for rust-lang/rust integration
")
    (description
      "Explicitly empty crate for rust-lang/rust integration
")
    (license (list license:expat license:asl2.0))))

(define-public rust-libc
  (package
    (name "rust-libc")
    (version "0.2.66")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0n0mwry21fxfwc063k33mvxk8xj7ia5ar8m42c9ymbam2ksb25fm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis
      "Raw FFI bindings to platform libraries like libc.
")
    (description
      "Raw FFI bindings to platform libraries like libc.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-libc-print
  (package
    (name "rust-libc-print")
    (version "0.1.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc-print" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sh4l815w7zxg8w17fvwj63y421sjqxxrdamzwyvg90n6mr70phv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-libc" ,rust-libc))))
    (home-page
      "https://github.com/mmastrac/rust-libc-print")
    (synopsis
      "println! and eprintln! macros on libc without stdlib")
    (description
      "println! and eprintln! macros on libc without stdlib")
    (license (list license:asl2.0 license:expat))))

(define-public rust-rustversion
  (package
    (name "rust-rustversion")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustversion" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11krxgi7j6h5crhs6ws06wwzjvgdqaazvli805xja5vyi6ykh19s"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://github.com/dtolnay/rustversion")
    (synopsis
      "Conditional compilation according to rustc compiler version")
    (description
      "Conditional compilation according to rustc compiler version")
    (license (list license:expat license:asl2.0))))

(define-public rust-quote
  (package
    (name "rust-quote")
    (version "0.6.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qgqq48jymp5h4y082aanf25hrw6bpb678xh3zw993qfhxmkpqkc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1))
        #:cargo-development-inputs
        (("rust-rustversion" ,rust-rustversion)
         ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    (license (list license:expat license:asl2.0))))

(define-public rust-ctor
  (package
    (name "rust-ctor")
    (version "0.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ctor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p7fd2zp3lkb098sn740jlf3np8qg5ivycsc037b4jhqsixf736d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn-1))
        #:cargo-development-inputs
        (("rust-libc-print" ,rust-libc-print))))
    (home-page
      "https://github.com/mmastrac/rust-ctor")
    (synopsis
      "__attribute__((constructor)) for Rust")
    (description
      "__attribute__((constructor)) for Rust")
    (license (list license:asl2.0 license:expat))))

(define-public rust-emacs
  (package
    (name "rust-emacs")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "emacs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xn6nr8m3p24irlmv3i7c9n95yb10qc6aikwqv99qlmhbky5x0z7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ctor" ,rust-ctor)
         ("rust-emacs-macros" ,rust-emacs-macros)
         ("rust-emacs-module" ,rust-emacs-module)
         ("rust-failure" ,rust-failure-0.1)
         ("rust-failure-derive" ,rust-failure-derive-0.1)
         ("rust-lazy-static" ,rust-lazy-static-1.3))))
    (home-page
      "https://github.com/ubolonton/emacs-module-rs")
    (synopsis
      "Rust library for creating Emacs's dynamic modules")
    (description
      "Rust library for creating Emacs's dynamic modules")
    (license license:bsd-3)))


(define-public rust-libloading
  (package
    (name "rust-libloading")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libloading" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lyply8rcqc8agajzxs7bq6ivba9dnn1i68kgb9z2flnfjh13cgj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-winapi" ,rust-winapi-0.3))))
    (home-page
      "https://github.com/nagisa/rust_libloading/")
    (synopsis
      "A safer binding to platformâ\x80\x99s dynamic library loading utilities")
    (description
      "This package provides a safer binding to platformâ\x80\x99s dynamic library loading utilities")
    (license license:isc)))


(define-public rust-pin-project-internal
  (package
    (name "rust-pin-project-internal")
    (version "0.4.22")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pin-project-internal" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xxac6f3ip45zqbfcmmk748ywjw9sbavz1fcswvqgn3rrx2zs3va"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://github.com/taiki-e/pin-project")
    (synopsis
      "An internal crate to support pin_project - do not use directly
")
    (description
      "An internal crate to support pin_project - do not use directly
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pin-project
  (package
    (name "rust-pin-project")
    (version "0.4.22")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pin-project" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05wwxy46j9z27ibbiisjqk0rivf0z00h4al1f92mwjp9pz6sdqqj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-pin-project-internal"
          ,rust-pin-project-internal))))
    (home-page
      "https://github.com/taiki-e/pin-project")
    (synopsis
      "A crate for safe and ergonomic pin-projection.
")
    (description
      "This package provides a crate for safe and ergonomic pin-projection.
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-tracing-core-0.1.10
  (package
    (name "rust-tracing-core")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05dpa21valy0c6nj5ybn951774icxhdb70cwq0ida7088yd3ma0a"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static-1))))
    (home-page "https://tokio.rs")
    (synopsis
      "Core primitives for application-level tracing.
")
    (description
      "Core primitives for application-level tracing.
")
    (license license:expat)))


(define-public rust-tracing-futures
  (package
    (name "rust-tracing-futures")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-futures" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0k4vd3jyqz9cx8rbwbp0p93qfp1w6rfk7sc6c1jh1ai18zqvcyxb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-futures-task" ,rust-futures-task-0.3)
         ("rust-pin-project" ,rust-pin-project)
         ("rust-tokio" ,rust-tokio-0.1)
         ("rust-tokio-executor" ,rust-tokio-executor-0.1)
         ("rust-tracing" ,rust-tracing))
        #:cargo-development-inputs
        (("rust-tokio" ,rust-tokio-0.1)
         ("rust-tokio-test" ,rust-tokio-test-0.2)
         ("rust-tracing-core" ,rust-tracing-core-0.1.10))))
    (home-page "https://tokio.rs")
    (synopsis
      "Utilities for instrumenting `futures` with `tracing`.
")
    (description
      "Utilities for instrumenting `futures` with `tracing`.
")
    (license license:expat)))

(define-public rust-tracing-attributes
  (package
    (name "rust-tracing-attributes")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-attributes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0k4qvq437md3zynm8qbas6jfb0xp222xisij6af3r4pxwc6svfwr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))
        #:cargo-development-inputs
        (("rust-async-trait" ,rust-async-trait)
         ("rust-tokio-test" ,rust-tokio-test-0.2)
         ("rust-tracing" ,rust-tracing)
         ("rust-tracing-core" ,rust-tracing-core-0.1.10)
         ("rust-tracing-futures" ,rust-tracing-futures))))
    (home-page "https://tokio.rs")
    (synopsis
      "Procedural macro attributes for automatically instrumenting functions.
")
    (description
      "Procedural macro attributes for automatically instrumenting functions.
")
    (license license:expat)))

(define-public rust-tracing
  (package
    (name "rust-tracing")
    (version "0.1.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08r98sp312rxwsm26wzgd013w134gfvhdswhv6r8q8bd26fvbim7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-log" ,rust-log-0.4)
         ("rust-tracing-attributes"
          ,rust-tracing-attributes)
         ("rust-tracing-core" ,rust-tracing-core-0.1.10))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.3)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-log" ,rust-log-0.4))))
    (home-page "https://tokio.rs")
    (synopsis
      "Application-level tracing for Rust.
")
    (description
      "Application-level tracing for Rust.
")
    (license license:expat)))

(define-public rust-async-trait
  (package
    (name "rust-async-trait")
    (version "0.1.36")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-trait" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14pbhl1psi81851in004fh3ccdzy4ar12ykbwar31kpxxymy6rd2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))
        #:cargo-development-inputs
        (("rust-rustversion" ,rust-rustversion)
         ("rust-tracing" ,rust-tracing)
         ("rust-tracing-attributes"
          ,rust-tracing-attributes)
         ("rust-tracing-futures" ,rust-tracing-futures)
         ("rust-trybuild" ,rust-trybuild-1))))
    (home-page
      "https://github.com/dtolnay/async-trait")
    (synopsis "Type erasure for async trait methods")
    (description
      "Type erasure for async trait methods")
    (license (list license:expat license:asl2.0))))
