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

(define-module (personal packages rustlibs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
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
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gnome)      ;; for vte and libpeas
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages llvm)

  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages textutils))

(define-public rust-bitflags
 (package
   (name "rust-bitflags")
   (version "1.0.4")
   (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitflags" version))
       (file-name
         (string-append name "-" version ".tar.gz"))
       (sha256
         (base32
           "04nfhscc9mxwhmai5xgwh4q458rjszmwsvkpf752g1j6dyklg012"))))
   (build-system cargo-build-system)
   (home-page
     "https://github.com/bitflags/bitflags")
   (synopsis
     "A macro to generate structures which behave like bitflags.
 ")
   (description
     "This package provides a macro to generate structures which behave like bitflags.
 ")
   (license #f)))


(define-public rust-autocfg
  (package
   (name "rust-autocfg")
   (version "0.1.2")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "autocfg" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "16fprz5qi7paij5swyxb2hjris6d7bjzm9v8805gcjfswaz41mm6"))))
   (build-system cargo-build-system)
   (home-page "https://github.com/cuviper/autocfg")
   (synopsis
    "Automatic cfg for Rust compiler features")
   (description
    "Automatic cfg for Rust compiler features")
   (license #f)))

(define-public rust-serde-bytes
  (package
   (name "rust-serde-bytes")
   (version "0.11.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "serde_bytes" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0sc5n336i7q4fiij4l8f892zcirgybrbxzl8bp51qxzqdvdlgzxa"))))
   (build-system cargo-build-system)
   (native-inputs
    `(("rust-bincode" ,rust-bincode "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-test" ,rust-serde-test "src")))
   (inputs `(("rust-serde" ,rust-serde "src")))
   (home-page "https://github.com/serde-rs/bytes")
   (synopsis
    "Optimized handling of `&[u8]` and `Vec<u8>` for Serde")
   (description
    "Optimized handling of `&[u8]` and `Vec<u8>` for Serde")
   (license #f)))

(define-public rust-serde-derive
  (package
   (name "rust-serde-derive")
   (version "1.0.90")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "serde_derive" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0yfysbp1l27rcyh4dld92nd6wwpmidfb8qqr7nr6iwa4qaz85z2q"))))
   (build-system cargo-build-system)
   (native-inputs
    `(("rust-serde" ,rust-serde "src")))
   (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
   (home-page "https://serde.rs")
   (synopsis
    "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
   (description
    "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
   (license #f)))


(define-public rust-proc-macro2
  (package
   (name "rust-proc-macro2")
   (version "0.4.29")
   (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2" version))
       (file-name
         (string-append name "-" version ".tar.gz"))
       (sha256
         (base32
           "05jkjvzh12x70l327yp2xkb7lq8ig96y1rck8p731ax7lz72gj34"))))
   (build-system cargo-build-system)
   (native-inputs
     `(("rust-quote" ,rust-quote "src")))
   (inputs
     `(("rust-unicode-xid" ,rust-unicode-xid "src")))
   (home-page
     "https://github.com/alexcrichton/proc-macro2")
   (synopsis
     "A stable implementation of the upcoming new `proc_macro` API. Comes with an
 option, off by default, to also reimplement itself in terms of the upstream
 unstable API.
 ")
   (description
     "This package provides a stable implementation of the upcoming new `proc_macro` API.  Comes with an
 option, off by default, to also reimplement itself in terms of the upstream
 unstable API.
 ")
   (license #f)))

(define-public rust-unicode-xid
  (package
    (name "rust-unicode-xid")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z57lqh4s18rr4x0j4fw4fmp9hf9346h0kmdgqsqx0fhjr3k0wpw"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/unicode-rs/unicode-xid")
    (synopsis
      "Determine whether characters have the XID_Start
  or XID_Continue properties according to
  Unicode Standard Annex #31.
  ")
    (description
      "Determine whether characters have the XID_Start
  or XID_Continue properties according to
  Unicode Standard Annex #31.
  ")
    (license #f)))

(define-public rust-byteorder
  (package
    (name "rust-byteorder")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "byteorder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1syvclxqjwf6qfq98y3fiy82msjp7q8wh7qkvf9b5pkw585b26d0"))))
    (build-system cargo-build-system)
    (native-inputs
      `(("rust-quickcheck" ,rust-quickcheck "src")
        ("rust-rand" ,rust-rand "src")))
    (home-page
      "https://github.com/BurntSushi/byteorder")
    (synopsis
      "Library for reading/writing numbers in big-endian and little-endian.")
    (description
      "Library for reading/writing numbers in big-endian and little-endian.")
    (license #f)))


(define-public rust-quote
 (package
  (name "rust-quote")
  (version "0.6.12")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "quote" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1nw0klza45hf127kfyrpxsxd5jw2l6h21qxalil3hkr7bnf7kx7s"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")))
  (home-page "https://github.com/dtolnay/quote")
  (synopsis "Quasi-quoting macro quote!(...)")
  (description "Quasi-quoting macro quote!(...)")
  (license #f)))




(define-public rust-bincode
  (package
   (name "rust-bincode")
   (version "1.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "bincode" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0dgzpvs4hc9msd9gihmklx02sbg0mnf5mw4mxgz2yhddq5a8x74m"))))
   (build-system cargo-build-system)
   (native-inputs
    `(("rust-autocfg" ,rust-autocfg "src")
      ("rust-serde-bytes" ,rust-serde-bytes "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
   (inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-serde" ,rust-serde "src")))
   (home-page "https://github.com/TyOverby/bincode")
   (synopsis
    "A binary serialization / deserialization strategy that uses Serde for transforming structs into bytes and vice versa!")
   (description
    "This package provides a binary serialization / deserialization strategy that uses Serde for transforming structs into bytes and vice versa!")
   (license #f)))


(define-public rust-chrono
  (package
   (name "rust-chrono")
   (version "0.4.6")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "chrono" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0y1qcgnr7g9zgnmlzcrn31vn91x1vakpph9qgjnnzchw2a0ji4a5"))))
   (build-system cargo-build-system)
   (native-inputs
    `(("rust-bincode" ,rust-bincode "src")
      ("rust-num-iter" ,rust-num-iter "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")))
   (inputs
    `(("rust-num-integer" ,rust-num-integer "src")
      ("rust-num-traits" ,rust-num-traits "src")
      ("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-time" ,rust-time "src")))
   (home-page
    "https://github.com/chronotope/chrono")
   (synopsis "Date and time library for Rust")
   (description "Date and time library for Rust")
   (license #f)))


(define-public rust-skim
  (package
   (name "rust-skim")
   (version "0.6.6")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "skim" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0xvgivrpj8jplyv80nk17skz8x8gzj2nx6j4nk1c7zbyf18hd2m0"))))
   (build-system cargo-build-system)
   (inputs
    `(("rust-bitflags" ,rust-bitflags "src")
      ("rust-chrono" ,rust-chrono "src")
      ("rust-clap" ,rust-clap "src")
      ("rust-derive-builder"
       ,rust-derive-builder
       "src")
      ("rust-env-logger" ,rust-env-logger "src")
      ("rust-fuzzy-matcher" ,rust-fuzzy-matcher "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-log" ,rust-log "src")
      ("rust-nix" ,rust-nix "src")
      ("rust-rayon" ,rust-rayon "src")
      ("rust-regex" ,rust-regex "src")
      ("rust-shlex" ,rust-shlex "src")
      ("rust-time" ,rust-time "src")
      ("rust-timer" ,rust-timer "src")
      ("rust-tuikit" ,rust-tuikit "src")
      ("rust-unicode-width" ,rust-unicode-width "src")
      ("rust-vte" ,rust-vte "src")))
   (home-page "https://github.com/lotabout/skim")
   (synopsis "Fuzzy Finder in rust!")
   (description "Fuzzy Finder in rust!")
   (license #f)))
