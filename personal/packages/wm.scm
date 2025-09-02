;; SPDX-FileCopyrightText: 2022-2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (personal packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system python)
  #:use-module (guix build-system guile)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages base)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages music)
  #:use-module (gnu packages rust-crates)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-audio)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages debug)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))


(define-public guile-swayer
  (package
    (name "guile-swayer")
    (version "0.3.0")
    (home-page "https://github.com/ebeem/guile-swayer")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ebeem/guile-swayer")
             (commit "9c962281f405453fb3770dd0546ef6951c9236dd")))
       (sha256 (base32 "09c0143q9sm75xp1qz7a7ihdqfwqg4w8nlq0mmnivhvamww775ss"))))
    (native-inputs (list guile-3.0))
    (build-system guile-build-system)
    (synopsis "Extensible Guile bindings for SwayWM")
    (description "Extensible Guile bindings for SwayWM")
    (license license:expat)))

(define-public rust-upower-dbus-0.3
  (package
    (name "rust-upower-dbus")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "upower_dbus" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "133g244g12acdzqhgkawkv2czvn4p12v18jzgwcfbclax5yxgcdh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-zbus" ,rust-zbus-3))))
    (home-page "https://github.com/pop-os/upower-dbus")
    (synopsis "UPower info via zbus")
    (description "This package provides UPower info via zbus.")
    (license license:expat)))

(define-public rust-winnow-0.7
  (package
    (name "rust-winnow")
    (version "0.7.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winnow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kfb2m5ahpqg037a34rgkm18x6nj2mw4fwn89ya0gdw06ipb5ivl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.3)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-is-terminal-polyfill" ,rust-is-terminal-polyfill-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-terminal-size" ,rust-terminal-size-0.4))))
    (home-page "https://github.com/winnow-rs/winnow")
    (synopsis "byte-oriented, zero-copy, parser combinators library")
    (description
     "This package provides a byte-oriented, zero-copy, parser combinators library.")
    (license license:expat)))

(define-public rust-toml-write-0.1
  (package
    (name "rust-toml-write")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml_write" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "008qlhqlqvljp1gpp9rn5cqs74gwvdgbvs92wnpq8y3jlz4zi6ax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "low-level interface for writing out TOML")
    (description
     "This package provides a low-level interface for writing out TOML.")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-edit-0.22
  (package
    (name "rust-toml-edit")
    (version "0.22.27")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml_edit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16l15xm40404asih8vyjvnka9g0xs9i4hfb6ry3ph9g419k8rzj1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-kstring" ,rust-kstring-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-spanned" ,rust-serde-spanned-0.6)
                       ("rust-toml-datetime" ,rust-toml-datetime-0.6)
                       ("rust-toml-write" ,rust-toml-write-0.1)
                       ("rust-winnow" ,rust-winnow-0.7))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "Yet another format-preserving TOML parser")
    (description
     "This package provides Yet another format-preserving TOML parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-datetime-0.6
  (package
    (name "rust-toml-datetime")
    (version "0.6.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml_datetime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "TOML-compatible datetime type")
    (description "This package provides a TOML-compatible datetime type.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-spanned-0.6
  (package
    (name "rust-serde-spanned")
    (version "0.6.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_spanned" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "Serde-compatible spanned Value")
    (description "This package provides Serde-compatible spanned Value.")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-0.8
  (package
    (name "rust-toml")
    (version "0.8.23")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-spanned" ,rust-serde-spanned-0.6)
                       ("rust-toml-datetime" ,rust-toml-datetime-0.6)
                       ("rust-toml-edit" ,rust-toml-edit-0.22))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis
     "native Rust encoder and decoder of TOML-formatted files and streams. Provides
implementations of the standard Serialize/Deserialize traits for TOML data to
facilitate deserializing and serializing Rust structures.")
    (description
     "This package provides a native Rust encoder and decoder of TOML-formatted files
and streams.  Provides implementations of the standard Serialize/Deserialize
traits for TOML data to facilitate deserializing and serializing Rust
structures.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-impl-2
  (package
    (name "rust-thiserror-impl")
    (version "2.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07bsn7shydaidvyyrm7jz29vp78vrxr9cr9044rfmn078lmz8z3z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description
     "This package provides Implementation detail of the `thiserror` crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-2
  (package
    (name "rust-thiserror")
    (version "2.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "024791nsc0np63g2pq30cjf9acj38z3jwx9apvvi8qsqmqnqlysn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror-impl" ,rust-thiserror-impl-2))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "derive(Error)")
    (description "This package provides derive(Error).")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-yaml-0.9
  (package
    (name "rust-serde-yaml")
    (version "0.9.34+deprecated")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_yaml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0isba1fjyg3l6rxk156k600ilzr8fp7crv82rhal0rxz5qd1m2va"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unsafe-libyaml" ,rust-unsafe-libyaml-0.2))))
    (home-page "https://github.com/dtolnay/serde-yaml")
    (synopsis "YAML data format for Serde")
    (description "This package provides YAML data format for Serde.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.140")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wwkp4vc20r87081ihj3vpyz5qf7wqkqipq17v99nv6wjrp8n1i0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "JSON serialization file format")
    (description "This package provides a JSON serialization file format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.219")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "001azhjmj7ya52pmfiw4ppxm16nd44y15j2pf5gkcwrcgz7pc0jv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "This package provides Macros 1.1 implementation of #[derive(Serialize, Deserialize)].")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.219")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dl6nyxnsi82a197sd752128a4avm6mxnscywas1jq30srp2q3jz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ron-0.10
  (package
    (name "rust-ron")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ron" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zvv5mbzjd5hb4zgrw71154jn6wsdlsx2vggmrrkxiw1pzvvdkmy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.22)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "https://github.com/ron-rs/ron")
    (synopsis "Rusty Object Notation")
    (description "This package provides Rusty Object Notation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-wasm-bindgen-0.6
  (package
    (name "rust-serde-wasm-bindgen")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-wasm-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sz1l4v8059hiizf5z7r2spm6ws6sqcrs4qgqwww3p7dy1ly20l3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/RReverser/serde-wasm-bindgen")
    (synopsis "Native Serde adapter for wasm-bindgen")
    (description
     "This package provides Native Serde adapter for wasm-bindgen.")
    (license license:expat)))

(define-public rust-console-error-panic-hook-0.1
  (package
    (name "rust-console-error-panic-hook")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "console_error_panic_hook" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g5v8s0ndycc10mdn6igy914k645pgpcl8vjpz6nvxkhyirynsm0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/rustwasm/console_error_panic_hook")
    (synopsis
     "panic hook for `wasm32-unknown-unknown` that logs panics to `console.error`")
    (description
     "This package provides a panic hook for `wasm32-unknown-unknown` that logs panics
to `console.error`.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libcorn-0.10
  (package
    (name "rust-libcorn")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libcorn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qaaspxjjh28k9cbg6ad7gb99j95x3qdzan3rgldpi0gchxs1sny"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-console-error-panic-hook" ,rust-console-error-panic-hook-0.1)
                       ("rust-criterion" ,rust-criterion-0.5)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-mlua" ,rust-mlua-0.9)
                       ("rust-pest" ,rust-pest-2)
                       ("rust-pest-derive" ,rust-pest-derive-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-wasm-bindgen" ,rust-serde-wasm-bindgen-0.6)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wee-alloc" ,rust-wee-alloc-0.4))))
    (home-page "https://github.com/JakeStanger/corn")
    (synopsis
     "Parsing engine for Corn, a simple and pain-free configuration language")
    (description
     "This package provides Parsing engine for Corn, a simple and pain-free configuration language.")
    (license license:expat)))

(define-public rust-serde-mobile-3
  (package
    (name "rust-serde-mobile")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-mobile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1911rw3xdhrlidll1b1dcarbnw6bqvycr7jbnkizvwgl96n14bn3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Lucretiel/serde-mobile")
    (synopsis
     "Move-oriented sequence and map access types for serde deserializers")
    (description
     "This package provides Move-oriented sequence and map access types for serde deserializers.")
    (license license:mpl2.0)))

(define-public rust-horrorshow-0.8
  (package
    (name "rust-horrorshow")
    (version "0.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "horrorshow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19k7xhxy6k5f3f983wafiq1ggcyxbv8ypf2cj1yiakrx5s9njaxy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/Stebalien/horrorshow-rs")
    (synopsis "a templating library written in rust macros")
    (description
     "This package provides a templating library written in rust macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lazy-format-1
  (package
    (name "rust-lazy-format")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lazy_format" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hxxmyarfl700yd7ppkmvc74cdp9b8ckbyb48j9hcc6nkjz64mmh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-horrorshow" ,rust-horrorshow-0.8))))
    (home-page "https://github.com/Lucretiel/lazy_format")
    (synopsis "utility crate for lazily formatting values for later")
    (description
     "This package provides a utility crate for lazily formatting values for later.")
    (license license:mpl2.0)))

(define-public rust-indent-write-2
  (package
    (name "rust-indent-write")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indent_write" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hqjp80argdskrhd66g9sh542yxy8qi77j6rc69qd0l7l52rdzhc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/Lucretiel/indent-write")
    (synopsis "Simple Write adapters to add line indentation")
    (description
     "This package provides Simple Write adapters to add line indentation.")
    (license license:mpl2.0)))

(define-public rust-brownstone-3
  (package
    (name "rust-brownstone")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brownstone" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08kzq5dmf1cl8pzhp6wm8wa3wsicrc4za8zjvjzi3s2kz7j9x0y5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7))))
    (home-page "https://github.com/Lucretiel/brownstone")
    (synopsis "Utilities for building fixed-size arrays")
    (description
     "This package provides Utilities for building fixed-size arrays.")
    (license license:mpl2.0)))

(define-public rust-nom-supreme-0.8
  (package
    (name "rust-nom-supreme")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nom-supreme" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09xbvjqwbb7l5sfam4gzp7n93d14bp4m3zsrhxc5j68zj1naxlrb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-brownstone" ,rust-brownstone-3)
                       ("rust-indent-write" ,rust-indent-write-2)
                       ("rust-joinery" ,rust-joinery-2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-nom" ,rust-nom-7))))
    (home-page "https://github.com/Lucretiel/nom-supreme")
    (synopsis "collection of excellent utilities for nom")
    (description
     "This package provides a collection of excellent utilities for nom.")
    (license license:mpl2.0)))

(define-public rust-kaydle-primitives-3
  (package
    (name "rust-kaydle-primitives")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kaydle-primitives" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wl2v1hksw982xwn0i00sxi08vamk8y3yfhy7y2w8123ydbzcc9g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-nom-supreme" ,rust-nom-supreme-0.8)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Lucretiel/kaydle")
    (synopsis
     "Low level primitive parsers for KDL, intended as a building block for higher level parsers or deserializers")
    (description
     "This package provides Low level primitive parsers for KDL, intended as a building block for higher
level parsers or deserializers.")
    (license license:mpl2.0)))

(define-public rust-kaydle-0.2
  (package
    (name "rust-kaydle")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kaydle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lyvf98pd1f3s5a5xz12qfarx1npn0w1rvpmi0r6yzags69y2b9b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-kaydle-primitives" ,rust-kaydle-primitives-3)
                       ("rust-lazy-format" ,rust-lazy-format-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-nom-supreme" ,rust-nom-supreme-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-mobile" ,rust-serde-mobile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Lucretiel/kaydle")
    (synopsis "Serde serializer and deserializer for KDL")
    (description
     "This package provides Serde serializer and deserializer for KDL.")
    (license license:mpl2.0)))

(define-public rust-redox-users-0.5
  (package
    (name "rust-redox-users")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "redox_users" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0awxx66izdw6kz97r3zxrl5ms5f6dqi5l0f58mlsvlmx8wyrsvyx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-libredox" ,rust-libredox-0.1)
                       ("rust-rust-argon2" ,rust-rust-argon2-0.8)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://gitlab.redox-os.org/redox-os/users")
    (synopsis "Rust library to access Redox users and groups functionality")
    (description
     "This package provides a Rust library to access Redox users and groups
functionality.")
    (license license:expat)))

(define-public rust-dirs-sys-0.5
  (package
    (name "rust-dirs-sys")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dirs-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aqzpgq6ampza6v012gm2dppx9k35cdycbj54808ksbys9k366p0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-option-ext" ,rust-option-ext-0.2)
                       ("rust-redox-users" ,rust-redox-users-0.5)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/dirs-dev/dirs-sys-rs")
    (synopsis
     "System-level helper functions for the dirs and directories crates")
    (description
     "This package provides System-level helper functions for the dirs and directories crates.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dirs-6
  (package
    (name "rust-dirs")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dirs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0knfikii29761g22pwfrb8d0nqpbgw77sni9h2224haisyaams63"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dirs-sys" ,rust-dirs-sys-0.5))))
    (home-page "https://github.com/soc/dirs-rs")
    (synopsis
     "tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (description
     "This package provides a tiny low-level library that provides platform-specific
standard locations of directories for config, cache and other data on Linux,
Windows, @code{macOS} and Redox by leveraging the mechanisms defined by the XDG
base/user directory specifications on Linux, the Known Folder API on Windows,
and the Standard Directory guidelines on @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-universal-config-0.5
  (package
    (name "rust-universal-config")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "universal-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mym33bw6irhkq6rf84ywkwbbkp7dw4xs9n62yz72ci12wari4nh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dirs" ,rust-dirs-6)
                       ("rust-kaydle" ,rust-kaydle-0.2)
                       ("rust-libcorn" ,rust-libcorn-0.10)
                       ("rust-ron" ,rust-ron-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-xml-rs" ,rust-serde-xml-rs-0.6)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-toml" ,rust-toml-0.8)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "")
    (synopsis
     "library to simplify reading configuration files from various file formats.")
    (description
     "This package provides a library to simplify reading configuration files from
various file formats.")
    (license license:expat)))

(define-public rust-dbusmenu-glib-sys-0.1
  (package
    (name "rust-dbusmenu-glib-sys")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dbusmenu-glib-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1synl6ix79a5bgihywd70zdl1n0rmjbwjlxr891wj6076d0fvybz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://github.com/ralismark/dbusmenu-rs")
    (synopsis "FFI bindings to dbusmenu-glib")
    (description "This package provides FFI bindings to dbusmenu-glib.")
    (license license:lgpl3)))

(define-public rust-dbusmenu-gtk3-sys-0.1
  (package
    (name "rust-dbusmenu-gtk3-sys")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dbusmenu-gtk3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jkfrdhiygd2jvfrywhy41xl3xmn7ppci6sp9jl3h3pci9gvlc3g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dbusmenu-glib-sys" ,rust-dbusmenu-glib-sys-0.1)
                       ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.20)
                       ("rust-gdk-sys" ,rust-gdk-sys-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.20)
                       ("rust-gtk-sys" ,rust-gtk-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://github.com/ralismark/dbusmenu-rs")
    (synopsis "FFI bindings to dbusmenu-gtk3")
    (description "This package provides FFI bindings to dbusmenu-gtk3.")
    (license license:lgpl3)))

(define-public rust-system-tray-0.4
  (package
    (name "rust-system-tray")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "system-tray" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1182k0nz22j7pjs51hmsanfcnx7v0f5xmy8bj8k2ih0vyjlzgvy5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dbusmenu-gtk3-sys" ,rust-dbusmenu-gtk3-sys-0.1)
                       ("rust-gtk" ,rust-gtk-0.18)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-zbus" ,rust-zbus-3))))
    (home-page "https://github.com/jakestanger/system-tray")
    (synopsis
     "Async `StatusNotifierItem` and `DBusMenu` client for custom tray implementations")
    (description
     "This package provides Async `@code{StatusNotifierItem`} and `D@code{BusMenu`} client for custom tray
implementations.")
    (license license:expat)))

(define-public rust-from-variants-impl-1
  (package
    (name "rust-from-variants-impl")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "from_variants_impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17p6djij0ar0c9dlfnq4dj9bgmq16fcsf3winjr9cv8fm12fd9am"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-darling" ,rust-darling-0.14)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/TedDriggs/from_variants")
    (synopsis "Internal helper crate for from_variants crate")
    (description
     "This package provides Internal helper crate for from_variants crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-from-variants-1
  (package
    (name "rust-from-variants")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "from_variants" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wmv85523z261vwmx1iqjykf0dp8fvy9kgjxj0c7cs2p427rr1af"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-from-variants-impl" ,rust-from-variants-impl-1))))
    (home-page "https://github.com/TedDriggs/from_variants")
    (synopsis
     "Rust macro to automatically generate conversions for newtype enums")
    (description
     "This package provides Rust macro to automatically generate conversions for newtype enums.")
    (license (list license:expat license:asl2.0))))

(define-public rust-enum-kinds-0.5
  (package
    (name "rust-enum-kinds")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "enum-kinds" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qnlgzlsydnxsgcf2lkvqsrmdxignjkam1fsnfd4c7b8amls2h2f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/Soft/enum-kinds")
    (synopsis
     "Generate enums with matching variants but without any of the associated data")
    (description
     "This package provides Generate enums with matching variants but without any of the associated data.")
    (license license:expat)))

(define-public rust-derive-is-enum-variant-0.1
  (package
    (name "rust-derive-is-enum-variant")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_is_enum_variant" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15w18649m4h47pdpr04id0wv8br8bg606zvrafcrfijihicqib6h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.3)
                       ("rust-quote" ,rust-quote-0.3)
                       ("rust-syn" ,rust-syn-0.11))))
    (home-page "https://github.com/fitzgen/derive_is_enum_variant")
    (synopsis
     "Automatically derives `is_dog` and `is_cat` methods for `enum Pet { Dog, Cat }`")
    (description
     "This package provides Automatically derives `is_dog` and `is_cat` methods for `enum Pet { Dog, Cat }`.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-mpris-2
  (package
    (name "rust-mpris")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mpris" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1svzblilq3kxgra63axl3wibsnl9g8p6b8q1x401wsw2lxazkkjm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dbus" ,rust-dbus-0.9)
                       ("rust-derive-is-enum-variant" ,rust-derive-is-enum-variant-0.1)
                       ("rust-enum-kinds" ,rust-enum-kinds-0.5)
                       ("rust-from-variants" ,rust-from-variants-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Mange/mpris-rs")
    (synopsis "Idiomatic MPRIS D-Bus interface library")
    (description
     "This package provides Idiomatic MPRIS D-Bus interface library.")
    (license license:asl2.0)))

(define-public rust-mpd-protocol-1
  (package
    (name "rust-mpd-protocol")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mpd_protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hmcrdyml1kzn39xiil56xdgvfkx2ldz67w62608g9rfnr1l35j0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/elomatreb/mpd_client")
    (synopsis "Implementation of MPD client protocol")
    (description
     "This package provides Implementation of MPD client protocol.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mpd-client-1
  (package
    (name "rust-mpd-client")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mpd_client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13mgkswdndxvjzjvmy8q64czrilcfn9dihcqc74fafjwgxghbhir"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-mpd-protocol" ,rust-mpd-protocol-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/elomatreb/mpd_client")
    (synopsis "Asynchronous user-friendly MPD client")
    (description
     "This package provides Asynchronous user-friendly MPD client.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mpd-utils-0.2
  (package
    (name "rust-mpd-utils")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mpd-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ljlybw28l21f55s1ymg5c3xqlq9lsd595jyz7plcsi0ndqhxrcg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.3)
                       ("rust-mpd-client" ,rust-mpd-client-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "")
    (synopsis
     "Utilities for working with MPD servers, built on top of `mpd_client` and `tokio`")
    (description
     "This package provides Utilities for working with MPD servers, built on top of `mpd_client` and
`tokio`.")
    (license license:expat)))

(define-public rust-mlua-derive-0.9
  (package
    (name "rust-mlua-derive")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mlua_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ndq9y56b4h1rkwksqalhry91ihiqqc5ray70a5gbrw8xin7ls89"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-itertools" ,rust-itertools-0.12)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/mlua-rs/mlua")
    (synopsis "Procedural macros for the mlua crate")
    (description "This package provides Procedural macros for the mlua crate.")
    (license license:expat)))

(define-public rust-luau0-src-0.12
  (package
    (name "rust-luau0-src")
    (version "0.12.3+luau663")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "luau0-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18db8dwb6cfc3bjzbfj12za3661i4gh3xkp9v2l8dgsbciy37bkn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/mlua-rs/luau-src-rs")
    (synopsis "Minimal sources of Luau and logic to build them.")
    (description
     "This package provides Minimal sources of Luau and logic to build them.")
    (license license:expat)))

(define-public rust-luajit-src-210
  (package
    (name "rust-luajit-src")
    (version "210.5.12+a4f56a4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "luajit-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wf6r910v56jm0aswh8rzjirl3z9aniaaifhckrdas2k5abfga5k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-which" ,rust-which-7))))
    (home-page "https://github.com/mlua-rs/luajit-src-rs")
    (synopsis "Sources of LuaJIT 2.1 and logic to build it.")
    (description
     "This package provides Sources of @code{LuaJIT} 2.1 and logic to build it.")
    (license license:expat)))

(define-public rust-lua-src-547
  (package
    (name "rust-lua-src")
    (version "547.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lua-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hjy276xbidvp8n0c6f1w76qamybiijfa0b7fj5rpd0p6ngg5nhy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/mlua-rs/lua-src-rs")
    (synopsis "Sources of Lua 5.1/5.2/5.3/5.4 and logic to build them.")
    (description
     "This package provides Sources of Lua 5.1/5.2/5.3/5.4 and logic to build them.")
    (license license:expat)))

(define-public rust-mlua-sys-0.6
  (package
    (name "rust-mlua-sys")
    (version "0.6.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mlua-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14wafad18hzhw7m15l54g9c7fs9l1braklsi1vsgrjlr41z1y31q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-lua-src" ,rust-lua-src-547)
                       ("rust-luajit-src" ,rust-luajit-src-210)
                       ("rust-luau0-src" ,rust-luau0-src-0.12)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/mlua-rs/mlua")
    (synopsis
     "Low level (FFI) bindings to Lua 5.4/5.3/5.2/5.1 (including LuaJIT) and Luau")
    (description
     "This package provides Low level (FFI) bindings to Lua 5.4/5.3/5.2/5.1 (including @code{LuaJIT}) and
Luau.")
    (license license:expat)))

(define-public rust-mlua-0.9
  (package
    (name "rust-mlua")
    (version "0.9.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mlua" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1iqxq7lnv7si4ydglsr73nqglfsj893ryc212lzd76wwiaqxw4fi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-erased-serde" ,rust-erased-serde-0.4)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-libloading" ,rust-libloading-0.8)
                       ("rust-mlua-sys" ,rust-mlua-sys-0.6)
                       ("rust-mlua-derive" ,rust-mlua-derive-0.9)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rustc-hash" ,rust-rustc-hash-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-value" ,rust-serde-value-0.7))))
    (home-page "https://github.com/mlua-rs/mlua")
    (synopsis
     "High level bindings to Lua 5.4/5.3/5.2/5.1 (including LuaJIT) and Luau
with async/await features and support of writing native Lua modules in Rust.")
    (description
     "This package provides High level bindings to Lua 5.4/5.3/5.2/5.1 (including @code{LuaJIT}) and Luau
with async/await features and support of writing native Lua modules in Rust.")
    (license license:expat)))

(define-public rust-lua-src-547
  (package
    (name "rust-lua-src")
    (version "547.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lua-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gwfkz4v75wscfk36bc0bnznsxix34rrlnv86fb4h8hb7v9p841a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/mlua-rs/lua-src-rs")
    (synopsis "Sources of Lua 5.1/5.2/5.3/5.4 and logic to build them.")
    (description
     "This package provides Sources of Lua 5.1/5.2/5.3/5.4 and logic to build them.")
    (license license:expat)))

(define-public rust-hyprland-macros-0.4
  (package
    (name "rust-hyprland-macros")
    (version "0.4.0-beta.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyprland-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10w9ymwh8yvrvynkcf24zykn87mdssg9mlkm242hh12ndvnwpqv9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page
     "https://github.com/hyprland-community/hyprland-rs/tree/master/hyprland-macros")
    (synopsis "Macros used in hyprland-rs")
    (description "This package provides Macros used in hyprland-rs.")
    (license license:gpl3+)))

(define-public rust-derive-more-impl-1
  (package
    (name "rust-derive-more-impl")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_more-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08mxyd456ygk68v5nfn4dyisn82k647w9ri2jl19dqpvmnp30wyb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-convert-case" ,rust-convert-case-0.6)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page "https://github.com/JelteF/derive_more")
    (synopsis "Internal implementation of `derive_more` crate")
    (description
     "This package provides Internal implementation of `derive_more` crate.")
    (license license:expat)))

(define-public rust-derive-more-1
  (package
    (name "rust-derive-more")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_more" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01cd8pskdjg10dvfchi6b8a9pa1ja1ic0kbn45dl8jdyrfwrk6sa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-derive-more-impl" ,rust-derive-more-impl-1)
                       ("rust-rustc-version" ,rust-rustc-version-0.4))))
    (home-page "https://github.com/JelteF/derive_more")
    (synopsis "Adds #[derive(x)] macros for more traits")
    (description
     "This package provides Adds #[derive(x)] macros for more traits.")
    (license license:expat)))

(define-public rust-hyprland-0.4
  (package
    (name "rust-hyprland")
    (version "0.4.0-beta.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyprland" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f03rgrgalv0l56h97s4wix4kcih1r4pjd26wjr11zghnq9i976w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-async-net" ,rust-async-net-2)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-async-stream" ,rust-async-stream-0.3)
                       ("rust-derive-more" ,rust-derive-more-1)
                       ("rust-either" ,rust-either-1)
                       ("rust-futures-lite" ,rust-futures-lite-2)
                       ("rust-hyprland-macros" ,rust-hyprland-macros-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-phf" ,rust-phf-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1))))
    (home-page "https://github.com/hyprland-community/hyprland-rs")
    (synopsis "unoffical rust wrapper for hyprland's IPC")
    (description
     "This package provides a unoffical rust wrapper for hyprland's IPC.")
    (license license:gpl3+)))

(define-public rust-gtk-layer-shell-sys-0.7
  (package
    (name "rust-gtk-layer-shell-sys")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk-layer-shell-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sx61xsp39ps6j4qn9zkhzkadn1974yrvrhdlx9nsh92w1ky1vml"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gdk-sys" ,rust-gdk-sys-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gtk-sys" ,rust-gtk-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-7))))
    (home-page
     "https://github.com/pentamassiv/gtk-layer-shell-gir/tree/main/gtk-layer-shell-sys")
    (synopsis
     "UNMAINTAINED Unsave gir-generated FFI bindings for gtk-layer-shell")
    (description
     "This package provides UNMAINTAINED Unsave gir-generated FFI bindings for gtk-layer-shell.")
    (license license:expat)))

(define-public rust-gtk-layer-shell-0.8
  (package
    (name "rust-gtk-layer-shell")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk-layer-shell" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "166awg8w36q47yi9p9xv0avhlial9c6b96jl65xm82l3hhqrnxdw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gdk" ,rust-gdk-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gtk" ,rust-gtk-0.18)
                       ("rust-gtk-layer-shell-sys" ,rust-gtk-layer-shell-sys-0.7)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page
     "https://github.com/pentamassiv/gtk-layer-shell-gir/tree/main/gtk-layer-shell")
    (synopsis "UNMAINTAINED Save gir-generated wrapper for gtk-layer-shell")
    (description
     "This package provides UNMAINTAINED Save gir-generated wrapper for gtk-layer-shell.")
    (license license:expat)))

(define-public rust-gtk3-macros-0.18
  (package
    (name "rust-gtk3-macros")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk3-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "179yszj83hgfxl4h4g2zfbsyn9a2zc5zrp6nzqv0fkzi45dkrzsj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://gtk-rs.org/")
    (synopsis
     "UNMAINTAINED Rust bindings for the GTK 3 library (use gtk4-macros instead)")
    (description
     "This package provides UNMAINTAINED Rust bindings for the GTK 3 library (use gtk4-macros instead).")
    (license license:expat)))

(define-public rust-gdk-0.18
  (package
    (name "rust-gdk")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14967h4pac5gjyrd47yls4wbicrzhbwnd4ajisfwjyk2ijalbwnr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.18)
                       ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.18)
                       ("rust-gdk-sys" ,rust-gdk-sys-0.18)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.18))))
    (home-page "https://gtk-rs.org/")
    (synopsis
     "UNMAINTAINED Rust bindings for the GDK 3 library (use gdk4 instead)")
    (description
     "This package provides UNMAINTAINED Rust bindings for the GDK 3 library (use gdk4 instead).")
    (license license:expat)))

(define-public rust-atk-0.18
  (package
    (name "rust-atk")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jw2n5xln62px4dh0hxdzbkbfraznkjakwznwhxrjbh72c9646r4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atk-sys" ,rust-atk-sys-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://gtk-rs.org/")
    (synopsis "UNMAINTAINED Rust bindings for the ATK library")
    (description
     "This package provides UNMAINTAINED Rust bindings for the ATK library.")
    (license license:expat)))

(define-public rust-gtk-0.18
  (package
    (name "rust-gtk")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sjh12mvvcmkz54nn30lb2xrzxagshbz1x2i4xfvshpwgccznmpx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atk" ,rust-atk-0.18)
                       ("rust-cairo-rs" ,rust-cairo-rs-0.18)
                       ("rust-field-offset" ,rust-field-offset-0.3)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-gdk" ,rust-gdk-0.18)
                       ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.18)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-gtk-sys" ,rust-gtk-sys-0.18)
                       ("rust-gtk3-macros" ,rust-gtk3-macros-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.18)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://gtk-rs.org/")
    (synopsis
     "UNMAINTAINED Rust bindings for the GTK+ 3 library (use gtk4 instead)")
    (description
     "This package provides UNMAINTAINED Rust bindings for the GTK+ 3 library (use gtk4 instead).")
    (license license:expat)))

(define-public rust-gensym-0.1
  (package
    (name "rust-gensym")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gensym" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10iwpbjapcdg9wjhrsc32dmzr2dcfzvhd30pzi0fmhh6bx6cwgci"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/regiontog/gensym")
    (synopsis "Creates unique identifiers for macros using procedural macros")
    (description
     "This package creates unique identifiers for macros using procedural macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-signals-0.3
  (package
    (name "rust-futures-signals")
    (version "0.3.34")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-signals" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15hnby4hw4z7dc6a2hw1m0anrszb2j3sb6y5yydxdk0d1b2fkavh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-discard" ,rust-discard-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gensym" ,rust-gensym-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Pauan/rust-signals")
    (synopsis "Zero cost FRP signals using the futures crate")
    (description
     "This package provides Zero cost FRP signals using the futures crate.")
    (license license:expat)))


(define-public ironbar
  (let ((commit "f31c1710d6b62cd85a5fe0df247221a59d2cdb38"))
    (package
      (name "ironbar")
      (version "121121211")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/JakeStanger/ironbar.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0f2lhhks857yw4hxhcpyczx1x2cb1gx8b7yspz56r1m0fvalxl2j"))))
      (build-system cargo-build-system)
      (arguments
       `(
         ;; #:imported-modules (,@%meson-build-system-modules
         ;;                     ,@%glib-or-gtk-build-system-modules
         ;;                     ,@%cargo-build-system-modules)
         ;; #:modules ((guix build cargo-build-system)
         ;;            ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
         ;;            ((guix build meson-build-system) #:prefix meson:)
         ;;            (guix build utils))
         #:phases
         (modify-phases %standard-phases
           ;; (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
           ;;   (assoc-ref glib-or-gtk:%standard-phases
           ;;              'generate-gdk-pixbuf-loaders-cache-file))
           (add-after 'unpack 'remove-optional-deps
             (lambda _
               (substitute* "Cargo.toml"

                 ((", \"workspaces\\+hyprland\"") "")
;;                 ((".*features = \\[\"silent\"\\],") "")
                 ((".*hyprland.*") "")
                 ))))
#:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.18)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-color-eyre" ,rust-color-eyre-0.6)
                       ("rust-ctrlc" ,rust-ctrlc-3)
                       ("rust-dirs" ,rust-dirs-5)
                       ("rust-futures-lite" ,rust-futures-lite-2)
                       ("rust-futures-signals" ,rust-futures-signals-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-gtk" ,rust-gtk-0.18)
                       ("rust-gtk-layer-shell" ,rust-gtk-layer-shell-0.8)
                       ("rust-hyprland" ,rust-hyprland-0.4)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-libpulse-binding" ,rust-libpulse-binding-2)
                       ("rust-lua-src" ,rust-lua-src-547)
                       ("rust-mlua" ,rust-mlua-0.9)
                       ("rust-mpd-utils" ,rust-mpd-utils-0.2)
                       ("rust-mpris" ,rust-mpris-2)
                       ("rust-nix" ,rust-nix-0.29)
                       ("rust-notify" ,rust-notify-7)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.12)
                       ("rust-schemars" ,rust-schemars-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.18)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
                       ("rust-swayipc-async" ,rust-swayipc-async-2)
                       ("rust-sysinfo" ,rust-sysinfo-0.29)
                       ("rust-system-tray" ,rust-system-tray-0.4)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-appender" ,rust-tracing-appender-0.2)
                       ("rust-tracing-error" ,rust-tracing-error-0.2)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-universal-config" ,rust-universal-config-0.5)
                       ("rust-upower-dbus" ,rust-upower-dbus-0.3)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-wayland-client" ,rust-wayland-client-0.31)
                       ("rust-wayland-protocols-wlr" ,rust-wayland-protocols-wlr-0.2)
                       ("rust-zbus" ,rust-zbus-3))))
      (native-inputs (list pkg-config))
      (inputs
       (list
        at-spi2-atk
        gtk
        glib
        gtk+
        gdk-pixbuf
        pulseaudio

        openssl
        libdbusmenu
        gtk-layer-shell
        luajit
        pango))

      (home-page "https://github.com/jakestanger/ironbar")
      (synopsis "Customisable GTK Layer Shell wlroots/sway bar")
      (description
       "This package provides Customisable GTK Layer Shell wlroots/sway bar.")
      (license license:expat))))
