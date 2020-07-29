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

(define-module (personal packages display-managers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system cargo)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
;  #:use-module (guix build utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages autotools)
  #:use-module (personal packages crates-io)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (ice-9 match)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages xorg))

(define-public lightdm-with-vala
  (package
    (inherit lightdm)
    (name "lightdm-with-vala")
    (version "1.30.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/CanonicalLtd/lightdm/releases/download/"
                                 version "/lightdm-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "158zb2d0v1309a8v19hh32y4yj3v6yg4yg6m0l7v59d3a2b7f651"))))
    (arguments
     (substitute-keyword-arguments (package-arguments lightdm)
      ((#:configure-flags flags ''())
       `(cons* "--enable-vala"
               ,flags))
      ((#:phases phases)
       `(modify-phases ,phases
                       (delete 'check)))))
    (inputs
     (cons `("vala" ,vala)
           (package-inputs lightdm)))))

(define-public lightdm-gtk-greeter-with-vala
 (package
   (inherit lightdm-gtk-greeter)
   (name "lightdm-gtk-greeter-with-vala")
   (version "2.0.7")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://launchpad.net/lightdm-gtk-greeter/"
                   (version-major+minor version) "/" version
                   "/+download/lightdm-gtk-greeter-" version ".tar.gz"))
             (sha256
              (base32
               "1g7wc3d3vqfa7mrdhx1w9ywydgjbffla6rbrxq9k3sc62br97qms"))))
   (inputs
    `(("lightdm" ,lightdm-with-vala)
      ("gtk+" ,gtk+)))))



(define-public greetd
  (package
    (name "greetd")
    (version "0.6.1")
    (source
      (origin
       (method url-fetch)
       (uri (string-append
              "https://git.sr.ht/~kennylevinsen/greetd/archive/" version ".tar.gz"))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1gf8z83f0fz1dj4j3177qg86n20qr4laavjr0nf4vc5lgnq6pq4a"))))
    (build-system cargo-build-system)
    (arguments
     `(
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-cargo-toml
          (lambda _
            (substitute* "Cargo.toml"
              ((", path =.*}") "}"))
            #t)))
         ;; (add-after 'unpack 'chdir
         ;;            (lambda _
         ;;              (chdir "greetd/")
         ;;              #t))
         ;; (add-after 'configure 'cwchdir
         ;;            (lambda _
         ;;              (display (getcwd))
         ;;              #t)))
       #:cargo-inputs

       (;;("rust-clap" ,rust-clap-2)
        ;; ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.4)
        ;; ("rust-dirs" ,rust-dirs-2.0)
        ;; ("rust-encoding-rs-io" ,rust-encoding-rs-io-0.1)
        ;; ("rust-env-logger" ,rust-env-logger-0.7)
        ;; ("rust-grep-searcher" ,rust-grep-searcher-0.1)
        ;; ("rust-hex" ,rust-hex-0.4)
        ;; ("rust-ignore" ,rust-ignore-0.4)
        ;; ("rust-log" ,rust-log-0.4)
        ;; ("rust-rayon" ,rust-rayon-1)
        ("rust-async-trait" ,rust-async-trait)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1))))

        ;; ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ;; ("rust-term-size" ,rust-term-size-0.3)
        ;; ("rust-toml" ,rust-toml-0.5))
       ;; #:cargo-development-inputs
       ;; (("rust-async-trait" ,rust-async-trait))))

    ;; (native-inputs
    ;;  `(("libgit2" ,libgit2)
    ;;    ("openssl" ,openssl)
    ;;    ("pkg-config" ,pkg-config)
    ;;    ("zlib" ,zlib)))
    (inputs
     `(("libgit2" ,libgit2)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://tokei.rs")
    (synopsis "Count code, quickly")
    (description
     "Tokei is a program that displays statistics about your code.  Tokei will
show number of files, total lines within those files and code, comments, and
blanks grouped by language.")
    (license (list license:expat license:asl2.0))))
