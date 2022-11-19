;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2016 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017, 2018, 2020, 2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2017 Feng Shu <tumashu@163.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2014 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.org>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2019, 2020, 2021, 2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Tom Zander <tomz@freedommail.ch>
;;; Copyright © 2020 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2020 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 aecepoglu <aecepoglu@fastmail.fm>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Calum Irwin <calumirwin1@gmail.com>
;;; Copyright © 2022 Luis Henrique Gomes Higino <luishenriquegh2701@gmail.com>
;;; Copyright © 2022 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2022 jgart <jgart@dismail.de>
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

(define-module (personal packages text-editors)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages llvm))

(define-public parinfer-rust
  (let ((commit "7e99d3afa830dd5ef1566ce9ba78c79835251bb4")
        (revision "1"))
    (package
     (name "parinfer-rust")
     (version "0.4.3")
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/geostarling/parinfer-rust.git")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zjv4glm7l2q008cbd41y724x39ywngsm5kvql3w03aylzyi3v04"))))

     (build-system cargo-build-system)
     (arguments
      `(#:cargo-inputs
        (("rust-getopts" ,rust-getopts-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-emacs" ,rust-emacs-0.18)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-serde-derive" ,rust-serde-derive-1)
         ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
         ("rust-unicode-width" ,rust-unicode-width-0.1))
        #:phases
        ;; clang is required by rust-emacs-module
        (modify-phases %standard-phases
                       (add-after 'install 'install-so
                                  (lambda* (#:key inputs outputs #:allow-other-keys)
                                           (let ((lib-target (string-append (assoc-ref outputs "out")
                                                                            "/lib/")))
                                             (mkdir-p lib-target)
                                             (copy-file
                                              "target/release/libparinfer_rust.so"
                                              (string-append lib-target "parinfer-rust-linux.so"))
                                             #t))))))
     (inputs
      (list clang))
     (home-page "https://github.com/justinbarclay/parinfer-rust")
     (synopsis "Infer parentheses for Clojure, Lisp and Scheme")
     (description
      "Parinfer is a plugin for Kakoune, Vim, Neovim and Emacs that infers
parentheses and indentation.  This library can be called from other editors that
can load dynamic libraries.")
     (license license:expat))))
