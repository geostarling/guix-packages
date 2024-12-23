;; SPDX-FileCopyrightText: 2022-2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (personal packages wm)
  #:use-module ((guix licenses) #:prefix license:)
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages music)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages golang)
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
