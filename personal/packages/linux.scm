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

(define-module (personal packages linux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system dub)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
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
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gnome)      ;; for vte and libpeas
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xorg)

  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages textutils))


(define-public linux-5.3-source
  (let ((version linux-libre-5.3-version)
        (hash (base32 "0vi4bgcr921z5l6fbcrcgmhaji5gl2avpmp7njna6v0f7sxism0r")))
    ((@@ (gnu packages linux) %upstream-linux-source) version hash)))


(define-public linux-5.3-source
  ((@@ (gnu packages linux)
       source-with-patches)
   linux-5.3-source
   (list (search-patch "linux-libre-active-entropy.patch")
         (@@ (gnu packages linux) %boot-logo-patch)
         (@@ (gnu packages linux) %linux-libre-arm-export-__sync_icache_dcache-patch))))


(define-public linux-headers-5.3
  ((@@ (gnu packages linux) make-linux-libre-headers*)
   linux-libre-5.3-version
   linux-5.3-source))


(define-public linux-5.3-vanilla
  ((@@ (gnu packages linux) make-linux-libre*)
   linux-libre-5.3-version
   linux-5.3-source
   '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux")
   #:configuration-file (@@ (gnu packages linux) kernel-config)
   #:extra-version "vanilla"))


(define-public linux-vanilla linux-5.3-vanilla)
