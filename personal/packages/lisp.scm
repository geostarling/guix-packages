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

(define-module (personal packages lisp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages file)
  #:use-module (gnu packages c)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (gnu packages check)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages kodi)
  #:use-module (personal packages kodi)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages video)
  #:use-module (gnu packages python)
  #:use-module (gnu packages wget))


(define-public sbcl-vivace-graph
  (let ((commit "0859c364caf27007a0afc10a8f35a5eaa46b682d")
        (revision "1"))
    (package
      (name "sbcl-vivace-graph")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/kraison/vivace-graph-v3")
           (commit commit)))
         (file-name (git-file-name "cl-vivace-graph" version))
         (sha256
          (base32 "0jkvklh70x12ia7zbrr2pv29lf0hx2r32wrd0ndj1fpfmvy0s28d"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems '("graph-db")))
      (inputs
       (list sbcl-trivial-shell
             sbcl-iterate
             sbcl-cffi
             sbcl-osicat
             sbcl-cl-ppcre
             sbcl-uuid
             sbcl-split-sequence
             sbcl-cl-store
             sbcl-local-time
             sbcl-ieee-floats
             sbcl-cl-json
             sbcl-hunchentoot
             sbcl-ningle
             sbcl-clack
             sbcl-log4cl
             sbcl-bordeaux-threads))
      (synopsis "Lenient XML / XHTML / HTML parser for Common Lisp")
      (description
       "")
      (home-page "")
      (license license:zlib))))

(define-public cl-vivace-graph
  (sbcl-package->cl-source-package sbcl-vivace-graph))

(define-public ecl-vivace-graph
  (sbcl-package->ecl-package sbcl-vivace-graph))
