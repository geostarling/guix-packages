;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2017, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (personal packages kodi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages kodi)
  #:use-module (personal packages tv))

(define-public kodi-with-addons/wayland
  (package
    (inherit kodi/wayland)
    (name "kodi-with-addons-wayland")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils) (guix build union))
       #:builder
       (begin
         (use-modules (guix build utils) (guix build union) (srfi srfi-26))
         (let ((kodi-input (assoc-ref %build-inputs "kodi"))
               (kodi-output (assoc-ref %outputs "out")))
           (union-build kodi-output
                        (map (lambda (input) (cdr input))
                             (filter (lambda (input)
                                       (member (car input) '("kodi" "addon")))
                                     %build-inputs))
                        #:symlink (lambda [input output]
                                    (if (file-is-directory? input)
                                        (copy-recursively input output)
                                        (copy-file input output))))
           (substitute* (string-append kodi-output "/bin/kodi")
             ((kodi-input) kodi-output))
           #t))))
    (inputs
     `(("kodi" ,kodi/wayland)
       ,@(map (lambda (addon) (list "addon" addon))
              `(,kodi-pvr-hts))))))
