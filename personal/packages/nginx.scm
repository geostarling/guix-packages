;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2013, 2015, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 Alexander I.Grafov <grafov@gmail.com>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 xd1le <elisp.vim@gmail.com>
;;; Copyright © 2015 Florian Paul Schmidt <mista.tapas@gmx.net>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Marek Benc <dusxmt@gmx.com>
;;; Copyright © 2017 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2018 Thomas Sigurdsen <tonton@riseup.net>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Nam Nguyen <namn@berkeley.edu>
;;; Copyright © 2019 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2019 Kyle Andrews <kyle.c.andrews@gmail.com>
;;; Copyright © 2019 Josh Holland <josh@inv.alid.pw>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
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

(define-module (personal packages nginx)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages web)
  #:use-module (ice-9 match))



(define-public nginx-rtmp-module
  (let ((commit "23ec4ce2d769830124abf3be1353dd9b105ab09c"))
    (package
     (inherit nginx)
     (name "nginx-rtmp-module")
     (version "1.1.7.10")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/sergey-dryabzhinsky/nginx-rtmp-module")
              (commit commit)))
        (file-name (git-file-name "nginx-rtmp-module" version))
        (sha256
         (base32
          "1xqph5n6chq13wiz3yr7lbjm2pars740av7nn94nrjfm9zzcccqy"))))
     (build-system gnu-build-system)
     (inputs
      `(("nginx-sources" ,(package-source nginx))
        ,@(package-inputs nginx)))
     (arguments
      (substitute-keyword-arguments
       `(#:configure-flags '("--add-dynamic-module=./src")
         #:make-flags '("modules")
         #:modules ((guix build utils)
                    (guix build gnu-build-system)
                    (ice-9 popen)
                    (ice-9 regex)
                    (ice-9 textual-ports))
         ,@(package-arguments nginx))
        ((#:phases phases)
         `(modify-phases
           ,phases
           (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (format #t "XXX ~s~%" source)
                      (mkdir "source")
                      (mkdir "source/src")
                      (chdir "source")
                      (copy-recursively source "src"
                                        #:keep-mtime? #t)
                      #t))
           (add-after 'unpack 'unpack-nginx-sources
             (lambda* (#:key inputs native-inputs #:allow-other-keys)
               (begin
                 ;; The nginx source code is part of the module’s source.
                 (format #t "decompressing nginx source code~%")
                 (let ((tar (assoc-ref inputs "tar"))
                       (nginx-srcs (assoc-ref inputs "nginx-sources")))
                   (invoke (string-append tar "/bin/tar")
                           "xvf" nginx-srcs "--strip-components=1"))
                 #t)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((modules-dir (string-append (assoc-ref outputs "out")
                                                 "/etc/nginx/modules")))
                 (install-file "objs/ngx_rtmp_module.so" modules-dir)
                 #t)))
           (delete 'fix-root-dirs)
           (delete 'install-man-page)))))
     (synopsis "NGINX module for Lua programming language support")
     (description "This NGINX module provides a scripting support with Lua
programming language."))))
