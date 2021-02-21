;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020, 2021 James Smith <jsubuntuxp@disroot.org>
;;; Copyright © 2020, 2021 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (personal packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match))


(define-public linux-firmware-demod-si2168
  (package
   (name "linux-firmware-demod-si2168")
   (version "20191116")
   (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenELEC/dvb-firmware.git")
             (commit "3fef04a4a4bfeba88ae3b20aff9d3a1fabf1c159")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04lv3hv22r65ficrlq637jfyp8rbz9cjazvrsnv7z2q4cgz7gvbd"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils))
                  (let ((source (assoc-ref %build-inputs "source"))
                        (fw-dir (string-append %output "/lib/firmware/")))
                    (mkdir-p fw-dir)
                    (for-each (lambda (file)
                                (copy-file file
                                           (string-append fw-dir (basename file))))
                              (find-files source
                                          "dvb-demod-si2168.+\\.fw$")))
                  #t)))
   (home-page "https://wireless.wiki.kernel.org/en/users/drivers/iwlwifi")
   (synopsis "Non-free firmware for Intel wifi chips")
   (description "Non-free iwlwifi firmware")
   (license (list license:unlicense license:expat))))
