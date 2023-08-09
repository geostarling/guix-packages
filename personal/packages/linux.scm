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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match))


(define-public dvb-demod-si2168-firmware
  (package
   (name "dvb-demod-si2168-firmware")
   (version "1.4.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/CoreELEC/dvb-firmware/archive/refs/tags/"
                  version ".tar.gz"))
            (sha256
             (base32
              "1hz2kxlgd4n2s4777zmr3g9cpf2nmllbsqiz1113fd4fr3lyacc8"))))
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
