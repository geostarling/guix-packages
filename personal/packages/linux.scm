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
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages file)
  #:use-module (gnu packages c)
  #:use-module (guix utils)
  #:use-module (guix git-download)

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

(define-public linux-5.9-version "5.9.2")

(define-public linux-5.9-source
  (let ((version linux-5.9-version)
        (hash (base32 "0dh2ciyrm2ac7r4pybxa1cq3pfw3z3ilj50gdaa0clm9j7nyrx2i")))
    ((@@ (gnu packages linux) %upstream-linux-source) version hash)))


(define-public linux-5.9-source
  ((@@ (gnu packages linux)
       source-with-patches)
   linux-5.9-source
   (list (@@ (gnu packages linux) %boot-logo-patch)
         (@@ (gnu packages linux) %linux-libre-arm-export-__sync_icache_dcache-patch))))

(define-public linux-headers-5.9
  ((@@ (gnu packages linux) make-linux-libre-headers*)
   linux-5.9-version
   linux-5.9-source))

(define-public linux-5.9-vanilla
  ((@@ (gnu packages linux) make-linux-libre*)
   linux-5.9-version
   linux-5.9-source
   '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux")
   #:configuration-file (@@ (gnu packages linux) kernel-config)
   #:extra-version "vanilla"))

(define-public linux-vanilla linux-5.9-vanilla)

(define (linux-firmware-version) "20201022")
(define (linux-firmware-source version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url (string-append "https://git.kernel.org/pub/scm/linux/kernel"
                              "/git/firmware/linux-firmware.git"))
          (commit version)))
    (file-name (string-append "linux-firmware-" version "-checkout"))
    (sha256
     (base32
      "15l32993pfm0albx7ky64c5i60vph150hrdkg87md15919a6naia"))))

(define-public linux-firmware-iwlwifi
  (package
    (name "linux-firmware-iwlwifi")
    (version (linux-firmware-version))
    (source (linux-firmware-source version))
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
                                           "iwlwifi-.*\\.ucode$|LICENSE\\.iwlwifi_firmware$")))
                   #t)))
    (home-page "https://wireless.wiki.kernel.org/en/users/drivers/iwlwifi")
    (synopsis "Non-free firmware for Intel wifi chips")
    (description "Non-free iwlwifi firmware")
    (license (license:non-copyleft
              "https://git.kernel.org/cgit/linux/kernel/git/firmware/linux-firmware.git/tree/LICENCE.iwlwifi_firmware?id=HEAD"))))

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
   (license (license:non-copyleft
             "https://git.kernel.org/cgit/linux/kernel/git/firmware/linux-firmware.git/tree/LICENCE.iwlwifi_firmware?id=HEAD"))))
