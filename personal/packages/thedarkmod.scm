;;; GNU Guix --- Functional package management for GNU
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

(define-module (personal packages thedarkmod)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system scons)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (srfi srfi-26))

(define-public darkmod
  (package
    (name "thedarkmod")
    (version "2.0.7")
    (source
     (origin
      (method svn-fetch)
      (uri (svn-reference
            (url "https://svn.thedarkmod.com/publicsvn/darkmod_src/tags/2.07hotfix/")
            (revision 8483)))
      (file-name (string-append "thedarkmod-" version "-checkout"))
      (sha256
       (base32
        "0ksqnp459xmclgl853zpsgzh4xwj0xcq1sybxjk28cpl619vj6c0"))))
    (build-system scons-build-system)
    (arguments
     `(#:scons ,scons-python2
       #:scons-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                           "TARGET_ARCH=x64")
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
                      (delete 'install))))
       ;; ;;   (add-after 'unpack 'patch-resource-locations
       ;; ;;     (lambda* (#:key outputs #:allow-other-keys)
       ;; ;;       (substitute* "source/Files.cpp"
       ;; ;;         (("/usr/local/")
       ;; ;;          (string-append (assoc-ref outputs "out") "/")))
       ;; ;;       #t))
       ;;   (add-after 'unpack 'patch-scons
       ;;     (lambda _
       ;;       (substitute* "SConstruct"
       ;;         ;; Keep environmental variables
       ;;         (("Environment\\(\\)")
       ;;          "Environment(ENV = os.environ)")
       ;;         ;; Install into %out/bin
       ;;         (("games\"") "bin\""))
       ;;       #t))))
    (native-inputs
     `(("m4" ,m4)
       ("subversion" ,subversion)))
    (inputs
     `(("mesa" ,mesa)
       ("openal" ,openal)))
    (home-page "https://endless-sky.github.io/")
    (synopsis "2D space trading and combat game")
    (description "Endless Sky is a 2D space trading and combat game.  Explore
other star systems.  Earn money by trading, carrying passengers, or completing
missions.  Use your earnings to buy a better ship or to upgrade the weapons and
engines on your current one.  Blow up pirates.  Take sides in a civil war.  Or
leave human space behind and hope to find friendly aliens whose culture is more
civilized than your own.")
    (license (list license:gpl3+
                   license:cc-by-sa3.0
                   license:cc-by-sa4.0
                   license:public-domain))))
