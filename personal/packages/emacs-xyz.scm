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

(define-module (personal packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (guix build utils)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages file)
  #:use-module (gnu packages c)

  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gnome)      ;; for vte and libpeas
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages llvm)

  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages textutils)
  #:use-module (guix git-download))


(define-public emacs-parinfer-rust-mode
  (let ((commit "ca9e7b6f8c3c70daf6a933952955b6931a24af83")
        (revision "3"))
      (package
        (name "emacs-parinfer-rust-mode")
        (version "0.8.2")
        (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/justinbarclay/parinfer-rust-mode.git")
                       (commit (string-append "v" version))))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "1d9885l1aifrdrg6c4m2kakhs3bbmfmsm02q96j0k0mzzwr7rs41"))))
        (build-system emacs-build-system)
        (arguments
         `(#:phases
           (modify-phases %standard-phases
             (delete 'build)
             (add-after 'unpack 'parinfer-rust-path-patch
               (lambda* (#:key inputs #:allow-other-keys)
                 ;; Hard-code path to `parinfer-rust`
                 (let* ((parinfer-rust-libdir (string-append (assoc-ref inputs "parinfer-rust")
                                                            "/lib/"))
                        (parinfer-rust-lib (string-append parinfer-rust-libdir "parinfer-rust-linux.so")))
                   (substitute* "parinfer-rust-changes.el"
                                (("user-emacs-directory \"parinfer-rust/\"")
                                 (string-append
                                  "\"" parinfer-rust-libdir "\"")))
                   (substitute* "parinfer-rust-mode.el"
                                (("user-emacs-directory \"parinfer-rust/\"")
                                 (string-append
                                  "\"" parinfer-rust-libdir "\""))
                                (("0.4.4-beta")
                                 "0.4.3")
                                (("\\(locate-user-emacs-file \\(concat \"parinfer-rust/\"")
                                 (string-append
                                  "(concat \"" parinfer-rust-libdir "\""))
                                (("^[ ]+parinfer-rust--lib-name\\)\\)")
                                 "parinfer-rust--lib-name)"))))))))
        (propagated-inputs
         `(("parinfer-rust" ,parinfer-rust)))
        (synopsis "GNU Emacs client for the Telegram messenger")
        (description
         "")
        (home-page "https://github.com/zevlg/telega.el")
        (license license:gpl3+))))

(define-public emacs-selected
  (package
    (name "emacs-selected")
    (version "0.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Kungsgeten/selected.el.git")
             (commit "03edaeac90bc6000d263f03be3d889b4685e1bf7")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d72vw1dcxnyir7vymr3cfxal5dndm1pmm192aa9bcyrcg7aq39g"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/emacscollective/no-littering")
    (synopsis "Help keep ~/.emacs.d/ clean")
    (description "The default paths used to store configuration files and
persistent data are not consistent across Emacs packages, be them built-in or
third-party ones.  @code{no-littering} sets out to help clean
@file{~/.emacs.d/} by putting configuration files and persistent data files in
two user-defined directories, as well as using more descriptive names for
files and subdirectories when appropriate.")
    (license license:gpl3+)))


(define-public emacs-plz
  (let ((commit "7e456638a651bab3a814e3ea81742dd917509cbb")
        (revision "1")
        (version "0.1-pre"))
    (package
      (name "emacs-plz")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alphapapa/plz.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "05kgxrps1s20im5hhq799nrs3615bvssm4r0ysgmwm203mmzsjgj"))))
      (build-system emacs-build-system)
      (home-page
       "https://github.com/alphapapa/plz.el")
      (synopsis "GNU Emacs HTTP library")
      (description #f)
      (license license:gpl3+))))


(define-public emacs-ement
  (let ((commit "3c14f6c4c10f994218689d372bf1fdbe714ac529")
        (revision "1")
        (version "0.1-pre"))
    (package
      (name "emacs-ement")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alphapapa/ement.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0hvyaia1hacm1kxgd5gvac8m7wxp62qwyf1gjng5sg5rpa0ppavz"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-plz" ,emacs-plz)
         ("emacs-ts" ,emacs-ts)))
      (home-page
       "https://github.com/alphapapa/ement.el")
      (synopsis "GNU Emacs client for Matrix")
      (description #f)
      (license license:gpl3+))))
