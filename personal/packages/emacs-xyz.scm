;;;
;;; Copyright © 2022 Jiri Spacek <spacek@stokorec.cz>
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
  #:use-module (guix build-system emacs)
  #:use-module (guix git-download)
  #:use-module (personal packages text-editors))


(define-public emacs-parinfer-rust-mode
  (let ((commit "c2c1bbec6cc7dad4f546868aa07609b8d58a78f8")
        (revision "3"))
      (package
        (name "emacs-parinfer-rust-mode")
        (version (git-version "0.8.3" revision commit))
        (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/justinbarclay/parinfer-rust-mode.git")
                       (commit commit)))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "0az4qp118vsqzgsl87wgszzq91qzqkpabifd8qrr2li3sizsn049"))))
        (build-system emacs-build-system)
        ;; (arguments
        ;;  `(#:phases
        ;;    (modify-phases %standard-phases
        ;;      (delete 'build)
        ;;      (add-after 'unpack 'parinfer-rust-path-patch
        ;;        (lambda* (#:key inputs #:allow-other-keys)
        ;;          ;; Hard-code path to `parinfer-rust`
        ;;          (let* ((parinfer-rust-libdir (string-append (assoc-ref inputs "parinfer-rust")
        ;;                                                     "/lib/"))
        ;;                 (parinfer-rust-lib (string-append parinfer-rust-libdir "parinfer-rust-linux.so")))
        ;;            (substitute* "parinfer-rust-changes.el"
        ;;                         (("user-emacs-directory \"parinfer-rust/\"")
        ;;                          (string-append
        ;;                           "\"" parinfer-rust-libdir "\"")))
        ;;            (substitute* "parinfer-rust-mode.el"
        ;;                         (("user-emacs-directory \"parinfer-rust/\"")
        ;;                          (string-append
        ;;                           "\"" parinfer-rust-libdir "\""))
        ;;                         (("0.4.4-beta")
        ;;                          "0.4.3")
        ;;                         (("\\(locate-user-emacs-file \\(concat \"parinfer-rust/\"")
        ;;                          (string-append
        ;;                           "(concat \"" parinfer-rust-libdir "\""))
        ;;                         (("^[ ]+parinfer-rust--lib-name\\)\\)")
        ;;                          "parinfer-rust--lib-name)"))))))))
        (inputs (list parinfer-rust))
        (synopsis "Parinfer-rust-mode aims to be a simpler adaptation of Parinfer for Emacs")
        (description "")
        (home-page "https://github.com/justinbarclay/parinfer-rust-mode")
        (license license:gpl3))))
