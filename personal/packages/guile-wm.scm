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

(define-module (personal packages guile-wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system dub)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)

  #:use-module (gnu packages texinfo)
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

  #:use-module (gnu packages guile)

  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages guile-wm))

(define-public custom-guile-wm
  (let ((commit "f3c7b3be719f425ffb87265d34855a73366351be")
        (revision "1"))
    (package
      (name "custom-guile-wm")
      (version (git-version "1.0" revision commit))
      (synopsis "X11 window manager toolkit in Scheme")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mwitmer/guile-wm")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "086dijnpl5dpglf70d6f9sizyakr313y7blpdjrmbi687j1x3qcl"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 rdelim)
                    (ice-9 popen))
         ;; The '.scm' files go to $(datadir), so set that to the
         ;; standard value.
         #:configure-flags (list (string-append "--datadir="
                                                (assoc-ref %outputs "out")
                                                "/share/guile/site/2.2"))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'set-module-directory
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Install .scm files to $out/share/guile/site/2.2.
               (let ((out (assoc-ref outputs "out"))
                     (effective (read-line
                                 (open-pipe* OPEN_READ
                                             "guile" "-c"
                                             "(display (effective-version))"))))
                 (substitute* "module/Makefile.in"
                   (("^wmdir = .*$")
                    (string-append "wmdir = " out
                                   "/share/guile/site/"
                                   effective "\n"))))
               #t))
           (add-after 'install 'set-load-path
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Put Guile-XCB's and Guile-WM's modules in the
               ;; search path of PROG.
               (let* ((out       (assoc-ref outputs "out"))
                      (effective (read-line
                                  (open-pipe* OPEN_READ
                                              "guile" "-c"
                                              "(display (effective-version))")))
                      (prog      (string-append out "/bin/guile-wm"))
                      (mods      (string-append out "/share/guile/site/" effective))
                      (gos       (string-append out "/lib/guile/" effective "/site-ccache"))
                      (xcb       (assoc-ref inputs "guile-xcb")))
                 (wrap-program prog
                   `("GUILE_AUTO_COMPILE" ":" = ("0"))
                   `("GUILE_LOAD_PATH" ":" prefix
                     (,mods ,(string-append xcb "/share/guile/site/" effective)))
                   `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                     (,gos ,(string-append xcb "/lib/guile/"
                                           effective "/site-ccache")))))
               #t))
           (add-after 'install 'install-go-files
             (lambda* (#:key outputs inputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (effective (read-line
                                  (open-pipe* OPEN_READ
                                              "guile" "-c"
                                              "(display (effective-version))")))
                      (module-dir (string-append out "/share/guile/site/"
                                                 effective))
                      (object-dir (string-append out "/lib/guile/" effective
                                                 "/site-ccache"))
                      (prefix     (string-length module-dir)))
                 (setenv "GUILE_AUTO_COMPILE" "0")
                 ;; compile to the destination
                 (for-each (lambda (file)
                             (let* ((base (string-drop (string-drop-right file 4)
                                                       prefix))
                                    (go   (string-append object-dir base ".go")))
                               (invoke "guild" "compile" "-L" module-dir
                                       file "-o" go)))
                           (find-files module-dir "\\.scm$"))
                 #t)))
           (add-after 'install 'install-xsession
             (lambda* (#:key outputs #:allow-other-keys)
               ;; add a .desktop file to xsessions
               (let ((xsessions (string-append
                                 %output "/share/xsessions")))
                 (mkdir-p xsessions)
                 (call-with-output-file (string-append
                                         xsessions "/guile-wm.desktop")
                   (lambda (port)
                     (format port
                             "[Desktop Entry]~@
                                    Name=~a~@
                                    Comment=~a~@
                                    Exec=~a/bin/guile-wm -l /tmp/guile-wm.log~@
                                    Type=Application~%"
                             ,name ,synopsis %output))))
               #t)))))
      (native-inputs `(("pkg-config" ,pkg-config)
                       ("texinfo" ,texinfo)))
      (inputs `(("guile" ,guile-2.2)
                ("guile-xcb" ,guile-xcb)))
      (home-page "https://github.com/mwitmer/guile-wm/releases")
      (description
       "Guile-WM is a simple window manager that's completely customizable—you
have total control of what it does by choosing which modules to include.
Included with it are a few modules that provide basic TinyWM-like window
management, some window record-keeping, multi-monitor support, and emacs-like
keymaps and minibuffer.  At this point, it's just enough to get you started.")
      (license license:gpl3+))))
