;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2017, 2020-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016-2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Guy Fleury Iteriteka <hoonandon@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020, 2021, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021, 2022, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2021, 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Dion Mendel <guix@dm9.info>
;;; Copyright © 2021 Andrew Whatson <whatson@gmail.com>
;;; Copyright © 2021 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2022 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2022, 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2023 Juliana Sims <juli@incana.org>
;;; Copyright © 2023 Ahmad Draidi <a.r.draidi@redscript.org>
;;; Copyright © 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023, 2024 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (personal packages virtualization)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages apparmor)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages augeas)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages java)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages build-tools)
  #:use-module (personal packages cluster)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cluster)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages figlet)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public lxc-6
  (package
   (name "lxc")
   (version "6.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://linuxcontainers.org/downloads/lxc/lxc-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1q3p3zzm338pmc97z6ly8cjginkyljxqbk1c37l2xa46vfy8zcyc"))))
   (build-system meson-build-system)
   (native-inputs
    (list pkg-config docbook2x))
   (inputs
    (list gnutls libcap libseccomp libselinux))
   ;;    LXC_DISTRO_SYSCONF
   (arguments
    (list #:configure-flags
          #~(list "-Ddistrosysconfdir=/tmp/etc"
                  "-Dinit-script=[]"
                  "-Ddbus=false"
                  "-Dinstall-state-dirs=false"
                  ;;"-Dinstall-init-files=false"
                  )))
   (synopsis "Linux container tools")
   (home-page "https://linuxcontainers.org/")
   (description
    "LXC is a userspace interface for the Linux kernel containment features.
Through a powerful API and simple tools, it lets Linux users easily create and
manage system or application containers.")
   (license license:lgpl2.1+)))


(define-public incus
  (package
   (name "incus")
   (version "6.4.0")
   (source (origin
            (method url-fetch)
            ;;https://github.com/lxc/incus/releases/download/v6.3.0/incus-6.3.tar.xz
            (uri (string-append
                  "https://github.com/lxc/incus/releases/download/v" version "/incus-" (version-major+minor version) ".tar.xz"))
            (sha256
             (base32
              "1rzfxlh45smwnpqjaj5lv41ikpky7xli7jiqgj0cssq698gscj37"))))
   (build-system go-build-system)
   (arguments
    `(#:go ,go-1.21
      #:import-path "github.com/lxc/incus"
      #:tests? #f ;; tests fail due to missing /var, cgroups, etc.
      #:modules ((guix build go-build-system)
                 (guix build union)
                 (guix build utils)
                 (srfi srfi-1))
      #:phases
      (modify-phases %standard-phases
                     ;; (add-after 'unpack 'unpack-dist
                     ;;   (lambda* (#:key import-path #:allow-other-keys)
                     ;;     (with-directory-excursion (string-append "src/" import-path)
                     ;;       ;; Move all the dependencies into the src directory.
                     ;;       (copy-recursively "_dist/src" "../../.."))))
                     (replace 'build
                              (lambda* (#:key import-path #:allow-other-keys)
                                       (with-directory-excursion (string-append "src/" import-path)
                                                                 (invoke "make" "build" "CC=gcc" "TAG_SQLITE3=libsqlite3"))))
                     (replace 'check
                              (lambda* (#:key tests? import-path #:allow-other-keys)
                                       (when tests?
                                         (with-directory-excursion (string-append "src/" import-path)
                                                                   (invoke "make" "check" "CC=gcc" "TAG_SQLITE3=libsqlite3")))))
                     (replace 'install
                              (lambda* (#:key inputs outputs import-path #:allow-other-keys)
                                       (let* ((out (assoc-ref outputs "out"))
                                              (bin-dir
                                               (string-append out "/bin/"))
                                              (doc-dir
                                               (string-append out "/share/doc/incus-" ,version))
                                              (completions-dir
                                               (string-append out "/share/bash-completion/completions")))
                                         (with-directory-excursion (string-append "src/" import-path)
                                                                   ;; Wrap lxd with run-time dependencies.
                                                                   (wrap-program (string-append bin-dir "incus")
                                                                                 `("PATH" ":" prefix
                                                                                   ,(fold (lambda (input paths)
                                                                                            ;; TODO: Use 'search-input-directory' rather
                                                                                            ;; than look up inputs by name.
                                                                                            (let* ((in (assoc-ref inputs input))
                                                                                                   (bin (string-append in "/bin"))
                                                                                                   (sbin (string-append in "/sbin")))
                                                                                              (append (filter file-exists?
                                                                                                              (list bin sbin)) paths)))
                                                                                          '()
                                                                                          '("bash-minimal" "acl" "rsync" "tar" "xz" "btrfs-progs"
                                                                                            "gzip" "dnsmasq" "squashfs-tools" "iproute2"
                                                                                            "iptables" "attr"))))
                                                                   (wrap-program (string-append bin-dir "incusd")
                                                                                 `("PATH" ":" prefix
                                                                                   ,(fold (lambda (input paths)
                                                                                            ;; TODO: Use 'search-input-directory' rather
                                                                                            ;; than look up inputs by name.
                                                                                            (let* ((in (assoc-ref inputs input))
                                                                                                   (bin (string-append in "/bin"))
                                                                                                   (sbin (string-append in "/sbin")))
                                                                                              (append (filter file-exists?
                                                                                                              (list bin sbin)) paths)))
                                                                                          '()
                                                                                          '("bash-minimal" "acl" "rsync" "tar" "xz" "btrfs-progs"
                                                                                            "gzip" "dnsmasq" "squashfs-tools" "iproute2"
                                                                                            "iptables" "attr"))))
                                                                   ;; Remove unwanted binaries.
                                                                   ;; (for-each (lambda (prog)
                                                                   ;;             (delete-file (string-append bin-dir prog)))
                                                                   ;;           '("deps" "macaroon-identity" "generate"))
                                                                   ;; Install documentation.
                                                                   (for-each (lambda (file)
                                                                               (install-file file doc-dir))
                                                                             (find-files "doc"))
                                                                   ;; Install bash completion.
                                                                   ;; (rename-file "scripts/bash/incus-client" "scripts/bash/incus")
                                                                   ;; (install-file "scripts/bash/incus" completions-dir)
                                                                   )))))))
   (native-inputs
    (list ;; Test dependencies:
     ;; ("go-github-com-rogpeppe-godeps" ,go-github-com-rogpeppe-godeps)
     ;; ("go-github-com-tsenart-deadcode" ,go-github-com-tsenart-deadcode)
     ;; ("go-golang-org-x-lint" ,go-golang-org-x-lint)
     pkg-config))
   (inputs
    (list acl
          eudev
          libcowsql
          cowsql-libraft
          libcap
          lxc-6
          ;; Run-time dependencies.
          attr
          bash-minimal
          rsync
          tar
          xz
          btrfs-progs
          gzip
          dnsmasq
          squashfs-tools
          iproute
          iptables))
   (synopsis "Daemon based on liblxc offering a REST API to manage containers")
   (home-page "https://linuxcontainers.org/lxd/")
   (description "LXD is a next generation system container manager.  It
offers a user experience similar to virtual machines but using Linux
containers instead.  It's image based with pre-made images available for a
wide number of Linux distributions and is built around a very powerful, yet
pretty simple, REST API.")
   (license license:asl2.0)))

(define-public libguestfs
  (package
   (name "libguestfs")
   (version "1.52.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://download.libguestfs.org/1.52-stable/libguestfs-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1m5cg654yp0n36zhcbrnhm0k3jrl2gxd52a8700jbivnsl2jswhz"))))
   (build-system gnu-build-system)
   (native-inputs
    (list pkg-config docbook2x))
   (inputs
    (list gnutls libcap libseccomp libselinux))
   (synopsis "Linux container tools")
   (home-page "https://linuxcontainers.org/")
   (description
    "LXC is a userspace interface for the Linux kernel containment features.
Through a powerful API and simple tools, it lets Linux users easily create and
manage system or application containers.")
   (license license:lgpl2.1+)))


(define-public libguestfs-minimal
  (package
   (name "libguestfs-minimal")
   (version "1.53.6")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://libguestfs.org/download/"
                                (version-major+minor version)
                                "-stable/libguestfs-" version ".tar.gz"))
            (sha256
             (base32
              "0vssarc3n4kv26fyjmkrrcvh55v41fhycba43pij3rc2izl72s2y"))
            (patches
             (search-patches "libguestfs-syms.patch"))))
   (build-system gnu-build-system)
   (arguments
    (list #:configure-flags
          #~(list "--disable-appliance"
                  "--disable-daemon"
                  "--disable-static"
                  "--with-distro=\"Guix System\"")
          #:make-flags #~'("REALLY_INSTALL=yes")
          #:phases
          #~(let* ((lib/ocaml (string-append #$output "/lib/ocaml")))
              (modify-phases %standard-phases
                             (add-after 'unpack 'patch-makefiles
                                        (lambda _
                                          (for-each patch-shebang
                                                    (find-files "."))
                                          (for-each (lambda (makefile)
                                                      (substitute* "ocaml/Makefile.am"
                                                                   (("\\$\\(DESTDIR\\)\\$\\(OCAMLLIB\\)")
                                                                    lib/ocaml)))
                                                    '("ocaml/Makefile.am"
                                                      "ocaml/Makefile.in"))))
                             (replace 'bootstrap
                                      (lambda _
                                        (invoke "autoreconf" "-vif")))
                             (replace 'check
                                      (lambda* (#:key tests? make-flags #:allow-other-keys)
                                               (when tests?
                                                 (apply invoke `("make" ,@make-flags "check-direct")))))
                             (add-before 'install 'set-rpath
                                         (lambda _
                                           (for-each
                                            (lambda (sofile)
                                              (invoke "ls" "-la" "ocaml/")
                                              ;; The ‘validate-runpath’ phase fails to find libguestfs.so.0.
                                              (invoke (string-append #$(this-package-native-input "patchelf")
                                                                     "/bin/patchelf")
                                                      "--add-rpath" (string-append #$output "/lib") sofile))
                                            '("perl/blib/arch/auto/Sys/Guestfs/Guestfs.so"
                                              "ocaml/dllmlguestfs.so"))))
                             (replace 'install
                                      (lambda* (#:key make-flags #:allow-other-keys)
                                               (mkdir-p "temp-build-dir")
                                               (apply invoke `("make" ,@make-flags "INSTALLDIRS=vendor"
                                                               "install"))))
                             (add-after 'install 'wrap-binaries
                                        (lambda _
                                          (let ((bin (string-append #$output "/bin")))
                                            (for-each
                                             (lambda (binary)
                                               (use-modules (srfi srfi-1))
                                               (wrap-program binary
                                                             `("PERL5LIB" ":" prefix
                                                               (,(string-append #$output
                                                                                "/lib/perl5/site_perl")))
                                                             `("PATH" ":" prefix
                                                               ,(search-path-as-list
                                                                 '("bin")
                                                                 (map second
                                                                      '#$(package-inputs this-package))))))
                                             (find-files bin)))))
                             (replace 'validate-documentation-location
                                      (lambda _
                                        (let ((man-dir
                                               (string-append #$output "/man"))
                                              (info-dir
                                               (string-append #$output "/info")))
                                          (for-each (lambda (d)
                                                      (invoke "rm" "-rf" d))
                                                    (list man-dir info-dir)))))))))
   (native-inputs (list autoconf
                        automake
                        augeas
                        bison
                        cpio
                        flex
                        gettext-minimal
                        gperf
                        libtool
                        ocaml
                        ocaml-findlib
                        ncurses
                        patchelf
                        perl
                        perl-getopt-long
                        perl-module-build
                        pkg-config
                        po4a
                        xorriso
                        xz
                        zstd))
   (inputs
    (list file
          fuse
          jansson
          hivex
          libtirpc
          pcre2
          readline
          qemu))
   (home-page "https://libguestfs.org/")
   (synopsis "Access and modify virtual machine disk images")
   (description
    "@code{libguestfs} is a set of tools for accessing and modifying virtual
machine (VM) disk images.  You can use this for viewing and editing files inside
guests, scripting changes to VMs, monitoring disk used/free statistics, creating
guests, P2V, V2V, performing backups, cloning VMs, building VMs, formatting
disks, resizing disks, and much more.")
   (license (list license:gpl2+ license:lgpl2.1+))))



(define-public hivex
  (package
   (name "hivex")
   (version "1.3.24")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://libguestfs.org/download/"
                                name "/" name "-" version ".tar.gz"))
            (sha256
             (base32
              "0g0rib62qg81fda8lxsaa7a1ykqy4rl5sq185pdqm9y9xifa8bx5"))))
   (build-system gnu-build-system)
   (native-inputs (list automake
                        autoconf
                        gettext-minimal
                        libtool
                        ocaml
                        patchelf
                        pkg-config
                        perl-io-stringy
                        python-wrapper
                        ruby
                        ruby-rake
                        ruby-rdoc))
   (inputs
    (list bash-minimal
          libxml2
          perl
          readline))
   (arguments
    (list
     #:configure-flags
     #~(list
        "--disable-static"
        "--with-readline"
        "--disable-rpath")
     #:phases
     #~(modify-phases %standard-phases
                      (replace 'bootstrap
                               (lambda _
                                 (invoke "autoreconf" "-vif")))
                      (add-after 'unpack 'patch-makefiles
                                 (lambda _
                                   (let* ((current-system (or #$(%current-target-system)
                                                              #$(%current-system)))
                                          (ocamllib
                                           (string-append #$output "/lib/ocaml/"
                                                          #$(package-version
                                                             (this-package-native-input "ocaml")) "/site-lib"))
                                          (python-installdir
                                           (string-append #$output "/lib/python"
                                                          #$(version-major+minor
                                                             (package-version
                                                              (this-package-native-input
                                                               "python-wrapper")))
                                                          "/site-packages"))
                                          (ruby-version
                                           #$(package-version
                                              (this-package-native-input "ruby")))
                                          (ruby-libdir
                                           (string-append #$output
                                                          "/lib/ruby/site_ruby/"
                                                          ruby-version))
                                          (ruby-archdir
                                           (string-append ruby-libdir "/" current-system)))
                                     (substitute* "lib/Makefile.am"
                                                  (((string-append "\\$\\(VERSION_SCRIPT_FLAGS\\)"
                                                                   "\\$\\(srcdir\\)/hivex\\.syms"))
                                                   ""))
                                     (substitute* "python/Makefile.am"
                                                  (("\\$\\(PYTHON_INSTALLDIR\\)")
                                                   python-installdir))
                                     (substitute* "ocaml/Makefile.am"
                                                  (("\\$\\(DESTDIR\\)\\$\\(OCAMLLIB\\)")
                                                   ocamllib))
                                     (substitute* "ruby/Makefile.am"
                                                  (("\\$\\(DESTDIR\\)\\$\\(RUBY_ARCHDIR\\)")
                                                   ruby-archdir)
                                                  (("\\$\\(DESTDIR\\)\\$\\(RUBY_LIBDIR\\)")
                                                   ruby-libdir)))))
                      (add-before 'install 'set-rpath
                                  (lambda _
                                    (for-each
                                     (lambda (sofile)
                                       ;; The ‘validate-runpath’ phase fails to find libhivex.so.0.
                                       (invoke (string-append #$(this-package-native-input "patchelf")
                                                              "/bin/patchelf")
                                               "--add-rpath" (string-append #$output "/lib") sofile))
                                     '("perl/blib/arch/auto/Win/Hivex/Hivex.so"
                                       "ruby/ext/hivex/_hivex.so"))))
                      (add-after 'install 'wrap-binaries
                                 (lambda _
                                   (let ((hivexregedit
                                          (string-append #$output "/bin/hivexregedit"))
                                         (hivexml
                                          (string-append #$output "/bin/hivexml")))
                                     (wrap-program hivexregedit
                                                   `("PERL5LIB" ":" prefix
                                                     (,(string-append #$output "/lib/perl5/site_perl")))
                                                   `("PATH" ":" prefix
                                                     (,(string-append #$output "/bin"))))
                                     (wrap-program hivexml
                                                   `("PATH" ":" prefix
                                                     (,(string-append #$output "/bin"))))))))))
   (home-page "https://github.com/libguestfs/hivex")
   (synopsis "Windows registry hive extraction library")
   (description
    "This package provides a self-contained library for reading and writing
Windows Registry \"hive\" binary files.  Unlike many other tools in this area,
it doesn't use the textual @code{.REG} format for output, because parsing that
is as much trouble as parsing the original binary format.  Instead it makes the
file available through a C API, or through a separate program to export the
hive as XML.")
   (license license:lgpl2.1)))

(define-public libguestfs
  (package
   (inherit libguestfs-minimal)
   (name "libguestfs")
   (arguments
    (substitute-keyword-arguments (package-arguments libguestfs-minimal)
                                  ((#:configure-flags flags)
                                   #~(cons (string-append "--with-python-installdir="
                                                          #$output "/lib/python"
                                                          #$(version-major+minor
                                                             (package-version python))
                                                          "/site-packages")
                                           #$flags))))
   (native-inputs
    (modify-inputs (package-native-inputs libguestfs-minimal)
                   (prepend cdrtools
                            gobject-introspection
                            lua
                            `(,openjdk "jdk")
                            python
                            util-linux)))
   (inputs
    (modify-inputs (package-inputs libguestfs-minimal)
                   (prepend acl
                            bdb
                            gmp
                            libapparmor
                            libcap
                            libcap-ng
                            libconfig
                            libvirt
                            libxcrypt
                            numactl
                            yajl)))))
