;; SPDX-FileCopyrightText: 2022-2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (personal packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system python)
  #:use-module (guix build-system go)

  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages music)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages debug)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))


(define-public libinput-minimal-1.26.2
  (package
    (inherit libinput-minimal)
    (name "libinput-minimal")
    (version "1.26.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/libinput/libinput.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zwwq7a0a6yznc6jxhp6gb50yw5vpfkvgbrabrpc5pwldpckfbrg"))))
    (native-inputs
     (modify-inputs (package-native-inputs libinput)
       (append python-minimal-wrapper python-pytest)))))

(define-public libseat-0.8.0
  (package
    (inherit libseat)
    (name "libseat")
    (version "0.8.0")
        (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~kennylevinsen/seatd")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02wzrgp8di6hqmicnm2fim6jnvbn62wy248ikvdvrhiywrb7i931"))))))


(define cairo-for-hyprland
  (package
    (inherit cairo)
    (name "cairo")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cairographics.org/releases/cairo-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0r0by563s75xyzz0d0j1nmjqmdrk2x9agk7r57p3v8vqp4v0ffi4"))))
    (build-system meson-build-system)
    (arguments
     (list #:tests? #f
           #:glib-or-gtk? #t
           #:configure-flags
           #~(list "-Dspectre=disabled")))
    (outputs '("out"))))

(define hwdata-for-hyprland
  (package
    (inherit hwdata)
    (arguments
     (substitute-keyword-arguments (package-arguments hwdata)
       ((#:phases _) #~%standard-phases)))
    (outputs '("out"))))

(define libdrm-for-hyprland
  (package
    (inherit libdrm)
    (name "libdrm")
    (version "2.4.120")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0yijzgg6rdsa68bz03sw0lcfa2nclv9m3as1cja50wkcyxim7x9v"))))))

(define udis86-for-hyprland
  (let ((revision "186")
        (commit "5336633af70f3917760a6d441ff02d93477b0c86"))
    (package
      (name "udis86")
      (version (git-version "1.7.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/canihavesomecoffee/udis86")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0y5z1169wff578jylpafsww4px4y6gickhcs885a9c660d8xs9qy"))))
      (build-system gnu-build-system)
      (native-inputs (list autoconf automake libtool python-minimal-wrapper))
      (home-page "https://github.com/canihavesomecoffee/udis86")
      (synopsis "Disassembler Library for x86 and x86-64")
      (description
       "Udis86 is a disassembler for the x86 and x86-64 class of instruction
set architectures.  It consists of a C library called @code{libudis86} and a
command line tool called @code{udcli} that incorporates the library.")
      (license license:bsd-2))))

(define wayland-protocols-for-hyprland
  (package
    (inherit wayland-protocols)
    (name "wayland-protocols")
    (version "1.36")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gitlab.freedesktop.org/wayland/wayland-protocols"
                    "/-/releases/" version "/downloads/"
                    "wayland-protocols-" version ".tar.xz"))
              (sha256
               (base32
                "14kyxywpfkgpjpkrybs28q1s2prnz30k1b4zap5a3ybrbvh4vzbi"))))))

(define-public hyprcursor
  (package
    (name "hyprcursor")
    (version "0.1.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprcursor")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s5x3nk2f48xl2z797f8s5jddmhpkg0ndw0jl7mirv9l23xmajag"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;TODO: No themes packaged.
    (inputs (list cairo-for-hyprland hyprlang librsvg libzip tomlplusplus))
    (native-inputs (list gcc-14 pkg-config))
    (home-page "https://hyprland.org/")
    (synopsis "Hyprland cursor format, library and utilities")
    (description
     "This package provides Hyprland cursor format, library and utilities.")
    (license license:bsd-3)))

(define-public aquamarine
  (package
    (name "aquamarine")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/aquamarine")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19yrwaiyh4za8d3xixjkqdif1l4r71q7rzqa05by5zc3za3vzlzw"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:cmake cmake-3.30
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (find-files "CMakeLists.txt")
                (("OpenGL::OpenGL") "OpenGL::GL")))))))
    (inputs (list
             libdrm-for-hyprland
             hwdata-for-hyprland
             libdisplay-info
             mesa
             hyprutils
             libseat-0.8.0
             pixman
             gcc-14
             hyprwayland-scanner
             libinput-minimal-1.26.2
             wayland-protocols-for-hyprland
             wayland))
    (native-inputs (list gcc-14 pkg-config))
    (home-page "https://github.com/hyprwm/hyprwayland-scanner")
    (synopsis "Hyprland implementation of wayland-scanner, in and for C++")
    (description
     "This package provides a Hyprland implementation of wayland-scanner, in and
for C++.")
    (license license:bsd-3)))


(define-public hyprutils
  (package
    (name "hyprutils")
    (version "0.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprutils")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nxx5yb5k9726x95n8gi27xyxyzwb0ma0nj3czpb51sda1k0hz0g"))))
    (build-system cmake-build-system)
    (inputs (list pixman))
    (native-inputs (list gcc-14 pkg-config))
    (home-page "https://hyprland.org/")
    (synopsis "Hyprland cursor format, library and utilities")
    (description
     "This package provides Hyprland cursor format, library and utilities.")
    (license license:bsd-3)))

(define-public hyprland-protocols
  (package
    (name "hyprland-protocols")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprland-protocols")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01j5hc8qnjzqiiwryfawx1wzrhkn0m794knphyc0vsxwkcmjaj8x"))))
    (build-system meson-build-system)
    (home-page "https://hyprland.org")
    (synopsis "Wayland protocol extensions for Hyprland")
    (description
     "This package provides Wayland protocol extensions for Hyprland and it
exists in an effort to bridge the gap between Hyprland and KDE/Gnome's
functionality.  Since @code{wlr-protocols} is closed for new submissions, and
@code{wayland-protocols} is very slow with changes, this package will hold
protocols used by Hyprland to bridge the aforementioned gap.")
    (license license:bsd-3)))


(define-public cmake-3.30
  (package
    (inherit cmake-minimal)
    (name "cmake")
    (version "3.30.3")
    (source (origin
              (inherit (package-source cmake-minimal))
              (method url-fetch)
              (uri (string-append "https://cmake.org/files/v"
                                  (version-major+minor version)
                                  "/cmake-" version ".tar.gz"))
              (sha256
               (base32
                "1r48zym4dy4mvwzk704zh1vx9gb4a910f424ypvis28mcxdy2pbd"))))

    (arguments
     (substitute-keyword-arguments (package-arguments cmake-minimal)
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'delete-help-documentation)))
      ((#:configure-flags flags #~'())
       #~(cons "-DCMake_ENABLE_DEBUGGER=OFF" #$flags))))))


(define-public hyprland
  (package
    (name "hyprland")
    (version "0.43.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/hyprwm/Hyprland"
                                  "/releases/download/v" version
                                  "/source-v" version ".tar.gz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled sources and hyprpm utility.
                  (substitute* "CMakeLists.txt"
                    (("add_subdirectory\\(hyprpm\\)") ""))
                    ;; (("add_subdirectory\\(hyprctl\\)") "")
                    ;; (("add_subdirectory\\(\\\"subprojects/udis86\\\"\\)") "")
                    ;; (("\"subprojects/udis86/\"") "")
                    ;; (("\"protocols/\"") ""))
                  (for-each delete-file-recursively
                            '("hyprpm"))))
              (sha256
               (base32
                "0v0y1ym9cssfy03qn1289fdr3b6clanxagqnchalslplyq4f6b7s"))))
    (build-system cmake-build-system)
    (arguments
     (list ;;#:configure-flags #~(list "-DLEGACY_RENDERER=ON")
      #:cmake cmake-3.30
      #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "." "CMakeLists.txt")
                     ;; (("3.27") "3.24")
                     ;; (("find_package\\(OpenGL REQUIRED COMPONENTS \\$\\{GLES_VERSION\\}\\)") "find_package(OpenGL)")
                     (("hwdata") ""))
                   (substitute* (find-files "src" "\\.cpp$")
                     (("/usr/local(/bin/Hyprland)" _ path)
                      (string-append #$output path))
                     (("/usr") #$output)
                     (("(execAndGet\\(\")\\<(cat|fc-list|lspci)\\>"
                       _ pre cmd)
                      (string-append
                       pre (search-input-file
                            inputs (string-append "bin/" cmd))))
                     (("\\<cc\\>") (search-input-file inputs "bin/gcc"))
                     ;; NOTE: Add binutils to inputs will override ld-wrapper.
                     (("(execAndGet\\(\\(\")\\<nm\\>" _ pre)
                      (string-append pre #$binutils "/bin/nm"))
                     (("\\<(addr2line|objcopy)\\>" _ cmd)
                      (string-append #$binutils "/bin/" cmd))))))))
    (native-inputs (list cmake-3.30 gcc-14 hyprwayland-scanner jq pkg-config python mesa))
    (propagated-inputs (list mesa))
    (inputs
     (list cairo-for-hyprland
           gcc-14
           aquamarine
           mesa
           wayland
           wayland-protocols
           hyprcursor
           xcb-util-errors
           xcb-util-wm
           ;;hyprland-protocols
           libinput-minimal-1.26.2
           libxkbcommon
           hyprlang
           hyprutils
           pango
           xcb-util
           tomlplusplus
           xorg-server-xwayland
           libliftoff
           libxcursor
           pciutils
           udis86-for-hyprland))
    (home-page "https://hyprland.org")
    (synopsis "Dynamic tiling Wayland compositor based on wlroots")
    (description
     "Hyprland is a dynamic tiling Wayland compositor based on @code{wlroots}
that doesn't sacrifice on its looks.  It supports multiple layouts, fancy
effects, has a very flexible IPC model allowing for a lot of customization, and
more.")
    (license license:bsd-3)))

(define-public hyprlang
  (package
    (name "hyprlang")
    (version "0.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprlang")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17i0372yv0fcwnyki36crz7afw8c5f3j985m083p7rjbh4fn3br6"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ; XXX: disable only fuzz test
    (native-inputs (list gcc-14))
    (home-page "https://hyprland.org/hyprlang/")
    (synopsis "Official implementation library for hypr config language")
    (description
     "This package provides the official implementation for hypr configuration
language used in @code{hyprland}.")
    (license license:gpl3+)))

(define-public hyprwayland-scanner
  (package
    (name "hyprwayland-scanner")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprwayland-scanner")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xc2xcxpq61lg964ihk0wbfzqqvibw20iz09g0p33ym51gwlpxr4"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;No tests.
    (inputs (list pugixml))
    (native-inputs (list gcc-14 pkg-config))
    (home-page "https://github.com/hyprwm/hyprwayland-scanner")
    (synopsis "Hyprland implementation of wayland-scanner, in and for C++")
    (description
     "This package provides a Hyprland implementation of wayland-scanner, in and
for C++.")
    (license license:bsd-3)))

(define-public grimblast
  (package
    (name "grimblast")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/contrib")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ld0sj7ahf9jf8cqzbqkhj3m2w60027ixic24ih26nwy90b5qjwx"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                ;no tests
           #:make-flags
           #~(list (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'chdir
                 (lambda _
                   (chdir "grimblast")))
               (add-after 'install 'wrap
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((grimblast (string-append #$output "/bin/grimblast")))
                     (wrap-script grimblast
                       `("PATH" suffix
                         ,(map (lambda (program)
                                 (dirname (search-input-file
                                           inputs (string-append "/bin/" program))))
                               '("grim" "slurp" "hyprctl" "wl-copy" "jq"
                                 "notify-send" "date"))))))))))
    (native-inputs (list scdoc))
    (inputs (list grim guile-3.0 jq libnotify slurp hyprland wl-clipboard))
    (home-page "https://github.com/hyprwm/contrib")
    (synopsis "Hyprland version of Grimshot")
    (description "A Hyprland version of Grimshot.")
    (license license:expat)))

(define-public xdg-desktop-portal-hyprland
  (package
    (name "xdg-desktop-portal-hyprland")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fdbzxanmmzrvb9wfzg1pdsnlg7dl6v5k8bl44w10n48s7bbbzn0"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f                  ;No tests
           #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "." "\\.cp?*$")
                     (("/bin/sh") "sh")
                     (("\\<(sh|grim|hyprctl|slurp)\\>" _ cmd)
                      (search-input-file inputs (string-append "/bin/" cmd)))
                     (("\\<(hyprland-share-picker)\\>" _ cmd)
                      (string-append #$output "/bin/" cmd))))))))
    (native-inputs
     (list gcc-14 pkg-config wayland))
    (inputs
     (list bash-minimal
           grim
           hyprland
           hyprland-protocols
           hyprlang
           mesa
           pipewire
           qtwayland
           sdbus-c++
           slurp
           wayland-protocols))
    (home-page "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
    (synopsis "XDG Desktop Portal backend for Hyprland")
    (description
     "This package provides @code{xdg-desktop-portal-hyprland}, which extends
@code{xdg-desktop-portal-wlr} for Hyprland with support for
@code{xdg-desktop-portal} screenshot and casting interfaces, while adding a few
extra portals specific to Hyprland, mostly for window sharing.")
    (license license:bsd-3)))


(define-public xdg-desktop-portal-hyprland
  (package
    (name "xdg-desktop-portal-hyprland")
    (version "1.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fdbzxanmmzrvb9wfzg1pdsnlg7dl6v5k8bl44w10n48s7bbbzn0"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f                  ;No tests
           #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "." "\\.cp?*$")
                     (("/bin/sh") "sh")
                     (("\\<(sh|grim|hyprctl|slurp)\\>" _ cmd)
                      (search-input-file inputs (string-append "/bin/" cmd)))
                     (("\\<(hyprland-share-picker)\\>" _ cmd)
                      (string-append #$output "/bin/" cmd))))))))
    (native-inputs
     (list gcc-14 pkg-config wayland))
    (inputs
     (list bash-minimal
           grim
           hyprland
           hyprland-protocols
           hyprlang
           mesa
           pipewire
           qtwayland
           sdbus-c++
           slurp
           wayland-protocols))
    (home-page "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
    (synopsis "XDG Desktop Portal backend for Hyprland")
    (description
     "This package provides @code{xdg-desktop-portal-hyprland}, which extends
@code{xdg-desktop-portal-wlr} for Hyprland with support for
@code{xdg-desktop-portal} screenshot and casting interfaces, while adding a few
extra portals specific to Hyprland, mostly for window sharing.")
    (license license:bsd-3)))




;; hyprland plugins



(define-public hyprscroller
    (let ((commit "5c01aac850c21451a5697a6fd7959424b247fe6a")
          (revision "0"))
      (package
       (name "hyprscroller")
       (version (git-version "0.0.0" revision commit))
       (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dawsers/hyprscroller")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0j6hzcv93p4cy2l1q2d6wqb7pv52p6vg4alqzkbpr7gkfv40ppc1"))))
       (build-system cmake-build-system)
       (arguments `(#:tests? #f
                    #:phases (modify-phases %standard-phases
                               (replace 'install
                                 (lambda* (#:key outputs #:allow-other-keys)
                                   (let ((lib (string-append (assoc-ref outputs "out")
                                                             "/lib")))
                                     (install-file "hyprscroller.so" lib)))))))
       (inputs (list hyprland
                     hyprlang
                     libdrm-for-hyprland
                     pixman
                     cairo-for-hyprland
                     gcc-14
                     libinput-minimal-1.26.0
                     wayland-protocols-for-hyprland
                     wayland))
       (native-inputs (list gcc-14 pkg-config))
       (home-page "https://github.com/hyprwm/hyprwayland-scanner")
       (synopsis "Hyprland implementation of wayland-scanner, in and for C++")
       (description
        "This package provides a Hyprland implementation of wayland-scanner, in and
for C++.")
       (license license:bsd-3))))



;; (define-public nwg-panel
;;   (package
;;     (name "nwg-panel")
;;     (version "0.9.32")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/nwg-piotr/nwg-panel")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32
;;          "11b34dx1j5rlh4f8h3pq3qc11y9xfmwcp2jr6jzna3c3g5hlwi7d"))))
;;     (build-system python-build-system)
;;     (arguments
;;      `(#:phases
;;        (modify-phases %standard-phases
;;          (delete 'check)
;;          (delete 'sanity-check))))
;;     (inputs (list python-pygobject
;;                   python-psutil
;;                   gtk+
;;                   gtk-layer-shell
;;                   python-requests
;;                   playerctl
;;                   python-i3ipc))
;;     (home-page "https://github.com/hykilpikonna/HyFetch")
;;     (synopsis "@code{neofetch} with pride flags <3")
;;     (description "HyFetch is a command-line system information tool fork of
;; @code{neofetch}.  HyFetch displays information about your system next to your
;; OS logo in ASCII representation.  The ASCII representation is then colored in
;; the pattern of the pride flag of your choice.  The main purpose of HyFetch is to
;; be used in screenshots to show other users what operating system or distribution
;; you are running, what theme or icon set you are using, etc.")
;;     (license license:expat)))


;; (define-public nwg-displays
;;   (package
;;     (name "nwg-displays")
;;     (version "0.3.20")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/nwg-piotr/nwg-displays")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32
;;          "13m5n3fv1f0pdal0wq5rg5xc728ylwlsl1s1iv8hf5j1iqgxr5cn"))))
;;     (build-system python-build-system)
;;     (arguments
;;      `(#:phases
;;        (modify-phases %standard-phases
;;          (delete 'check)
;;          (delete 'sanity-check))))
;;     (inputs (list python-pygobject
;;                   gtk-layer-shell
;;                   gtk+))
;;     (home-page "https://github.com/hykilpikonna/HyFetch")
;;     (synopsis "@code{neofetch} with pride flags <3")
;;     (description "HyFetch is a command-line system information tool fork of
;; @code{neofetch}.  HyFetch displays information about your system next to your
;; OS logo in ASCII representation.  The ASCII representation is then colored in
;; the pattern of the pride flag of your choice.  The main purpose of HyFetch is to
;; be used in screenshots to show other users what operating system or distribution
;; you are running, what theme or icon set you are using, etc.")
;;     (license license:expat)))


;; (define-public go-github-com-allan-simon-go-singleinstance
;;   (package
;;     (name "go-github-com-allan-simon-go-singleinstance")
;;     (version "0.0.0-20210120080615-d0997106ab37")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/allan-simon/go-singleinstance")
;;              (commit (go-version->git-ref version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "0rmkin2vjkl4qpbdcll1f4gy22xj5v83ixvbsyn78632wv0x2mqv"))))
;;     (build-system go-build-system)
;;     (arguments
;;      (list
;;       #:import-path "github.com/allan-simon/go-singleinstance"))
;;     (home-page "https://github.com/allan-simon/go-singleinstance")
;;     (synopsis "go-singleinstance")
;;     (description
;;      "Cross plateform library to have only one instance of a software(based on
;; python's
;; @@url{https://github.com/pycontribs/tendo/raw/master/tendo/singleton.py,tendo}).")
;;     (license license:expat)))

;; (define-public go-github-com-dlasky-gotk3-layershell
;;   (package
;;     (name "go-github-com-dlasky-gotk3-layershell")
;;     (version "0.0.0-20240515133811-5c5115f0d774")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/dlasky/gotk3-layershell")
;;              (commit (go-version->git-ref version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "0k7flgwmkwfq3lki4fjwx0868nxv2wa2igbqxjgqfm6150mnhmp8"))))
;;     (build-system go-build-system)
;;     (arguments
;;      (list
;;       #:import-path "github.com/dlasky/gotk3-layershell"))
;;     (propagated-inputs `(("go-github-com-gotk3-gotk3" ,go-github-com-gotk3-gotk3)))
;;     (home-page "https://github.com/dlasky/gotk3-layershell")
;;     (synopsis "gotk3-layershell")
;;     (description
;;      "This package provides a simple golang library to provide bindings for the
;; excellent @@url{https://github.com/wmww/gtk-layer-shell,Gtk Layer Shell} library
;; which can be consumed in the also excellent
;; @@url{https://github.com/gotk3/gotk3,gotk3 gtk library}.")
;;     (license license:expat)))

;; (define-public go-github-com-gotk3-gotk3
;;   (package
;;     (name "go-github-com-gotk3-gotk3")
;;     (version "0.6.3")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/gotk3/gotk3")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "1z8wfh0zz0jmpn4gxnb49364ddlabp23wvxvf7qxv14fx6waz439"))))
;;     (build-system go-build-system)
;;     (arguments
;;      (list
;;       #:import-path "github.com/gotk3/gotk3"))
;;     (home-page "https://github.com/gotk3/gotk3")
;;     (synopsis "gotk3")
;;     (description
;;      "The gotk3 project provides Go bindings for GTK 3 and dependent projects.  Each
;; component is given its own subdirectory, which is used as the import path for
;; the package.  Partial binding support for the following libraries is currently
;; implemented:")
;;     (license license:isc)))

;; (define-public go-github-com-joshuarubin-lifecycle
;;   (package
;;     (name "go-github-com-joshuarubin-lifecycle")
;;     (version "1.1.4")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/joshuarubin/lifecycle")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "1pcncj564m4isii58kbfq6ckl811jjdr5ni6b7a4600qd89l7ay5"))))
;;     (build-system go-build-system)
;;     (arguments
;;      (list
;;       #:go 1.18
;;       #:import-path "github.com/joshuarubin/lifecycle"))
;;     (propagated-inputs `(("go-golang-org-x-sync" ,go-golang-org-x-sync)))
;;     (home-page "https://github.com/joshuarubin/lifecycle")
;;     (synopsis "lifecycle")
;;     (description
;;      "@@code{lifecycle} helps manage goroutines at the application level.
;; @@code{context.Context} has been great for propagating cancellation signals, but
;; not for getting any feedback about goroutines actually finish.")
;;     (license license:expat)))

;; (define-public go-github-com-joshuarubin-go-sway
;;   (package
;;     (name "go-github-com-joshuarubin-go-sway")
;;     (version "1.2.0")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/joshuarubin/go-sway")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "1a4h34m5l2fjxp4s5ps66rjwpkvccvbqk4hr86cd2ajh127icvyv"))))
;;     (build-system go-build-system)
;;     (arguments
;;      (list
;;       #:import-path "github.com/joshuarubin/go-sway"))
;;     (propagated-inputs `(("go-go-uber-org-multierr" ,go-go-uber-org-multierr)
;;                          ("go-github-com-joshuarubin-lifecycle" ,go-github-com-joshuarubin-lifecycle)))
;;     (home-page "https://github.com/joshuarubin/go-sway")
;;     (synopsis "Differences from the i3 package")
;;     (description
;;      "This package simplifies working with the @@url{https://swaywm.org/,sway} IPC
;; from Go.  It was highly influenced by the @@url{https://github.com/i3/go-i3,i3
;; package}.")
;;     (license license:expat)))


;; (define-public nwg-drawer
;;   (package
;;     (name "nwg-drawer")
;;     (version "0.4.7")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/nwg-piotr/nwg-drawer")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32
;;          "1zfb5xmdbcq1gprrq2b77m099svkzxvwfqggvd721575p01gc5mc"))))
;;     (build-system go-build-system)
;;     (arguments
;;      (list
;;       #:go 1.22
;;       #:import-path "github.com/nwg-piotr/nwg-drawer"))

;;    ;; (arguments
;;   ;;   `(#:phases
;; ;;       (modify-phases %standard-phases
;;          ;;(delete 'check)
;;          ;;(delete 'sanity-check))))
;;     (inputs (list  go-github-com-gotk3-gotk3
;;                    go-github-com-dlasky-gotk3-layershell
;;                    go-github-com-fsnotify-fsnotify
;;                    go-github-com-joshuarubin-go-sway
;;                    go-github-com-allan-simon-go-singleinstance
;;                   go-github-com-sirupsen-logrus))
;;     (home-page "https://github.com/hykilpikonna/HyFetch")
;;     (synopsis "@code{neofetch} with pride flags <3")
;;     (description "HyFetch is a command-line system information tool fork of
;; @code{neofetch}.  HyFetch displays information about your system next to your
;; OS logo in ASCII representation.  The ASCII representation is then colored in
;; the pattern of the pride flag of your choice.  The main purpose of HyFetch is to
;; be used in screenshots to show other users what operating system or distribution
;; you are running, what theme or icon set you are using, etc.")
;;     (license license:expat)))



(define-public libliftoff
  (package
    (name "libliftoff")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.freedesktop.org/emersion/libliftoff/-/archive/v0.5.0/libliftoff-v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1cgk654crzs8b6fpv3mkv534wkji5f090dk5vam0z9rp6662229k"))))
    (build-system meson-build-system)
    (native-inputs (list gcc-14 pkg-config))
    (inputs (list libdrm))
    (home-page "https://hyprland.org/hyprlang/")
    (synopsis "Official implementation library for hypr config language")
    (description
     "This package provides the official implementation for hypr configuration
language used in @code{hyprland}.")
    (license license:gpl3+)))
