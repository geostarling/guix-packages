;; SPDX-FileCopyrightText: 2022-2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (personal packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))


(define-public libinput-minimal-1.24.0
  (package
    (inherit libinput-minimal)
    (name "libinput-minimal")
    (version "1.24.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/libinput/libinput.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xk0dljykjfmkks7kjxvbia6g3wadmy7lihfygm8icywkq8j0dw1"))))
    (native-inputs
     (modify-inputs (package-native-inputs libinput)
       (append python-minimal-wrapper python-pytest)))))


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

(define wlroots-for-hyprland
  (let ((base wlroots)
        (revision "1")
        (commit "5c1d51c5a2793480f5b6c4341ad0797052aec2ea"))
    (package
      (inherit base)
      (name "wlroots")
      (version (git-version "0.18.0-dev-hyprland" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hyprwm/wlroots-hyprland")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0gy0g0kxb3q1wjqrypnvmrxcb4ld3advikchygpwpfp4s9v0mmvd"))))
      (arguments
       (substitute-keyword-arguments (package-arguments wlroots)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'adjust-patching-script
                (lambda _
                  (substitute* "patches/apply.sh"
                    (("apply \\|\\| check_applied \\|\\| fail")
                     "patch -Np1 < $PATCH"))))))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs wlroots)
         (prepend libdrm-for-hyprland)
         (replace "libinput-minimal" libinput-minimal-1.24.0)
         (replace "wayland-protocols" wayland-protocols-for-hyprland)))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "hwdata" `(,hwdata-for-hyprland "out")))))))

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
    (native-inputs (list gcc-13 pkg-config))
    (home-page "https://hyprland.org/")
    (synopsis "Hyprland cursor format, library and utilities")
    (description
     "This package provides Hyprland cursor format, library and utilities.")
    (license license:bsd-3)))

(define-public hyprland-protocols
  (package
    (name "hyprland-protocols")
    (version "0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprland-protocols")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1drjznj7fn6m5m6skhzh0p031cb5x0bb4i56jxnxwpwaa71g1z20"))))
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

(define hyprland-unbundle-wlroots-patch
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/hyprwm/Hyprland" "/raw/"
                        "cba1ade848feac44b2eda677503900639581c3f4"
                        "/nix/patches/meson-build.patch"))
    (sha256
     (base32 "0fwvsshz3k6061p3hdl175pydmj80vnw5rm4xfcn64w1ssfq7liw"))))

(define-public hyprland
  (package
    (name "hyprland")
    (version "0.40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/hyprwm/Hyprland"
                                  "/releases/download/v" version
                                  "/source-v" version ".tar.gz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled sources and hyprpm utility.
                  (substitute* "meson.build"
                    ((".*hyprpm/src.*") ""))
                  (for-each delete-file-recursively
                            '("hyprpm"
                              "subprojects"))))
              (patches (list hyprland-unbundle-wlroots-patch))
              (sha256
               (base32
                "0f4hs8qzmfmg4lr491b2inanb02xn4xa0gwb8a0ks3m64iwzx589"))))
    (build-system meson-build-system)
    (arguments
     (list #:build-type "release"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
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
    (native-inputs (list gcc-13 hyprwayland-scanner jq pkg-config))
    (inputs
     (list cairo-for-hyprland
           gcc-13
           hyprcursor
           hyprland-protocols
           hyprlang
           pango
           pciutils
           udis86-for-hyprland
           wlroots-for-hyprland))
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
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprlang")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f8mahg6d6wylybvh6hgayls57miwwv4w69fbaskd8d7dkg2h7kd"))))
    (build-system cmake-build-system)
    (native-inputs (list gcc-13))
    (home-page "https://hyprland.org/hyprlang/")
    (synopsis "Official implementation library for hypr config language")
    (description
     "This package provides the official implementation for hypr configuration
language used in @code{hyprland}.")
    (license license:gpl3+)))

(define-public hyprwayland-scanner
  (package
    (name "hyprwayland-scanner")
    (version "0.3.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprwayland-scanner")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mi0kawfw311ybiy4xipy1n20nvjphkfqzgnd8jxxbkhjkwn0jhg"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;No tests.
    (inputs (list pugixml))
    (native-inputs (list gcc-13 pkg-config))
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
     (list gcc-13 pkg-config wayland))
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
     (list gcc-13 pkg-config wayland))
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
                     gcc-13
                     libinput-minimal-1.24.0
                     wayland-protocols-for-hyprland
                     wayland
                     wlroots-for-hyprland))
       (native-inputs (list gcc-13 pkg-config))
       (home-page "https://github.com/hyprwm/hyprwayland-scanner")
       (synopsis "Hyprland implementation of wayland-scanner, in and for C++")
       (description
        "This package provides a Hyprland implementation of wayland-scanner, in and
for C++.")
       (license license:bsd-3))))



;; (define-public niri
;;   (let ((commit "4fefab7d6b8298cfcd1a022b9c1ceeb039ca8e63")
;;         (revision "0"))
;;     (package
;;       (name "niri")
;;       (version (git-version "0.0.0" revision commit))
;;       (source
;;        (origin
;;          (method git-fetch)
;;          (uri (git-reference
;;                (url "https://github.com/YaLTeR/niri")
;;                (commit commit)))
;;          (file-name (git-file-name name version))
;;          (sha256
;;           (base32 "11n6mwwp28msbm2qf8j4mb1jgn3z1bqqx4m5i4rxqw6hvva72hqn"))))
;;       (build-system cargo-build-system)
;;       ;; sudo dnf install gcc libudev-devel libgbm-devel libxkbcommon-devel wayland-devel libinput-devel dbus-devel systemd-devel libseat-devel pipewire-devel clang
;;       (arguments
;;        `(#:cargo-inputs
;;          (("rust-serde" ,rust-serde-1))
;;          #:phases
;;          (modify-phases %standard-phases
;;            (add-after 'unpack 'adjust-feature-flags
;;              (lambda _
;;                (substitute* "Cargo.toml"
;;                  ((".*line-tables-only.*") "")))))))
;;       (native-inputs
;;        (list gcc))
;;       (home-page "https://github.com/Revertron/Alfis")
;;       (synopsis "Alternative Free Identity System")
;;       (description
;;        "This project represents a minimal blockchain without cryptocurrency,
;; capable of sustaining any number of domain names in a bunch of original
;; alternative zones.")
;;       (license license:agpl3+))))
