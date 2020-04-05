(define-module (personal packages scan)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake))


(define-public scantailor-advanced
  (let ((commit "f6c8e5b2a2d416be224df4db600f852356ccebc3"))
    (package
     (name "scantailor-advanced")
     (version (git-version "1.0.16" "1" commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/4lex4/scantailor-advanced.git")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jchc2408gbwdcyf2qipq3y6dqi49yschlgbvgih2bgclr83kfl7"))))
     (build-system cmake-build-system)
     (arguments
      '(#:tests? #f))

     ;; (arguments
     ;;  `(#:phases
     ;;    (modify-phases %standard-phases
     ;;                   (add-before 'configure 'fixgcc7
     ;;                               (lambda _
     ;;                                 (unsetenv "C_INCLUDE_PATH")
     ;;                                 (unsetenv "CPLUS_INCLUDE_PATH"))))))
     (native-inputs `(("pkg-config" ,pkg-config)
                      ("cmake" ,cmake)
                      ("qttools" ,qttools)
                      ("qtsvg" ,qtsvg)))
                      ;;("gcc" ,gcc-7)))
     (inputs `(("boost" ,boost)
               ("libpng" ,libpng-apng)
               ("zlib" ,zlib)
               ("libjpeg" ,libjpeg)
               ("libtiff" ,libtiff)
               ("qt" ,qtbase)))
     (home-page
      "https://goodies.xfce.org/projects/panel-plugins/xfce4-sensors-plugin")
     (synopsis "3D wayland compositor")
     (description
      "")
     (license (list license:gpl2+ license:lgpl2.0+)))))
