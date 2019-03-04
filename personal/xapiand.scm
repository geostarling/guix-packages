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

(define-module (personal xapiand)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages file)
  #:use-module (gnu packages c)

  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages llvm)

  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages search)
  #:use-module (gnu packages textutils))

(define-public libcxxabi
  (package
   (name "libcxxabi")
   (version (package-version llvm))
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://llvm.org/releases/"
                         version "/libcxxabi-" version ".src.tar.xz"))
     (sha256
      (base32
       "1n6yx0949l9bprh75dffchahn8wplkm79ffk4f2ap9vw2lx90s41"))))
   (build-system cmake-build-system)
   (native-inputs
    `(("clang" ,clang)
      ("llvm" ,llvm)))
   (inputs
    `(("libcxx" ,(origin

                  (method url-fetch)
                  (uri (string-append "http://llvm.org/releases/"
                                      version "/libcxx-" version ".src.tar.xz"))
                  (sha256
                   (base32
                    "1wdrxg365ig0kngx52pd0n820sncp24blb0zpalc579iidhh4002"))))))
   (arguments
    `(#:configure-flags '("-DLIBCXXABI_LIBCXX_INCLUDES=../libcxxabi-7.0.1.src/libcxx-7.0.1.src/include")
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'unpack-libcxx
                              (lambda* (#:key source inputs #:allow-other-keys)
                                (invoke "tar" "xvf" (assoc-ref inputs "libcxx"))))
                     (add-after 'install 'install-headers
                                (lambda* (#:key outputs #:allow-other-keys)
                                  (let* ((out (assoc-ref outputs "out"))
                                         (include-dir (string-append out "/include")))
                                   (for-each (lambda (header-file)
                                               (install-file header-file include-dir))
                                     (find-files "../libcxxabi-7.0.1.src/include"))
                                   #t))))))


   (home-page "https://libcxxabi.llvm.org")
   (synopsis "C++ standard library support")
   (description
    "This package provides an implementation of the C++ standard library,
targeting C++11, C++14 and above.")
   (license license:expat)))

(define-public libcxx-with-abi
  (package
    (name "libcxx-with-abi")
    (version (package-version llvm))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://llvm.org/releases/"
                           version "/libcxx-" version ".src.tar.xz"))
       (sha256
        (base32
         "1wdrxg365ig0kngx52pd0n820sncp24blb0zpalc579iidhh4002"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("clang" ,clang)
       ("llvm" ,llvm)))
    (inputs
     `(("libcxxabi" ,libcxxabi)))

    (arguments
     `(#:configure-flags (list
                          "-DLIBCXX_CXX_ABI=libcxxabi"
                          (string-append "-DLIBCXX_CXX_ABI_LIBRARY_PATH="
                                         (assoc-ref %build-inputs "libcxxabi")
                                         "/lib"))
       #:phases
       (modify-phases %standard-phases
                      (add-before 'configure 'setup-clang
                                  (lambda _
                                    (setenv "CC" "clang")
                                    (setenv "CXX" "clang++")
                                    (setenv "CMAKE_CXX_COMPILER" "clang++")
                                    #t)))))
    (home-page "https://libcxx.llvm.org")
    (synopsis "C++ standard library")
    (description
     "This package provides an implementation of the C++ standard library,
targeting C++11, C++14 and above.")
    (license license:expat)))

(define-public xapian-llvm
  (package
    (name "xapian-llvm")
    (version "1.4.9")
    ;; Note: When updating Xapian, remember to update xapian-bindings below.
    (source (origin
              (method url-fetch)
              (uri (string-append "https://oligarchy.co.uk/xapian/" version
                                  "/xapian-core-" version ".tar.xz"))
              (sha256
               (base32 "1k7m7m9jld96k16ansfw2w3c354pvd8ibhnrb6dw012g06fw7sfd"))))
    (build-system gnu-build-system)
    (inputs `(("zlib" ,zlib)
              ("util-linux" ,util-linux)
              ("libcxx" ,libcxx-with-abi)
              ("libcxxabi" ,libcxxabi)))
    (native-inputs `(("clang" ,clang)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'setup-include-path
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-append (assoc-ref inputs "libcxx")
                                      "/include/c++/v1:"
                                      (or (getenv "CPLUS_INCLUDE_PATH") "")))))
         (add-before 'configure 'setup-clang
           (lambda _
             (setenv "CC" "clang")
             (setenv "CXX" "clang++")
             (setenv "CXXFLAGS" (string-append (or (getenv "CXXFLAGS") "") " -stdlib=libc++"))
             (setenv "LDFLAGS" (string-append (or (getenv "LDFLAGS") "") " -lc++abi"))
             (setenv "CLANG_DEFAULT_CXX_STDLIB" "libc++")
             (setenv "CLANG_DEFAULT_RTLIB" "compiler-rt")
             #t))
         (replace 'check
           ;; As of Xapian 1.3.3, the TCP server implementation uses
           ;; getaddrinfo(). This does not work in the build environment,
           ;; so exclude those tests. See HACKING for the list of targets.
           (lambda _
             (invoke "make"
                     "check-inmemory"
                     "check-remoteprog"
                     "check-multi"
                     "check-glass"
                     "check-chert"))))))
    (synopsis "Search Engine Library")
    (description
     "Xapian is a highly adaptable toolkit which allows developers to easily
add advanced indexing and search facilities to their own applications.  It
supports the Probabilistic Information Retrieval model and also supports a
rich set of boolean query operators.")
    (home-page "https://xapian.org/")
    (license (list license:gpl2+ license:bsd-3 license:x11))))

(define-public xapiand
    (package
      (name "xapiand")
      (version "0.10.2")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/Kronuz/Xapiand/archive/v"
                      version ".tar.gz"))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "1wbamvl6j87lgs3260rx2qr443dzzs6xi0h87rpi0k5pb7acy2j0"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags
         '("-GNinja")
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'augment-CPLUS_INCLUDE_PATH
             (lambda* (#:key build inputs #:allow-other-keys)
               (let ((libcxx (assoc-ref inputs "libcxx")))
                 (setenv "CPLUS_INCLUDE_PATH"
                         (string-append libcxx "/include/c++/v1/" ":"
                                        (or (getenv "CPLUS_INCLUDE_PATH") ""))))
               #t))
           (add-before 'configure 'setup-clang
             (lambda _
               (setenv "CC" "clang")
               (setenv "CXX" "clang++")
               (setenv "CLANG_DEFAULT_CXX_STDLIB" "libc++")
               (setenv "CLANG_DEFAULT_RTLIB" "compiler-rt")
               (setenv "CXXFLAGS" (string-append (or (getenv "CXXFLAGS") "") " -stdlib=libc++"))
               (setenv "LDFLAGS" (string-append (or (getenv "LDFLAGS") "") " -lc++abi -lLLVMSupport -ldl"))
               #t))
           (replace 'build
             (lambda _
               (invoke "ninja"
                       "-j" (number->string (parallel-job-count)))))
           (replace 'install
             (lambda _
               (invoke "ninja" "check")))
           (replace 'install
             (lambda _
               (invoke "ninja" "install")))
           (delete 'check)))) ; no test suite
      (supported-systems '("x86_64-linux" "i686-linux"))
      (inputs
       `(("zlib" ,zlib)
         ("libcxx" ,libcxx-with-abi)
         ("xapian-llvm" ,xapian-llvm)
         ("libcxxabi" ,libcxxabi)
         ("util-linux" ,util-linux)))
      (native-inputs
       `(("clang" ,clang)
         ("llvm" ,llvm)
         ("ninja" ,ninja)
         ("perl" ,perl)
         ("tcl" ,tcl)
         ("pkg-config" ,pkg-config)))
      (home-page "https://kronuz.io/Xapiand")
      (synopsis "Xapiand is a fast, simple and modern search and storage server built for the cloud.")
      (description
       "")
      (license license:x11)))
