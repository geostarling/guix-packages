(define-module (personal packages lisp-xyz)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19))


(define sbcl-cl-ana-boot0
  (let ((commit "df0c862d0d517a9d6e740382529e253043fb682e")
        (revision "3"))
    (package
     (name "sbcl-cl-ana-boot0")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ghollisjr/cl-ana.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "185i5nvw8aav0dx603lzdgfxxqkwn2g230jv7fgz75ygipnhb52n"))))
     (build-system asdf-build-system/sbcl)
     (synopsis "Common Lisp data analysis library")
     (description
      "CL-ANA is a data analysis library in Common Lisp providing tabular and
binned data analysis along with nonlinear least squares fitting and
visualization.")
     (home-page "https://github.com/ghollisjr/cl-ana")
     (license license:gpl3))))

(define-public sbcl-cl-ana.pathname-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.pathname-utils")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "pathname-utils/cl-ana.pathname-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.pathname-utils")))))

(define-public cl-ana.pathname-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.pathname-utils))

(define-public ecl-cl-ana.pathname-utils
  (sbcl-package->ecl-package sbcl-cl-ana.pathname-utils))

(define-public sbcl-cl-ana.package-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.package-utils")
    (inputs
     `(("alexandria" ,sbcl-alexandria)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "package-utils/cl-ana.package-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.package-utils")))))

(define-public cl-ana.package-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.package-utils))

(define-public ecl-cl-ana.package-utils
  (sbcl-package->ecl-package sbcl-cl-ana.package-utils))

(define-public sbcl-cl-ana.string-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.string-utils")
    (inputs
     `(("split-sequence" ,sbcl-split-sequence)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "string-utils/cl-ana.string-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.string-utils")))))

(define-public cl-ana.string-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.string-utils))

(define-public ecl-cl-ana.string-utils
  (sbcl-package->ecl-package sbcl-cl-ana.string-utils))

(define-public sbcl-cl-ana.functional-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.functional-utils")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "functional-utils/cl-ana.functional-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.functional-utils")))))

(define-public cl-ana.functional-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.functional-utils))

(define-public ecl-cl-ana.functional-utils
  (sbcl-package->ecl-package sbcl-cl-ana.functional-utils))

(define-public sbcl-cl-ana.list-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.list-utils")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "list-utils/cl-ana.list-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.list-utils")))))

(define-public cl-ana.list-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.list-utils))

(define-public ecl-cl-ana.list-utils
  (sbcl-package->ecl-package sbcl-cl-ana.list-utils))

(define-public sbcl-cl-ana.generic-math
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.generic-math")
    (inputs
     `(("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.package-utils" ,sbcl-cl-ana.package-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "generic-math/cl-ana.generic-math.asd")
       ((#:asd-system-name _ #f) "cl-ana.generic-math")))))

(define-public cl-ana.generic-math
  (sbcl-package->cl-source-package sbcl-cl-ana.generic-math))

(define-public ecl-cl-ana.generic-math
  (sbcl-package->ecl-package sbcl-cl-ana.generic-math))

(define-public sbcl-cl-ana.math-functions
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.math-functions")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("gsll" ,sbcl-gsll)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "math-functions/cl-ana.math-functions.asd")
       ((#:asd-system-name _ #f) "cl-ana.math-functions")))))

(define-public cl-ana.math-functions
  (sbcl-package->cl-source-package sbcl-cl-ana.math-functions))

(define-public sbcl-cl-ana.calculus
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.calculus")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "calculus/cl-ana.calculus.asd")
       ((#:asd-system-name _ #f) "cl-ana.calculus")))))

(define-public cl-ana.calculus
  (sbcl-package->cl-source-package sbcl-cl-ana.calculus))

(define-public ecl-cl-ana.calculus
  (sbcl-package->ecl-package sbcl-cl-ana.calculus))

(define-public sbcl-cl-ana.symbol-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.symbol-utils")
    (inputs
     `(("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "symbol-utils/cl-ana.symbol-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.symbol-utils")))))

(define-public cl-ana.symbol-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.symbol-utils))

(define-public ecl-cl-ana.symbol-utils
  (sbcl-package->ecl-package sbcl-cl-ana.symbol-utils))

(define-public sbcl-cl-ana.macro-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.macro-utils")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("split-sequence" ,sbcl-split-sequence)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "macro-utils/cl-ana.macro-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.macro-utils")))))

(define-public cl-ana.macro-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.macro-utils))

(define-public ecl-cl-ana.macro-utils
  (sbcl-package->ecl-package sbcl-cl-ana.macro-utils))

(define-public sbcl-cl-ana.binary-tree
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.binary-tree")
    (inputs
     `(("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "binary-tree/cl-ana.binary-tree.asd")
       ((#:asd-system-name _ #f) "cl-ana.binary-tree")))))

(define-public cl-ana.binary-tree
  (sbcl-package->cl-source-package sbcl-cl-ana.binary-tree))

(define-public ecl-cl-ana.binary-tree
  (sbcl-package->ecl-package sbcl-cl-ana.binary-tree))

(define-public sbcl-cl-ana.tensor
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.tensor")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "tensor/cl-ana.tensor.asd")
       ((#:asd-system-name _ #f) "cl-ana.tensor")))))

(define-public cl-ana.tensor
  (sbcl-package->cl-source-package sbcl-cl-ana.tensor))

(define-public ecl-cl-ana.tensor
  (sbcl-package->ecl-package sbcl-cl-ana.tensor))

(define-public sbcl-cl-ana.error-propogation
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.error-propogation")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "error-propogation/cl-ana.error-propogation.asd")
       ((#:asd-system-name _ #f) "cl-ana.error-propogation")))))

(define-public cl-ana.error-propogation
  (sbcl-package->cl-source-package sbcl-cl-ana.error-propogation))

(define-public sbcl-cl-ana.quantity
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.quantity")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "quantity/cl-ana.quantity.asd")
       ((#:asd-system-name _ #f) "cl-ana.quantity")))))

(define-public cl-ana.quantity
  (sbcl-package->cl-source-package sbcl-cl-ana.quantity))

(define-public sbcl-cl-ana.table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "table/cl-ana.table.asd")
       ((#:asd-system-name _ #f) "cl-ana.table")))))

(define-public cl-ana.table
  (sbcl-package->cl-source-package sbcl-cl-ana.table))

(define-public ecl-cl-ana.table
  (sbcl-package->ecl-package sbcl-cl-ana.table))

(define-public sbcl-cl-ana.table-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.table-utils")
    (inputs
     `(("cl-ana.statistics" ,sbcl-cl-ana.statistics)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "table-utils/cl-ana.table-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.table-utils")))))

(define-public cl-ana.table-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.table-utils))

(define-public ecl-cl-ana.table-utils
  (sbcl-package->ecl-package sbcl-cl-ana.table-utils))

(define-public sbcl-cl-ana.hdf-cffi
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.hdf-cffi")
    (inputs
     `(("cffi" ,sbcl-cffi)
       ("hdf5" ,hdf5-parallel-openmpi)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "hdf-cffi/cl-ana.hdf-cffi.asd")
       ((#:asd-system-name _ #f) "cl-ana.hdf-cffi")
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "hdf-cffi/hdf-cffi.lisp"
                 (("/usr/lib/i386-linux-gnu/hdf5/serial/libhdf5.so")
                  (string-append
                   (assoc-ref inputs "hdf5")
                   "/lib/libhdf5.so")))))))))))

(define-public cl-ana.hdf-cffi
  (sbcl-package->cl-source-package sbcl-cl-ana.hdf-cffi))

(define-public ecl-cl-ana.hdf-cffi
  (sbcl-package->ecl-package sbcl-cl-ana.hdf-cffi))

(define-public sbcl-cl-ana.int-char
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.int-char")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "int-char/cl-ana.int-char.asd")
       ((#:asd-system-name _ #f) "cl-ana.int-char")))))

(define-public cl-ana.int-char
  (sbcl-package->cl-source-package sbcl-cl-ana.int-char))

(define-public ecl-cl-ana.int-char
  (sbcl-package->ecl-package sbcl-cl-ana.int-char))

(define-public sbcl-cl-ana.memoization
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.memoization")
    (inputs
     `(("alexandria" ,sbcl-alexandria)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "memoization/cl-ana.memoization.asd")
       ((#:asd-system-name _ #f) "cl-ana.memoization")))))

(define-public cl-ana.memoization
  (sbcl-package->cl-source-package sbcl-cl-ana.memoization))

(define-public ecl-cl-ana.memoization
  (sbcl-package->ecl-package sbcl-cl-ana.memoization))

(define-public sbcl-cl-ana.typespec
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.typespec")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cffi" ,sbcl-cffi)
       ("cl-ana.int-char" ,sbcl-cl-ana.int-char)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "typespec/cl-ana.typespec.asd")
       ((#:asd-system-name _ #f) "cl-ana.typespec")))))

(define-public cl-ana.typespec
  (sbcl-package->cl-source-package sbcl-cl-ana.typespec))

(define-public ecl-cl-ana.typespec
  (sbcl-package->ecl-package sbcl-cl-ana.typespec))

(define-public sbcl-cl-ana.hdf-typespec
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.hdf-typespec")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cffi" ,sbcl-cffi)
       ("cl-ana.hdf-cffi" ,sbcl-cl-ana.hdf-cffi)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "hdf-typespec/cl-ana.hdf-typespec.asd")
       ((#:asd-system-name _ #f) "cl-ana.hdf-typespec")))))

(define-public cl-ana.hdf-typespec
  (sbcl-package->cl-source-package sbcl-cl-ana.hdf-typespec))

(define-public ecl-cl-ana.hdf-typespec
  (sbcl-package->ecl-package sbcl-cl-ana.hdf-typespec))

(define-public sbcl-cl-ana.hdf-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.hdf-utils")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cffi" ,sbcl-cffi)
       ("cl-ana.hdf-cffi" ,sbcl-cl-ana.hdf-cffi)
       ("cl-ana.hdf-typespec" ,sbcl-cl-ana.hdf-typespec)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.pathname-utils" ,sbcl-cl-ana.pathname-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "hdf-utils/cl-ana.hdf-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.hdf-utils")))))

(define-public cl-ana.hdf-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.hdf-utils))

(define-public ecl-cl-ana.hdf-utils
  (sbcl-package->ecl-package sbcl-cl-ana.hdf-utils))

(define-public sbcl-cl-ana.typed-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.typed-table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "typed-table/cl-ana.typed-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.typed-table")))))

(define-public cl-ana.typed-table
  (sbcl-package->cl-source-package sbcl-cl-ana.typed-table))

(define-public ecl-cl-ana.typed-table
  (sbcl-package->ecl-package sbcl-cl-ana.typed-table))

(define-public sbcl-cl-ana.hdf-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.hdf-table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.binary-tree" ,sbcl-cl-ana.binary-tree)
       ("cl-ana.hdf-cffi" ,sbcl-cl-ana.hdf-cffi)
       ("cl-ana.hdf-typespec" ,sbcl-cl-ana.hdf-typespec)
       ("cl-ana.hdf-utils" ,sbcl-cl-ana.hdf-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("cl-ana.typed-table" ,sbcl-cl-ana.typed-table)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "hdf-table/cl-ana.hdf-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.hdf-table")))))

(define-public cl-ana.hdf-table
  (sbcl-package->cl-source-package sbcl-cl-ana.hdf-table))

(define-public ecl-cl-ana.hdf-table
  (sbcl-package->ecl-package sbcl-cl-ana.hdf-table))

(define-public sbcl-cl-ana.gsl-cffi
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.gsl-cffi")
    (inputs
     `(("cffi" ,sbcl-cffi)
       ("gsl" ,gsl)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "gsl-cffi/cl-ana.gsl-cffi.asd")
       ((#:asd-system-name _ #f) "cl-ana.gsl-cffi")
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "gsl-cffi/gsl-cffi.lisp"
                 (("define-foreign-library gsl-cffi" all)
                  (string-append all " (:unix "
                                 (assoc-ref inputs "gsl")
                                 "/lib/libgsl.so)")))))))))))

(define-public cl-ana.gsl-cffi
  (sbcl-package->cl-source-package sbcl-cl-ana.gsl-cffi))

(define-public ecl-cl-ana.gsl-cffi
  (sbcl-package->ecl-package sbcl-cl-ana.gsl-cffi))

(define-public sbcl-cl-ana.ntuple-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.ntuple-table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cffi" ,sbcl-cffi)
       ("cl-ana.gsl-cffi" ,sbcl-cl-ana.gsl-cffi)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("cl-ana.typed-table" ,sbcl-cl-ana.typed-table)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)
       ("gsll" ,sbcl-gsll)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "ntuple-table/cl-ana.ntuple-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.ntuple-table")))))

(define-public cl-ana.ntuple-table
  (sbcl-package->cl-source-package sbcl-cl-ana.ntuple-table))

(define-public sbcl-cl-ana.csv-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.csv-table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("antik" ,sbcl-antik)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("cl-csv" ,sbcl-cl-csv)
       ("iterate" ,sbcl-iterate)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "csv-table/cl-ana.csv-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.csv-table")))))

(define-public cl-ana.csv-table
  (sbcl-package->cl-source-package sbcl-cl-ana.csv-table))

(define-public sbcl-cl-ana.reusable-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.reusable-table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.table" ,sbcl-cl-ana.table)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "reusable-table/cl-ana.reusable-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.reusable-table")))))

(define-public cl-ana.reusable-table
  (sbcl-package->cl-source-package sbcl-cl-ana.reusable-table))

(define-public ecl-cl-ana.reusable-table
  (sbcl-package->ecl-package sbcl-cl-ana.reusable-table))

(define-public sbcl-cl-ana.linear-algebra
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.linear-algebra")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)
       ("gsll" ,sbcl-gsll)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "linear-algebra/cl-ana.linear-algebra.asd")
       ((#:asd-system-name _ #f) "cl-ana.linear-algebra")))))

(define-public cl-ana.linear-algebra
  (sbcl-package->cl-source-package sbcl-cl-ana.linear-algebra))

(define-public sbcl-cl-ana.lorentz
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.lorentz")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.linear-algebra" ,sbcl-cl-ana.linear-algebra)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)
       ("iterate" ,sbcl-iterate)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "lorentz/cl-ana.lorentz.asd")
       ((#:asd-system-name _ #f) "cl-ana.lorentz")))))

(define-public cl-ana.lorentz
  (sbcl-package->cl-source-package sbcl-cl-ana.lorentz))

(define-public sbcl-cl-ana.clos-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.clos-utils")
    (inputs
     `(("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)
       ("closer-mop" ,sbcl-closer-mop)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "clos-utils/cl-ana.clos-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.clos-utils")))))

(define-public cl-ana.clos-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.clos-utils))

(define-public ecl-cl-ana.clos-utils
  (sbcl-package->ecl-package sbcl-cl-ana.clos-utils))

(define-public sbcl-cl-ana.hash-table-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.hash-table-utils")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "hash-table-utils/cl-ana.hash-table-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.hash-table-utils")))))

(define-public cl-ana.hash-table-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.hash-table-utils))

(define-public ecl-cl-ana.hash-table-utils
  (sbcl-package->ecl-package sbcl-cl-ana.hash-table-utils))

(define-public sbcl-cl-ana.map
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.map")
    (inputs
     `(("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "map/cl-ana.map.asd")
       ((#:asd-system-name _ #f) "cl-ana.map")))))

(define-public cl-ana.map
  (sbcl-package->cl-source-package sbcl-cl-ana.map))

(define-public ecl-cl-ana.map
  (sbcl-package->ecl-package sbcl-cl-ana.map))

(define-public sbcl-cl-ana.fitting
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.fitting")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)
       ("gsll" ,sbcl-gsll)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "fitting/cl-ana.fitting.asd")
       ((#:asd-system-name _ #f) "cl-ana.fitting")))))

(define-public cl-ana.fitting
  (sbcl-package->cl-source-package sbcl-cl-ana.fitting))

(define-public sbcl-cl-ana.histogram
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.histogram")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("iterate" ,sbcl-iterate)
       ("cl-ana.binary-tree" ,sbcl-cl-ana.binary-tree)
       ("cl-ana.clos-utils" ,sbcl-cl-ana.clos-utils)
       ("cl-ana.fitting" ,sbcl-cl-ana.fitting)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "histogram/cl-ana.histogram.asd")
       ((#:asd-system-name _ #f) "cl-ana.histogram")))))

(define-public cl-ana.histogram
  (sbcl-package->cl-source-package sbcl-cl-ana.histogram))

(define-public sbcl-cl-ana.file-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.file-utils")
    (inputs
     `(("external-program" ,sbcl-external-program)
       ("split-sequence" ,sbcl-split-sequence)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "file-utils/cl-ana.file-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.file-utils")))))

(define-public cl-ana.file-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.file-utils))

(define-public ecl-cl-ana.file-utils
  (sbcl-package->ecl-package sbcl-cl-ana.file-utils))

(define-public sbcl-cl-ana.statistics
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.statistics")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.map" ,sbcl-cl-ana.map)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "statistics/cl-ana.statistics.asd")
       ((#:asd-system-name _ #f) "cl-ana.statistics")))))

(define-public cl-ana.statistics
  (sbcl-package->cl-source-package sbcl-cl-ana.statistics))

(define-public sbcl-cl-ana.gnuplot-interface
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.gnuplot-interface")
    (inputs
     `(("external-program" ,sbcl-external-program)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "gnuplot-interface/cl-ana.gnuplot-interface.asd")
       ((#:asd-system-name _ #f) "cl-ana.gnuplot-interface")))))

(define-public cl-ana.gnuplot-interface
  (sbcl-package->cl-source-package sbcl-cl-ana.gnuplot-interface))

(define-public ecl-cl-ana.gnuplot-interface
  (sbcl-package->ecl-package sbcl-cl-ana.gnuplot-interface))

(define-public sbcl-cl-ana.plotting
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.plotting")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.gnuplot-interface" ,sbcl-cl-ana.gnuplot-interface)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)
       ("cl-ana.pathname-utils" ,sbcl-cl-ana.pathname-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)
       ("external-program" ,sbcl-external-program)
       ("split-sequence" ,sbcl-split-sequence)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "plotting/cl-ana.plotting.asd")
       ((#:asd-system-name _ #f) "cl-ana.plotting")))))

(define-public cl-ana.plotting
  (sbcl-package->cl-source-package sbcl-cl-ana.plotting))

(define-public sbcl-cl-ana.table-viewing
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.table-viewing")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.plotting" ,sbcl-cl-ana.plotting)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "table-viewing/cl-ana.table-viewing.asd")
       ((#:asd-system-name _ #f) "cl-ana.table-viewing")))))

(define-public cl-ana.table-viewing
  (sbcl-package->cl-source-package sbcl-cl-ana.table-viewing))

(define-public sbcl-cl-ana.serialization
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.serialization")
    (inputs
     `(("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.hdf-utils" ,sbcl-cl-ana.hdf-utils)
       ("cl-ana.hdf-table" ,sbcl-cl-ana.hdf-table)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.int-char" ,sbcl-cl-ana.int-char)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "serialization/cl-ana.serialization.asd")
       ((#:asd-system-name _ #f) "cl-ana.serialization")))))

(define-public cl-ana.serialization
  (sbcl-package->cl-source-package sbcl-cl-ana.serialization))

(define-public sbcl-cl-ana.makeres
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.file-utils" ,sbcl-cl-ana.file-utils)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)
       ("cl-ana.hdf-utils" ,sbcl-cl-ana.hdf-utils)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.pathname-utils" ,sbcl-cl-ana.pathname-utils)
       ("cl-ana.plotting" ,sbcl-cl-ana.plotting)
       ("cl-ana.reusable-table" ,sbcl-cl-ana.reusable-table)
       ("cl-ana.serialization" ,sbcl-cl-ana.serialization)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("external-program" ,sbcl-external-program)))
    (native-inputs
     `(("cl-fad" ,sbcl-cl-fad)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres/cl-ana.makeres.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres")))))

(define-public cl-ana.makeres
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres))

(define-public sbcl-cl-ana.makeres-macro
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-macro")
    (inputs
     `(("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-macro/cl-ana.makeres-macro.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-macro")))))

(define-public cl-ana.makeres-macro
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-macro))

(define-public sbcl-cl-ana.makeres-block
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-block")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-block/cl-ana.makeres-block.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-block")))))

(define-public cl-ana.makeres-block
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-block))

(define-public sbcl-cl-ana.makeres-progress
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-progress")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-progress/cl-ana.makeres-progress.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-progress")))))

(define-public cl-ana.makeres-progress
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-progress))

(define-public sbcl-cl-ana.makeres-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-table")
    (inputs
     `(("cl-ana.csv-table" ,sbcl-cl-ana.csv-table)
       ("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)
       ("cl-ana.hdf-table" ,sbcl-cl-ana.hdf-table)
       ("cl-ana.hdf-utils" ,sbcl-cl-ana.hdf-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)
       ("cl-ana.makeres-macro" ,sbcl-cl-ana.makeres-macro)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.ntuple-table" ,sbcl-cl-ana.ntuple-table)
       ("cl-ana.reusable-table" ,sbcl-cl-ana.reusable-table)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)))
    (native-inputs
     `(("cl-fad" ,sbcl-cl-fad)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-table/cl-ana.makeres-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-table")))))

(define-public cl-ana.makeres-table
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-table))

(define-public sbcl-cl-ana.makeres-graphviz
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-graphviz")
    (inputs
     `(("cl-ana.makeres" ,sbcl-cl-ana.makeres)
       ("external-program" ,sbcl-external-program)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-graphviz/cl-ana.makeres-graphviz.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-graphviz")))))

(define-public cl-ana.makeres-graphviz
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-graphviz))

(define-public sbcl-cl-ana.makeres-branch
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-branch")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-branch/cl-ana.makeres-branch.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-branch")))))

(define-public cl-ana.makeres-branch
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-branch))

(define-public sbcl-cl-ana.makeres-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-utils")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.file-utils" ,sbcl-cl-ana.file-utils)
       ("cl-ana.fitting" ,sbcl-cl-ana.fitting)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.pathname-utils" ,sbcl-cl-ana.pathname-utils)
       ("cl-ana.plotting" ,sbcl-cl-ana.plotting)
       ("cl-ana.reusable-table" ,sbcl-cl-ana.reusable-table)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)))
    (native-inputs
     `(("cl-fad" ,sbcl-cl-fad)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-utils/cl-ana.makeres-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-utils")))))

(define-public cl-ana.makeres-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-utils))

(define-public sbcl-cl-ana.statistical-learning
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.statistical-learning")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.linear-algebra" ,sbcl-cl-ana.linear-algebra)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.statistics" ,sbcl-cl-ana.statistics)))
    (native-inputs
     `(("cl-fad" ,sbcl-cl-fad)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "")
        "statistical-learning/cl-ana.statistical-learning.asd")
       ((#:asd-system-name _ #f) "cl-ana.statistical-learning")))))

(define-public cl-ana.statistical-learning
  (sbcl-package->cl-source-package sbcl-cl-ana.statistical-learning))

(define-public sbcl-cl-ana.columnar-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.columnar-table")
    (inputs
     `(("cl-ana.reusable-table" ,sbcl-cl-ana.reusable-table)
       ("cl-ana.table" ,sbcl-cl-ana.table)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "columnar-table/cl-ana.columnar-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.columnar-table")))))

(define-public cl-ana.columnar-table
  (sbcl-package->cl-source-package sbcl-cl-ana.columnar-table))

(define-public sbcl-cl-ana
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana")
    (inputs
     `(("cl-ana.binary-tree" ,sbcl-cl-ana.binary-tree)
       ("cl-ana.calculus" ,sbcl-cl-ana.calculus)
       ("cl-ana.clos-utils" ,sbcl-cl-ana.clos-utils)
       ("cl-ana.csv-table" ,sbcl-cl-ana.csv-table)
       ("cl-ana.columnar-table" ,sbcl-cl-ana.columnar-table)
       ("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.file-utils" ,sbcl-cl-ana.file-utils)
       ("cl-ana.fitting" ,sbcl-cl-ana.fitting)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)
       ("cl-ana.hdf-table" ,sbcl-cl-ana.hdf-table)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.int-char" ,sbcl-cl-ana.int-char)
       ("cl-ana.linear-algebra" ,sbcl-cl-ana.linear-algebra)
       ("cl-ana.lorentz" ,sbcl-cl-ana.lorentz)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)
       ("cl-ana.makeres-block" ,sbcl-cl-ana.makeres-block)
       ("cl-ana.makeres-branch" ,sbcl-cl-ana.makeres-branch)
       ("cl-ana.makeres-graphviz" ,sbcl-cl-ana.makeres-graphviz)
       ("cl-ana.makeres-macro" ,sbcl-cl-ana.makeres-macro)
       ("cl-ana.makeres-progress" ,sbcl-cl-ana.makeres-progress)
       ("cl-ana.makeres-table" ,sbcl-cl-ana.makeres-table)
       ("cl-ana.makeres-utils" ,sbcl-cl-ana.makeres-utils)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)
       ("cl-ana.ntuple-table" ,sbcl-cl-ana.ntuple-table)
       ("cl-ana.package-utils" ,sbcl-cl-ana.package-utils)
       ("cl-ana.pathname-utils" ,sbcl-cl-ana.pathname-utils)
       ("cl-ana.plotting" ,sbcl-cl-ana.plotting)
       ("cl-ana.quantity" ,sbcl-cl-ana.quantity)
       ("cl-ana.reusable-table" ,sbcl-cl-ana.reusable-table)
       ("cl-ana.serialization" ,sbcl-cl-ana.serialization)
       ("cl-ana.statistics" ,sbcl-cl-ana.statistics)
       ("cl-ana.statistical-learning" ,sbcl-cl-ana.statistical-learning)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("cl-ana.table-utils" ,sbcl-cl-ana.table-utils)
       ("cl-ana.table-viewing" ,sbcl-cl-ana.table-viewing)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)
       ("libffi" ,libffi)))
    (native-inputs
     `(("cl-fad" ,sbcl-cl-fad)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "cl-ana.asd")
       ((#:asd-system-name _ #f) "cl-ana")))))

(define-public cl-ana
  (sbcl-package->cl-source-package sbcl-cl-ana))
