(define-module (personal packages jupyter)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages monitoring))

(define-public python-nose-warnings-filters
  (package
    (name "python-nose-warnings-filters")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nose_warnings_filters" version))
        (sha256
          (base32
            "17dvfqfy2fm7a5cmiffw2dc3064kpx72fn5mlw01skm2rhn5nv25"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-nose" ,python-nose)))
    (home-page "")
    (synopsis
      "Allow to inject warning filters during ``nosetest``.")
    (description
      "Allow to inject warning filters during ``nosetest``.")
    (license #f)))


(define-public python-nose-exclude
  (package
    (name "python-nose-exclude")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nose-exclude" version))
        (sha256
          (base32
            "0123x1lyv5b2p9civcfg8vilj2ga3q7p2ks1hq25z0gb3ssai3zp"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-nose" ,python-nose)))
    (home-page
      "https://github.com/kgrandis/nose-exclude")
    (synopsis
      "Exclude specific directories from nosetests runs.")
    (description
      "Exclude specific directories from nosetests runs.")
    (license license:lgpl2.0)))

(define-public python-pytest-console-scripts
  (package
    (name "python-pytest-console-scripts")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-console-scripts" version))
        (sha256
          (base32
            "16g6sdm66y5d97l3si44xcjxv0p7a0drm4nlsn7yz21vcrfw32m8"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; tests do not work at the moment
    (propagated-inputs
      `(("python-mock" ,python-mock)
        ("python-pytest" ,python-pytest)
        ("python-pytest-runner" ,python-pytest-runner)))
    (home-page
      "https://github.com/kvas-it/pytest-console-scripts")
    (synopsis
      "Pytest plugin for testing console scripts")
    (description
      "Pytest plugin for testing console scripts")
    (license license:expat)))

(define-public python-pytest-tornasync
  (package
    (name "python-pytest-tornasync")
    (version "0.6.0.post2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-tornasync" version))
        (sha256
          (base32
            "0pdyddbzppkfqwa7g17sdfl4w2v1hgsky78l8f4c1rx2a7cvd0fp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; tests do not work at the moment
    (propagated-inputs
      `(("python-pytest" ,python-pytest)
        ("python-tornado" ,python-tornado)))
    (home-page
      "https://github.com/eukaryote/pytest-tornasync")
    (synopsis
      "py.test plugin for testing Python 3.5+ Tornado code")
    (description
      "py.test plugin for testing Python 3.5+ Tornado code")
    (license #f)))


(define-public python-jupyter-server
  (package
    (name "python-jupyter-server")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jupyter_server" version))
        (sha256
          (base32
            "0k3yynaajs7rgv4s22bcyryj37wr0cph0wxw2wbx10ww74dsrn48"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; tests do not work at the moment
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipython-genutils"
        ,python-ipython-genutils)
       ("python-jinja2" ,python-jinja2)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-nbconvert" ,python-nbconvert)
       ("python-nbformat" ,python-nbformat)
       ("python-prometheus-client"
        ,python-prometheus-client)
       ("python-pyzmq" ,python-pyzmq)
       ("python-send2trash" ,python-send2trash)
       ("python-terminado" ,python-terminado)
       ("python-tornado" ,python-tornado)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
      `(("python-coverage" ,python-coverage)
        ("python-nose" ,python-nose)
        ("python-nose-exclude" ,python-nose-exclude)
        ("python-nose-warnings-filters"
         ,python-nose-warnings-filters)
        ("python-pytest" ,python-pytest)
        ("python-pytest-console-scripts"
         ,python-pytest-console-scripts)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-tornasync"
         ,python-pytest-tornasync)
        ("python-requests" ,python-requests)))
    (home-page "http://jupyter.org")
    (synopsis "The Jupyter Server")
    (description "The Jupyter Server")
    (license license:bsd-3)))

(define-public python-pytest-tornado
  (package
    (name "python-pytest-tornado")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-tornado" version))
        (sha256
          (base32
            "1cgisd7lb9q2hf55558cbn5jfhv65vsgk46ykgidzf9kqcq1kymr"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pytest" ,python-pytest)
        ("python-setuptools" ,python-setuptools)
        ("python-tornado" ,python-tornado)))
    (home-page
      "https://github.com/eugeniy/pytest-tornado")
    (synopsis
      "A py.test plugin providing fixtures and markers to simplify testing of asynchronous tornado applications.")
    (description
      "A py.test plugin providing fixtures and markers to simplify testing of asynchronous tornado applications.")
    (license license:asl2.0)))

(define-public python-jupyterlab-pygments
  (package
    (name "python-jupyterlab-pygments")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jupyterlab_pygments" version))
        (sha256
          (base32
            "1fcply9b8bvv659m5ml1j5659xm4cc5xdlrw6qwcdpmdgpgcr80r"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pygments" ,python-pygments)))
    (home-page "http://jupyter.org")
    (synopsis
      "Pygments theme using JupyterLab CSS variables")
    (description
      "Pygments theme using JupyterLab CSS variables")
    (license #f)))


(define-public python-voila
  (package
    (name "python-voila")
    (version "0.1.21")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "voila" version))
        (sha256
          (base32
            "1asr4inrqaisvzgpw5im2fx7xxzdyqi3y0kfzq2n7y977hxwdr3f"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; tests do not work at the moment
    (propagated-inputs
      `(("python-async-generator"
         ,python-async-generator)
        ("python-jupyter-server" ,python-jupyter-server)
        ("python-jupyterlab-pygments"
         ,python-jupyterlab-pygments)
        ("python-nbconvert" ,python-nbconvert)
        ("python-pygments" ,python-pygments)))
    (native-inputs
      `(("python-ipywidgets" ,python-ipywidgets)
        ("python-matplotlib" ,python-matplotlib)
        ("python-mock" ,python-mock)
        ("python-pytest" ,python-pytest)
        ("python-pytest-tornado" ,python-pytest-tornado)))
    (home-page
      "https://github.com/voila-dashboards/voila")
    (synopsis
      "Serving read-only live Jupyter notebooks")
    (description
      "Serving read-only live Jupyter notebooks")
    (license #f)))
