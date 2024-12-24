;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (personal services tv)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (personal packages tv)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages kodi)
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages python)
  #:use-module (guix build-system trivial)
  #:use-module (guix build union)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (tvheadend-configuration
            tvheadend-configuration?
            tvheadend-service-type
            kodi-configuration
            kodi-configuration?
            kodi-service-type
            lirc-configuration
            lirc-configuation?
            lirc-service-type
            irexec-configuration
            irexec-configuation?
            irexec-service-type
            miraclecast-wifid-service-type
            miraclecast-wifid-configuration
            miraclecast-sinkctl-service-type
            miraclecast-sinkctl-configuration
            script-o2tv-server-configuration
            script-o2tv-server-configuation?
            script-o2tv-server-service-type))




(define (uglify-field-name field-name)
  (apply string-append
         (map (lambda (str)
                (if (member (string->symbol str) '(ca db ssl))
                    (string-upcase str)
                    (string-capitalize str)))
              (string-split (string-delete #\?
                                           (symbol->string field-name))
                            #\-))))

(define (serialize-field field-name val)
  (format #t "~a=~a~%" (uglify-field-name field-name) val))

(define (serialize-number field-name val)
  (serialize-field field-name (number->string val)))

(define (serialize-string field-name val)
  (if (and (string? val) (string=? val ""))
      ""
      (serialize-field field-name val)))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val "1" "0")))

;;;; Commentary:
;;;
;;; This module implements a service that to run instance of tvheadend.
;;;
;;;; Code:

(define-configuration tvheadend-configuration
  (package
   (package tvheadend)
   "The tvheadend package.")
  (config-path
   (string "/etc/tvheadend")
   "Path to configuration directory.")
  (user
   (string "tvheadend")
   "User who will run the Tvheadend daemon.")
  (group
   (string "video")
   "Group who will run the Tvheadend daemon.")
  (satip-bind-address
   (string "")
   "Bind address for SAT>IP server")
  (satip-rtsp-port
   (number -1)
   "Database name.")
  (satip-enable?
   (boolean #f)
   "Enable SAT>IP client.")
  (satip-url
   (string "")
   "URL with the SAT>IP server XML location.")
  (ipv6-listen?
   (boolean #f)
   "Listen on IPv6")
  (bind-address
   (string "127.0.0.1")
   "Bind address.")
  (http-port
   (number 9981)
   "HTTP port.")
  (http-webroot
   (string "")
   "HTTP webroot path.")
  (htsp-port
   (number 9982)
   "HTSP port.")
  (htsp-extra-port
   (number -1)
   "HTSP port.")
  (user-agent-header
   (string "")
   "Specify User-Agent header for the http client.")
  (use-xspf-playlist?
   (boolean #f)
   "Use XSPF playlist instead of M3U.")
  (acl-disable?
   (boolean #f)
   "Disable all access control checks."))


(define (tvheadend-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((tvheadend-user (tvheadend-configuration-user config))
        (tvheadend-group (tvheadend-configuration-group config)))
    (list (user-group (name tvheadend-group) (system? #t))
          (user-account
           (name tvheadend-user)
           (system? #t)
           (group tvheadend-group)
           (comment "tvheadend privilege separation user")
           (home-directory (string-append "/var/run/" tvheadend-user))
           (shell (file-append shadow "/sbin/nologin"))))))

(define (tvheadend-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils)
                           (ice-9 rdelim))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 rdelim))
        (let ((user (getpw #$(tvheadend-configuration-user config))))
          (for-each (lambda (directory)
                        (mkdir-p directory)
                        (chown directory (passwd:uid user) (passwd:gid user))
                        (chmod directory #o750))
                    (list #$(tvheadend-configuration-config-path config)))))))

(define (tvheadend-shepherd-service config)
  "Return a <shepherd-service> for tvheadend with CONFIG."
  (let* ((tvheadend   (tvheadend-configuration-package config))
         (user        (tvheadend-configuration-user config))
         (group       (tvheadend-configuration-group config)))
    (list (shepherd-service
           (provision '(tvheadend))
           (documentation "Run tvheadend daemon.")
           (requirement '(networking))
           (start #~(make-forkexec-constructor
                     (list (string-append #$tvheadend "/bin/tvheadend")
                           "--config" #$(tvheadend-configuration-config-path config)
                           "--noacl")
                           ;; TODO:"--adapters" #$@(tvheadend-adapters config)
                           ;; TODO: every other config opt/
                     #:user #$user
                     #:group #$group))
           (stop #~(make-kill-destructor))))))

(define tvheadend-service-type
  (service-type
   (name 'tvheadend)
   (description "TODO")
   (extensions
    (list (service-extension shepherd-root-service-type tvheadend-shepherd-service)
          (service-extension account-service-type tvheadend-account)
          (service-extension activation-service-type tvheadend-activation)))
   (default-value (tvheadend-configuration))))


(define-record-type* <lirc-configuration>
  lirc-configuration make-lirc-configuration
  lirc-configuation?
  (lirc          lirc-configuration-lirc          ;<package>
                 (default lirc))
  (device        lirc-configuration-device)       ;string
  (driver        lirc-configuration-driver)       ;string
  (config-file   lirc-configuration-file)         ;string | file-like object
  (extra-options lirc-configuration-options       ;list of strings
                 (default '()))
  (extra-plugins lirc-configuration-extra-plugins ;list of <package>
                 (default '())))

(define (final-lirc lirc plugin-packages)
  (if (null? plugin-packages)
    lirc
    (package
      (inherit lirc)
      (source #f)
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils) (guix build union))
         #:builder
         (begin
           (use-modules (guix build utils) (guix build union) (srfi srfi-26))
           (union-build (assoc-ref %outputs "out") (map (lambda (input) (cdr input)) %build-inputs))
           #t)))
      (inputs
       `(("lirc" ,lirc)
         ,@(map (lambda (extension) (list "extension" extension))
                plugin-packages))))))

(define %lirc-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/run/lirc")))

(define lirc-shepherd-service
  (match-lambda
    (($ <lirc-configuration> lirc device driver config-file options extra-plugins)
     (let ((final-package (final-lirc lirc extra-plugins)))
       (list (shepherd-service
              (provision '(lircd))
              (documentation "Run the LIRC daemon.")
              (requirement '(user-processes))
              (start #~(make-forkexec-constructor
                        (list (string-append #$final-package "/sbin/lircd")
                              "--nodaemon"
                              "--plugindir" (string-append #$final-package "/lib/lirc/plugins")
                              #$@(if device
                                     #~("--device" #$device)
                                     #~())
                              #$@(if driver
                                     #~("--driver" #$driver)
                                     #~())
                              #$@options
                              #$@(if config-file
                                     #~(#$config-file)
                                     #~()))))
              (stop #~(make-kill-destructor))))))))

(define lirc-service-type
  (service-type (name 'lirc)
                (description "TODO")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          lirc-shepherd-service)
                       (service-extension activation-service-type
                                          (const %lirc-activation))))))


(define-configuration irexec-configuration
  (package
   (package lirc)
   "The irexec package.")
  (config-path
   (string "")
   "Path to lircrc configuration file.")
  (program-name
   (string "irexec")
   "Name of program in lircrc")
  (log-level
   (string "error")
   "Log level."))


(define (irexec-shepherd-service config)
  "Return a <shepherd-service> for irexec with CONFIG."
  (let* ((irexec   (irexec-configuration-package config)))
    (list (shepherd-service
           (provision '(irexec))
           (requirement '(lircd))
           (documentation "Run irexec daemon.")
           (start #~(make-forkexec-constructor
                     (list (string-append #$irexec "/bin/irexec")
                           (string-append "--loglevel=" #$(irexec-configuration-log-level config))
                           (string-append "--name=" #$(irexec-configuration-program-name config))
                           #$(irexec-configuration-config-path config))))
           (stop #~(make-kill-destructor))))))

(define irexec-service-type
  (service-type
   (name 'irexec)
   (description "TODO")
   (extensions
    (list (service-extension shepherd-root-service-type irexec-shepherd-service)))
   (default-value (irexec-configuration))))


(define-configuration miraclecast-wifid-configuration
  (package
   (package miraclecast)
   "The miraclecast package.")
  (interface
   (string "")
   "Wifi interface to use."))

(define (miraclecast-wifid-shepherd-service config)
  "Return a <shepherd-service> for miraclecast-wifid with CONFIG."
  (let* ((miraclecast (miraclecast-wifid-configuration-package config))
         (interface (miraclecast-wifid-configuration-interface config)))
    (list (shepherd-service
           (provision '(miraclecast-wifid))
           (documentation "Run miracle-wifid daemon.")
           (requirement '(networking dbus-system))
           (start #~(make-forkexec-constructor
                     (list (string-append #$miraclecast "/bin/miracle-wifid")
                           #$@(if interface
                               #~("--interface" #$interface)
                               #~()))
                     #:environment-variables
                     (list "PATH=/run/current-system/profile/sbin:/run/current-system/profile/bin"))) ;; miracle-wifid looks for wpa_supplicant binary in PATH
           (stop #~(make-kill-destructor))))))

(define miraclecast-wifid-service-type
  (service-type
   (name 'miraclecast-wifid)
   (description "TODO")
   (extensions
    (list (service-extension shepherd-root-service-type miraclecast-wifid-shepherd-service)))
   (default-value (miraclecast-wifid-configuration))))


(define-configuration miraclecast-sinkctl-configuration
  (package
   (package miraclecast)
   "The miraclecast package.")
  (link
   (string "")
   "link to run")
  (external-player
   (string "")
   "path to external player program"))


(define (miraclecast-sinkctl-shepherd-service config)
  "Return a <shepherd-service> for miraclecast-sinkctl with CONFIG."
  (let* ((miraclecast (miraclecast-sinkctl-configuration-package config))
         (link (miraclecast-sinkctl-configuration-link config))
         (external-player (miraclecast-sinkctl-configuration-external-player config)))
    (list (shepherd-service
           (provision '(miraclecast-sinkctl))
           (documentation "Run miracle-sinkctl daemon.")
           (requirement '(miraclecast-wifid dbus-system))
           (start #~(lambda _
                      (display "ajmsleeping")
                      (sleep 5)
                      (fork+exec-command
                          (list (string-append #$miraclecast "/bin/miracle-sinkctl")
                                #$@(if external-player
                                       #~("--external-player" #$external-player)
                                       #~())
                                "run"
                                #$link)
                     #:environment-variables
                     (list "PATH=/run/current-system/profile/sbin:/run/current-system/profile/bin")))) ;; miracle-sinkctl needs working PATH for external-player script

           (stop #~(make-kill-destructor))))))


      ;; (start #~(lambda args
      ;;          ;; spice-vdagentd supports being activated upon the client
      ;;          ;; connecting to its socket; when not using such feature, the
      ;;          ;; socket should not exist before vdagentd creates it itself.
      ;;          (mkdir-p "/run/spice-vdagentd")
      ;;          (false-if-exception
      ;;           (delete-file "/run/spice-vdagentd/spice-vdagent-sock"))
      ;;          (fork+exec-command '#$spice-vdagentd-command)))

      ;;        (start
      ;;         #~(lambda _
      ;;             (let ((pid
      ;;                    (fork+exec-command
      ;;                     (list #$(file-append network-manager
      ;;                                          "/sbin/NetworkManager")
      ;;                           (string-append "--config=" #$conf)
      ;;                           "--no-daemon")
      ;;                     #:environment-variables
      ;;                     (list (string-append "NM_VPN_PLUGIN_DIR=" #$vpn
      ;;                                          "/lib/NetworkManager/VPN")
      ;;                           ;; Override non-existent default users
      ;;                           "NM_OPENVPN_USER="
      ;;                           "NM_OPENVPN_GROUP="
      ;;                           ;; Allow NetworkManager to find the modules.
      ;;                           (string-append
      ;;                            "LINUX_MODULE_DIRECTORY="
      ;;                            "/run/booted-system/kernel/lib/modules")))))
      ;;               ;; XXX: Despite the "online" name, this doesn't guarantee
      ;;               ;; WAN connectivity, it merely waits for NetworkManager
      ;;               ;; to finish starting-up. This is required otherwise
      ;;               ;; services will fail since the network interfaces be
      ;;               ;; absent until NetworkManager finishes setting them up.
      ;;               (system* #$(file-append network-manager "/bin/nm-online")
      ;;                        "--wait-for-startup" "--quiet")
      ;;               ;; XXX: Finally, return the pid from running
      ;;               ;; fork+exec-command to shepherd.
      ;;               pid)))


(define miraclecast-sinkctl-service-type
  (service-type
   (name 'miraclecast-sinkctl)
   (description "TODO")
   (extensions
    (list (service-extension shepherd-root-service-type miraclecast-sinkctl-shepherd-service)))
   (default-value (miraclecast-sinkctl-configuration))))



(define-configuration script-o2tv-server-configuration
  (package
   (package script-o2tv-server)
   "The package."))


(define (script-o2tv-server-shepherd-service config)
  "Return a <shepherd-service> for script-o2tv-server with CONFIG."
  (let* ((script-o2tv-server   (script-o2tv-server-configuration-package config)))
    (list (shepherd-service
           (provision '(script-o2tv-server))
           (requirement '(tvheadend))
           (documentation "Run script-o2tv-server daemon.")
           (start #~(make-forkexec-constructor
                     (list (string-append #$python "/bin/python3")
                           (string-append #$script-o2tv-server "/server.py"))
                     #:log-file "/tmp/script-o2tv-server.log"
                     #:environment-variables
                     (list (string-append "PATH="
                                          (string-join '("/run/privileged/bin"
                                                         "/run/current-system/profile/sbin"
                                                         "/run/current-system/profile/bin")
                                                       ":"))
                           "GUIX_PYTHONPATH=/run/current-system/profile/lib/python3.10/site-packages"
                           "PYTHONPATH=/run/current-system/profile/lib/python3.10/site-packages")))
           (stop #~(make-kill-destructor))))))

(define script-o2tv-server-service-type
  (service-type
   (name 'script-o2tv-server)
   (description "TODO")
   (extensions
    (list (service-extension shepherd-root-service-type script-o2tv-server-shepherd-service)))
   (default-value (script-o2tv-server-configuration))))
