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
            kodi-service-type))

;;;; Commentary:
;;;
;;; This module implements a service that to run instance of tvheadend.
;;;
;;;; Code:


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
   (string "tvheadend")
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
   (extensions
    (list (service-extension shepherd-root-service-type tvheadend-shepherd-service)
          (service-extension account-service-type tvheadend-account)
          (service-extension activation-service-type tvheadend-activation)))
   (default-value (tvheadend-configuration))))



(define-configuration kodi-configuration
  (package
   (package kodi)
   "The kodi package.")
  (xinit-package
   (package xinit)
   "The xinit package.")
  (home-dir
   (string "/var/lib/kodi")
   "Path to home directory.")
  (user
   (string "kodi")
   "User who will run the Kodi daemon.")
  (group
   (string "kodi")
   "Group who will run the Kodi daemon."))


(define (kodi-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((kodi-user (kodi-configuration-user config))
        (kodi-group (kodi-configuration-group config)))
    (list (user-group (name kodi-group) (system? #t))
          (user-account
           (name kodi-user)
           (system? #t)
           (group kodi-group)
           (comment "kodi privilege separation user")
           (home-directory (kodi-configuration-home-dir config))
           (shell (file-append shadow "/sbin/nologin"))))))

(define (kodi-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils)
                           (ice-9 rdelim))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 rdelim))
        (let ((user (getpw #$(kodi-configuration-user config))))
          (for-each (lambda (directory)
                        (mkdir-p directory)
                        (chown directory (passwd:uid user) (passwd:gid user))
                        (chmod directory #o750))
                    (list #$(kodi-configuration-home-dir config)))))))

(define (kodi-shepherd-service config)
  "Return a <shepherd-service> for kodi with CONFIG."
  (let* ((kodi        (kodi-configuration-package config))
         (xinit       (kodi-configuration-xinit-package config))
         (user        (kodi-configuration-user config))
         (group       (kodi-configuration-group config)))
    (list (shepherd-service
           (provision '(kodi))
           (documentation "Run kodi as a daemon.")
           (requirement '(networking))
           (start #~(make-forkexec-constructor
                     (list (string-append #$xinit "--" #$kodi "/bin/kodi-stadalone"))
                     #:pid-file "/var/run/kodi.pid"
                     #:user #$user
                     #:group #$group))
           (stop #~(make-kill-destructor))))))

(define kodi-service-type
  (service-type
   (name 'kodi)
   (extensions
    (list (service-extension shepherd-root-service-type kodi-shepherd-service)
          (service-extension account-service-type kodi-account)
          (service-extension activation-service-type kodi-activation)))
   (default-value (kodi-configuration))))
