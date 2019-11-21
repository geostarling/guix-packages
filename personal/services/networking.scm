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

(define-module (personal services networking)
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
  #:export (bridge-configuration
            bridge-configuration?
            bridge-service-type))


;;;; Commentary:
;;;
;;; This module implements a service that to run instance of bridge.
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

(define (serialize-list field-name val)
  (if (null? val) "" (serialize-field field-name (string-join val))))


(define-configuration bridge-configuration
  (iproute
   (package iproute)
   "Iproute package used to configure the bridge.")
  (bridge-interface
   (string 'unset)
   "Bridge interface name.")
  (interfaces
   (list 'unset)
   "List of interfaces"))



(define (bridge-shepherd-service config)
  "Return a <shepherd-service> for bridge with CONFIG."
  (let* ((iproute                 (bridge-configuration-iproute config))
         (bridge-interface        (bridge-configuration-bridge-interface config))
         (interfaces              (bridge-configuration-interfaces config)))
    (list (shepherd-service
           (provision '(bridge))
           (documentation "Run bridge daemon.")
           ;;           (requirement '(networking))
           (start #~(lambda _
                      (let* ((ip (string-append #$iproute "/sbin/ip"))
                             (attach-interface (lambda (interface)
                                                 (system* ip "link"
                                                          "set" interface
                                                          "master" "br0"))))
                        (system* ip "link" "add"
                                 "name" #$bridge-interface
                                 "type" "bridge")
                        (set-network-interface-up #$bridge-interface)
                        (for-each set-network-interface-up #$@interfaces)
                        (for-each attach-interface #$@interfaces))))
           (stop #~(lambda _
                     (let ((ip (string-append #$iproute "/sbin/ip")))
                       (system* ip "link" "del"
                                #$bridge-interface))))
           (respawn? #f)))))

(define bridge-service-type
  (service-type
   (name 'bridge)
   (extensions
    (list (service-extension shepherd-root-service-type bridge-shepherd-service)))
   (default-value (bridge-configuration))))
