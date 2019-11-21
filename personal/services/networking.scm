;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (gnu services dbus)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages connman)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages usb-modeswitch)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ntp)
  #:use-module (gnu packages wicd)
  #:use-module (gnu packages gnome)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix deprecation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:re-export (static-networking-service
               static-networking-service-type)
  #:export (bridge

            bridge?
            bridge-bridge-interface
            bridge-interfaces
            bridge-requirement
            bridge-provision
            bridge-iproute

            bridge-service
            bridge-service-type))



;;
;; bridge service
;;

(define-record-type* <bridge-configuration>
  bridge-configuration
  make-bridge-configuration
  bridge-configuration?

  (iproute            bridge-configuration-iproute
                      (default iproute))
  (bridge-interface   bridge-configuration-bridge-interface) ; a string
  (interfaces         bridge-configuration-interfaces) ; list of interfaces
  (requirement        bridge-configuration-requirement
                      (default '()))
  (provision          bridge-configuration-provision))

(define bridge-shepherd-service
  (match-lambda
   (($ <bridge-configuration>
       iproute
       bridge-interfaces
       interfaces
       requirement
       provision)
    (shepherd-service

     (documentation "Create an L2 bridge for selected NICs.")

     (requirement requirement)

     (provision (or provision
                    (list (symbol-append 'bridge-
                                         (string->symbol bridge-interface)))))

     (start #~(lambda _
                (let* ((ip (string-append #$iproute "/sbin/ip"))
                       (attach-interface (lambda (interface)
                                           (system* ip "link"
                                                    "set" interface
                                                    "master" #bridge-interface))))
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

(define (bridge-shepherd-services bridges)
  "Return a list of Shepherd services to bring up BRIDGES, a list of
<bridge> objects."
  (let ((services (map bridge-shepherd-service bridges)))
    (cons (shepherd-service
           (provision '(all-bridges))
           (requirement (append-map shepherd-service-provision services))
           (start #~(const #t))
           (stop #~(const #f))
           (documentation "Bringing up all bridges."))
          services)))

(define bridge-service-type
  (service-type (name 'bridge)
                (extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     bridge-shepherd-services)))
                (compose concatenate)
                (extend append)
                (default-value '())
                (description "Add a physical or virtual interface to a bridge. The
value for services of this type is a list of @code{bridge} objects, one
per bridge.")))

(define* (bridge-service bridge-interface
                         interfaces
                         #:key
                         (requirement '(udev))
                         (iproute iproute)
                         provision)
  "Create a bridge."
  (simple-service 'bridge-service
                  bridge-service-type
                  (list
                   (bridge-configuration (iproute iproute)
                                         (bridge-interface bridge-interface)
                                         (interfaces interfaces)
                                         (requirement requirement)
                                         (provision provision)))))


;;; networking.scm ends here
