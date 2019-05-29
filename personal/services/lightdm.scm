;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 L  p R n  d n   <guix@lprndn.info>
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


(define-module (personal services lightdm)
  #:use-module (guix gexp)
  #:use-module (guix records)

  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)

  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services xorg)

  #:use-module (gnu packages admin)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xorg)

  #:export (lightdm-configuration
            lightdm-configuration?
            lightdm-service-type
            lightdm-gtk-greeter-configuration
            lightdm-gtk-greeter-configuration-file))


(define-record-type* <lightdm-gtk-greeter-configuration>
  lightdm-gtk-greeter-configuration make-lightdm-gtk-greeter-configuration
  lightdm-gtk-greeter-configuration?
  (theme-name lightdm-gtk-greeter-configuration-theme-name
              (default "Adwaita"))
  (icon-theme-name
   lightdm-gtk-greeter-configuration-icon-theme-name
   (default "Adwaita"))
  (cursor-theme-name
   lightdm-gtk-greeter-configuration-cursor-theme-name
   (default "Adwaita"))
  (cursor-size lightdm-gtk-greeter-configuration-cursor-size
               (default 16))
  (background lightdm-gtk-greeter-configuration-background
              (default "")))

(define (lightdm-gtk-greeter-configuration-file config)
  (mixed-text-file "lightdm-gtk-greeter.conf" "
[greeter]
theme-name = "        (lightdm-gtk-greeter-configuration-theme-name config) "
icon-theme-name = "   (lightdm-gtk-greeter-configuration-icon-theme-name config) "
cursor-theme-name = " (lightdm-gtk-greeter-configuration-cursor-theme-name config) "
cursor-theme-size = " (number->string
                       (lightdm-gtk-greeter-configuration-cursor-size config)) "
background = "        (lightdm-gtk-greeter-configuration-background config)))

(define-record-type* <lightdm-configuration>
  lightdm-configuration make-lightdm-configuration
  lightdm-configuration?

  (lightdm lightdm-configuration-lightdm
           (default lightdm))
  (sessions-directory lightdm-configuration-sessions-directory
                      (default (string-append
                                "/run/current-system/profile/share/xsessions"
                                ":/run/current-system/profile/share/wayland-sessions")))
  (allow-empty-passwords? lightdm-configuration-allow-empty-passwords?
                          (default #f))
  ;; [Seat]
  (xorg-configuration lightdm-configuration-xorg
                      (default (xorg-configuration)))
  (session-wrapper lightdm-configuration-session-wrapper
                   (default (xinitrc)))
  (default-session-name lightdm-configuration-default-session
    (default ""))

  ;; [Autologin]
  (autologin-user lightdm-configuration-autologin-user
                  (default #f))
  (autologin-timeout lightdm-configuration-autologin-timeout
                     (default #f))
  ;; [Greeter]
  (greeter-name lightdm-configuration-greeter-name
                (default "lightdm-gtk-greeter"))
  (greeter-package lightdm-configuration-greeter-package
                   (default lightdm-gtk-greeter))
  (greeter-assets lightdm-configuration-greeter-assets
                  (default (list adwaita-icon-theme
                                 gnome-themes-standard)))
  (greeter-configuration-file lightdm-configuration-greeter-configuration
                              (default (lightdm-gtk-greeter-configuration-file
                                        (lightdm-gtk-greeter-configuration)))))

(define %lightdm-accounts
  (list (user-group (name "lightdm") (system? #t))
        (user-account
         (name "lightdm")
         (group "lightdm")
         (system? #t)
         (comment "LighDM user")
         (home-directory "/var/lib/lightdm")
         (shell (file-append shadow "/sbin/nologin")))))


(define (lightdm-configuration-file config)
  (mixed-text-file "lightdm.conf" "
[LightDM]
greeter-user = lightdm
greeters-directory = /run/current-system/profile/share/xgreeters
sessions-directory = "  (lightdm-configuration-sessions-directory config) "


[Seat:*]
xserver-command = "  (xorg-start-command (lightdm-configuration-xorg config)) "
greeter-session = "       (lightdm-configuration-greeter-name config) "
user-session = "          (lightdm-configuration-default-session config)
(if (lightdm-configuration-autologin-user config) (string-append "
autologin-user = " (lightdm-configuration-autologin-user config)) "")
(if (string-null? (lightdm-configuration-default-session config)) ""
    (string-append "
autologin-session = " (lightdm-configuration-default-session config)))
(if (lightdm-configuration-autologin-timeout config) (string-append "
autologin-user-timeout = " (lightdm-configuration-autologin-timeout config)) "") "
session-wrapper = " (lightdm-configuration-session-wrapper config)))


(define (lightdm-pam-service config)
  "Return a PAM service for @command{lightdm}."
  (unix-pam-service
   "lightdm"
   #:allow-empty-passwords?
   (lightdm-configuration-allow-empty-passwords? config)))


(define (lightdm-greeter-pam-service)
  "Return a PAM service for @command{lightdm-greeter}}."
  (pam-service
   (name "lightdm-greeter")
   (auth
    (list
     ;; Load environment from /etc/environment and ~/.pam_environment
     (pam-entry (control "required") (module "pam_env.so"))
     ;; Always let the greeter start without authentication
     (pam-entry (control "required") (module "pam_permit.so"))))
   ;; No action required for account management
   (account
    (list
     (pam-entry (control "required") (module "pam_permit.so"))))
   ;; Can't change password
   (password
    (list
     (pam-entry (control "required") (module "pam_deny.so"))))
   ;; Setup session
   (session
    (list
     (pam-entry (control "required") (module "pam_unix.so"))
     (pam-entry (control "required") (module "pam_env.so"))))))


(define (lightdm-autologin-pam-service)
  "Return a PAM service for @command{lightdm-autologin}}."
  (pam-service
   (name "lightdm-autologin")
   (auth
    (list
     ;; Block login if they are globally disabled
     (pam-entry (control "required") (module "pam_nologin.so"))
     ;; Load environment from /etc/environment and ~/.pam_environment
     (pam-entry (control "required") (module "pam_env.so"))
     ;; Allow access without authentication
     (pam-entry (control "required") (module "pam_permit.so"))))
   ;; Stop autologin if account requires action
   (account
    (list
     (pam-entry (control "required") (module "pam_unix.so"))))
   ;; Can't change password
   (password
    (list
     (pam-entry (control "required") (module "pam_deny.so"))))
   ;; Setup session
   (session
    (list
     (pam-entry (control "required") (module "pam_unix.so"))))))


(define (lightdm-shepherd-service config)
  "Return a <lightdm-service> for LightDM with CONFIG."

  (define lightdm-command
    #~(list (string-append #$(lightdm-configuration-lightdm config) "/sbin/lightdm")))

  (list (shepherd-service
         (documentation "LightDM display manager.")
         (requirement '(dbus-system user-processes host-name))
         (provision '(display-manager))
         (respawn? #f)
         (start #~(lambda ()
                    (fork+exec-command
                     (list #$(file-append
                              (lightdm-configuration-lightdm config)
                              "/sbin/lightdm"))
                     #:environment-variables
                     (list
                      (string-append
                       "PATH=/run/current-system/profile/sbin"
                       ":/run/current-system/profile/bin")))))
         (stop #~(make-kill-destructor)))))

(define (lightdm-etc-service config)
  (list `("xdg/lightdm/lightdm.conf.d/lightdm.conf"
          ,(lightdm-configuration-file config))
        `(,(string-append "xdg/lightdm/"
                          (computed-file-name
                           (lightdm-configuration-greeter-configuration config)))
          ,(lightdm-configuration-greeter-configuration config))))

(define (lightdm-pam-services config)
  (list (lightdm-pam-service config)
        (lightdm-greeter-pam-service)
        (lightdm-autologin-pam-service)))

(define (lightdm-profile-service config)
  (append (list lightdm
                (lightdm-configuration-greeter-package config))
          (lightdm-configuration-greeter-assets config)))

(define (lightdm-activation-service config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (define %user
          (getpw "lightdm"))
        (let ((directory "/var/lib/lightdm-data"))
          (mkdir-p directory)
          (chown directory (passwd:uid %user) (passwd:gid %user))))))

(define lightdm-service-type
  (service-type (name 'lightdm)
                (extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     lightdm-shepherd-service)
                  (service-extension activation-service-type
                                     lightdm-activation-service)
                  (service-extension pam-root-service-type
                                     lightdm-pam-services)
                  (service-extension etc-service-type
                                     lightdm-etc-service)
                  (service-extension dbus-root-service-type
                                     (compose list lightdm-configuration-lightdm))
                  (service-extension account-service-type
                                     (const %lightdm-accounts))
                  (service-extension profile-service-type
                                     lightdm-profile-service)))
                (default-value (lightdm-configuration))
                (description "Return a service that spawns the LightDM graphical login
 manager.")))

