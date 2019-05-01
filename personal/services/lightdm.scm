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

  #:use-module (personal packages display-managers)

  #:export (lightdm-configuration
            lightdm-configuration?
            lightdm-service-type))

(define-record-type* <lightdm-configuration>
  lightdm-configuration make-lightdm-configuration
  lightdm-configuration?

  (lightdm lightdm-configuration-lightdm
           (default lightdm))
  (user lightdm-configuration-user
        (default "lightdm"))
  (greeters-directory lightdm-configuration-greeters-directory
                      (default "/run/current-system/profile/share/xgreeters"))
  (sessions-directory lightdm-configuration-sessions-directory
                      (default (string-append
                                "/run/current-system/profile/share/xsessions"
                                ":/run/current-system/profile/share/wayland-sessions")))
  (allow-empty-passwords? lightdm-configuration-allow-empty-passwords?
                          (default #f))
  ;; Seat configuration
  (greeter-session lightdm-configuration-greeter-session
                   (default "lightdm-gtk-greeter"))
  (xserver-command lightdm-configuration-xserver-command
                   (default (xorg-start-command)))
  (session-wrapper lightdm-configuration-session-wrapper
                   (default (xinitrc)))
  (autologin-user lightdm-configuration-autologin-user
                  (default ""))
  (default-session-name lightdm-configuration-default-session
    (default ""))
  (autologin-timeout lightdm-configuration-autologin-timeout
                     (default ""))
  ;; lightdm-gtk-greeter specifics
  ;; Maybe it should have its own service
  (gtk-greeter-assets lightdm-configuration-gtk-greeter-assets
                      (default (list adwaita-icon-theme
                                     gnome-themes-standard)))
  (gtk-greeter-theme-name lightdm-configuration-gtk-greeter-theme-name
                          (default "Adwaita"))
  (gtk-greeter-icon-theme-name
   lightdm-configuration-gtk-greeter-icon-theme-name
   (default "Adwaita"))
  (gtk-greeter-cursor-theme-name
   lightdm-configuration-gtk-greeter-cursor-theme-name
   (default "Adwaita"))
  (gtk-greeter-cursor-size lightdm-configuration-gtk-greeter-cursor-size
                           (default 16))
  (gtk-greeter-background lightdm-configuration-gtk-greeter-background
                          (default "")))

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
greeter-user = "          (lightdm-configuration-user config) "
greeters-directory = "    (lightdm-configuration-greeters-directory config) "
sessions-directory = "    (lightdm-configuration-sessions-directory config) "


[Seat:*]
xserver-command = "       (lightdm-configuration-xserver-command config) "
greeter-session = "       (lightdm-configuration-greeter-session config) "
user-session = "          (lightdm-configuration-default-session config) "
autologin-user = "        (lightdm-configuration-autologin-user config) "
autologin-session = "     (lightdm-configuration-default-session config) "
autologin-user-timeout = " (lightdm-configuration-autologin-timeout config) "
session-wrapper = " (lightdm-configuration-session-wrapper config)))


(define (lightdm-gtk-greeter-configuration-file config)
  (mixed-text-file "lightdm-gtk-greeter.conf" "
[greeter]
theme-name = "        (lightdm-configuration-gtk-greeter-theme-name config) "
icon-theme-name = "   (lightdm-configuration-gtk-greeter-icon-theme-name config) "
cursor-theme-name = " (lightdm-configuration-gtk-greeter-cursor-theme-name config) "
cursor-theme-size = " (number->string
                       (lightdm-configuration-gtk-greeter-cursor-size config)) "
background = "        (lightdm-configuration-gtk-greeter-background config)))


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
        `("xdg/lightdm/lightdm-gtk-greeter.conf"
          ,(lightdm-gtk-greeter-configuration-file config))))

(define (lightdm-pam-services config)
  (list (lightdm-pam-service config)
        (lightdm-greeter-pam-service)
        (lightdm-autologin-pam-service)))

(define (lightdm-profile-service config)
  (append (list my-lightdm-gtk-greeter lightdm)
          (lightdm-configuration-gtk-greeter-assets config)))

(define (lightdm-activation-service config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (define %user
          (getpw #$(lightdm-configuration-user config)))
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
                (default-value (lightdm-configuration))))
