;;;; dbus settings and functions that use dbus

(require 'dbus)

(defun nd-show-notification (msg)
"Show short message (no body, only summary)"
  (nd-show-notification2 "" msg -1))

(defun nd-show-notification2 (body summary time) 
  "Show notification using dbus and org.freedesktop.Notifications.
body - body of notification message; summary - summary of notification; time - The timeout time in milliseconds since the display of the notification at which the notification should automatically close (-1 the notification's expiration time is dependent on the notification server's settings)"
  (dbus-call-method 
   :session 
   "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications"
   "org.freedesktop.Notifications" 
   "Notify"
   "GNU Emacs"                 ;; Application name.
   0                           ;; No replacement of other notifications.
   ""                          ;; No icon.
   (encode-coding-string summary 'utf-8-unix) ;;summary
   (encode-coding-string body    'utf-8-unix) ;; body
   '(:array)                   ;; No actions (empty array of strings).
   '(:array :signature "{sv}") ;; No hints (empty array of dictionary entries).
   ':int32 time))

(defun nd-laptop-hibernate ()
  (interactive)
  (dbus-call-method :session
                    "org.freedesktop.PowerManagement"
                    "/org/freedesktop/PowerManagement"
                    "org.freedesktop.PowerManagement" 
                    "Hibernate"))

(provide 'nd-dbus-settings)

