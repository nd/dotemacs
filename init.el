;; entry point to my emacs customizations

(add-to-list 'load-path "~/.emacs.d/nd")

;;settings that require no 3rd party code, should work with plain emacs:
(require 'nd-common-settings)
(require 'nd-global-keys)
(require 'nd-decorations)
(require 'nd-ido-settings)
(require 'nd-shell-settings)
(require 'nd-dired-settings)
(require 'nd-woman-settings)
(require 'nd-environment-settings)
(require 'nd-nxml-settings)
(require 'nd-gpg-settings)
(require 'nd-server-settings)
(require 'nd-startup-settings)

;;settings that require something (maybe not required on every machine)
(require 'nd-git-settings)
;(require 'nd-emms-settings)
(require 'nd-scheme-settings)
(require 'nd-openwith-settings)
(require 'nd-auto-complete-settings)
;(require 'nd-dbus-settings)
;(require 'nd-wsdl-settings)
(require 'nd-clojure-settings)
(require 'nd-haskell-settings)
(require 'nd-erlang-settings)

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


