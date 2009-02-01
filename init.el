;; entry point to my emacs customizations

(add-to-list 'load-path "~/.emacs.d/nd")

(require 'nd-common-settings)
(require 'nd-global-keys)

(require 'nd-decorations)
(require 'nd-ido-settings)
(require 'nd-emms-settings)
(require 'nd-shell-settings)
(require 'nd-git-settings)
(require 'nd-dired-settings)
(require 'nd-woman-settings)
(require 'nd-environment-settings)
(require 'nd-nxml-settings)
(require 'nd-scheme-settings)
(require 'nd-server-settings)
(require 'nd-gpg-settings)
(require 'nd-openwith-settings)
(require 'nd-auto-complete-settings)
