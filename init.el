;; entry point to my emacs customizations

(add-to-list 'load-path "~/.emacs.d/nd")

(require 'nd-common-settings)
(require 'nd-global-keys)

(require 'nd-decorations)
(require 'nd-ido-settings)
(require 'nd-emms-settings)
(require 'nd-shell-settings)
(require 'nd-git-settings)