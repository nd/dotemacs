
;; entry point to my emacs customizations

(add-to-list 'load-path "~/.emacs.d/nd")

(require 'nd-common-settings)
(require 'nd-global-keys)

(require 'nd-decorations)
(require 'nd-ido-settings)
;(require 'nd-emms-settings)
(require 'nd-shell-settings)
(require 'nd-git-settings)
;(require 'nd-dired-settings)
(require 'nd-woman-settings)
(require 'nd-environment-settings)
(require 'nd-nxml-settings)
(require 'nd-scheme-settings)
(require 'nd-server-settings)
(require 'nd-gpg-settings)
(require 'nd-openwith-settings)
(require 'nd-auto-complete-settings)
(require 'nd-dbus-settings)
(require 'nd-elib-settings)
(require 'nd-startup-settings)
;(require 'nd-wsdl-settings)
(require 'nd-clojure-settings)
(require 'nd-haskell-settings)
(require 'nd-erlang-settings)

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(quack-programs (quote ("mit" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t (:foreground "dark blue" :weight ultra-bold)))))
