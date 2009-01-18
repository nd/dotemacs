;;;; open-with settings

(require 'openwith)
(openwith-mode t)

(setq openwith-associations
      '(("\\.pdf\\'" "evince" (file)) 
        ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file))
        ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file))))


(provide 'nd-openwith-settings)