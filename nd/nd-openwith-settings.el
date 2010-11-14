;;;; open-with settings

(require 'openwith)
(openwith-mode t)

(setq openwith-associations
      '(("\\.\\(?:pdf\\|djvu\\)\\'" "evince" (file)) 
        ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file))
        ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file))
        ("\\.chm\\'" "xchm" (file))
        ;("\\.html?\\'" "firefox" (file))
        ))


(provide 'nd-openwith-settings)