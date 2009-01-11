;;;; emms settings

(if (equal system-type 'gnu/linux)
    (progn
      (add-to-list 'load-path "~/emacs/site-lisp/emms/lisp/")

      (require 'emms-setup)
      (emms-standard)
      (emms-default-players)

      (require 'emms-volume)
      (global-set-key "\C-cen" 'emms-next)
      (global-set-key "\C-cep" 'emms-previous)
      (global-set-key "\C-ces" 'emms-stop)
      (global-set-key "\C-cer" 'emms-start)
      (global-set-key "\C-ceh" 'emms-volume-mode-plus)
      (global-set-key "\C-cel" 'emms-volume-mode-minus)
      (global-set-key "\C-ced" 'emms-playlist-mode-go)

      (setq emms-playlist-buffer-name "*Music*")
      (emms-add-directory-tree "/muz")

      (setq emms-cache-file "/tmp/emms.cache")))

(provide 'nd-emms-settings)