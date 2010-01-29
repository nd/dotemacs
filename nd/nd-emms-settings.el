;;;; emms settings

(if (equal system-type 'gnu/linux)
    (progn
      (add-to-list 'load-path "~/emacs/site-lisp/emms/lisp/")

      (require 'emms-setup)
      (emms-standard)
      (emms-default-players)
      (require 'emms-lastfm)

      (define-emms-simple-player mplayer '(file url)
        (concat "\\`\\(http\\|mms\\)://\\|"
                (emms-player-simple-regexp
                 "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
                 "mov" "avi" "divx" "ogm" "asf" "mkv" "m4v"
                 "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"))
        "mplayer" "-slave" "-quiet" "-really-quiet")

      (require 'emms-volume)
      (global-set-key "\C-cen" 'emms-next)
      (global-set-key "\C-cep" 'emms-previous)
      (global-set-key "\C-ces" 'emms-stop)
      (global-set-key "\C-cer" 'emms-start)
      (global-set-key "\C-ceh" 'emms-volume-mode-plus)
      (global-set-key "\C-cel" 'emms-volume-mode-minus)
      (global-set-key "\C-ced" 'emms-playlist-mode-go)

      (setq emms-playlist-buffer-name "*Music*")
;      (emms-add-directory-tree "/muz")
;      (emms-add-directory-tree "/video")
      (emms-add-url "http://207.200.96.228:8078/listen.pls")
      (setq emms-volume-amixer-control "PCM")

      (setq emms-cache-file "~/.emacs.d/tmp/emms.cache")))

(provide 'nd-emms-settings)