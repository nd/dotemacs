;;;; git settings
;;
;; if there is no magit on machine, run:
;; git clone http://github.com/philjackson/magit.git magit
;; cd magit
;; make; sudo make install

(require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'nd-git-settings)