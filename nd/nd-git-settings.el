;;;; git settings
;;
;; if there is no magit on machine, run:
;; git clone http://github.com/philjackson/magit.git magit
;; cd magit
;; make; sudo make install

(require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)
;;show tags in log:
(setq magit-have-decorate t)

(provide 'nd-git-settings)