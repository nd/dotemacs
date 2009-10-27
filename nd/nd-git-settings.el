;;;; git settings

(add-to-list 'load-path "~/.emacs.d/elpa/magit-0.7")
(require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'nd-git-settings)