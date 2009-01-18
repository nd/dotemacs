;;;; common setup

(add-to-list 'load-path "~/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/vendor")

;; moving cursor down at bottom scrolls 
;; only a single line, not half page
(setq-default scroll-step 1)
(setq-default scroll-conservatively 0)
(setq-default scroll-margin 2)

;; typed text replaces the selection
(delete-selection-mode t)

;; y and n instead of yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;; avoid mouse then typing
(mouse-avoidance-mode 'banish)

;; cutting and pasting uses the clipboard
(setq x-select-enable-clipboard t)

;; autosave configuration
(setq auto-save-list-file-prefix "/tmp/emacs/auto-save-list/.saves-")

;; tramp history file
(setq tramp-persistency-file-name "/home/nd/tmp/tramp")

;; backup settings
(setq make-backup-files t)
(setq backup-directory-alist '((".*" . "~/backup"))
      delete-old-versions t
      kept-new-versions 4
      kept-old-versions 2)

;; bury *scratch* buffer instead of kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; create directories automaticaly 
;; if we open file in directory that doesn't exist yet
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))


(provide 'nd-common-settings)
