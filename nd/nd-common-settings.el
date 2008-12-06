;;;; common setup

(add-to-list 'load-path "~/emacs/site-lisp")

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


(provide 'nd-common-settings)
