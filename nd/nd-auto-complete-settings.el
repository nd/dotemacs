;;;; auto-complete settings

(require 'auto-complete)
(global-auto-complete-mode nil)

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

(setq ac-auto-start t)
(global-set-key "\M- " 'ac-start)

(provide 'nd-auto-complete-settings)


