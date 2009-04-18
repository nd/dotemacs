;;;; auto-complete settings

(require 'auto-complete)

(setq ac-auto-start nil)
(global-set-key "\M- " 'ac-start) 

(define-key ac-complete-mode-map (kbd "M-x") 'execute-extended-command)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "C-g") 'ac-stop)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" 'ac-complete)


(provide 'nd-auto-complete-settings)


