;;;; ket bindings for all modes

;; Stevey Yegge said it is more comfortable
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm"    'execute-extended-command)

(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-h"     'backward-delete-char-untabify)
(global-set-key "\C-\M-h"  'backward-kill-word)

;; bindings for isearch-*-regexp
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\M-%" 'query-replace-regexp)

(global-set-key "\M-?" 'help-command)

;; I don't use set-goal-column
(global-unset-key "\C-x\C-n")
;; I don't use suspend-frame
(global-unset-key "\C-x\C-z")

;; scroll down
(global-set-key "\C-z" '(lambda () (interactive) (scroll-down -1)))
;; scroll up
(global-set-key "\C-\M-z" '(lambda () (interactive) (scroll-down 1)))


(provide 'nd-global-keys)