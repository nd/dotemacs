;;;; ket bindings for all modes

;; Stevey Yegge said it is more comfortable
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm"    'execute-extended-command)

(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-h"     'backward-delete-char-untabify)
;; C-h delete backward char while search
(define-key isearch-mode-map "\C-h" 'isearch-del-char)
(global-set-key "\C-\M-h"  'backward-kill-word)

;; bindings for isearch-*-regexp
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\M-%" 'query-replace-regexp)

(global-set-key "\M-?" 'help-command)
;; Help should search more than just commands (from emacs-starter-kit)
(global-set-key "\M-?a" 'apropos) 

;; I don't use set-goal-column
(global-unset-key "\C-x\C-n")
;; I don't use suspend-frame
(global-unset-key "\C-x\C-z")

;; scroll down
(global-set-key "\C-z" '(lambda () (interactive) (scroll-down -1)))
;; scroll up
(global-set-key "\C-\M-z" '(lambda () (interactive) (scroll-down 1)))

;; cua is usefull for rectagle editing
(setq cua-enable-cua-keys nil)
(setq cua-highlight-region-shift-only nil) ;no transient mark mode
(setq cua-toggle-set-mark nil) ;original set-mark behavior, i.e. no transient-mark-mode
(cua-mode)


(provide 'nd-global-keys)