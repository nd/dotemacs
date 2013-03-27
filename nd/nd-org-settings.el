(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(setq org-return-follows-link 't)

(require 'org-drill)
(provide 'nd-org-settings)
