(add-to-list 'load-path "~/.emacs.d/vendor/org/lisp")
(add-to-list 'load-path "~/.emacs.d/vendor/org/contrib/lisp")

(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "ON_NOTICE(n)" "|" "DONE(d)" "CANCELED(c)" "DISMISS(i)")))

(setq org-todo-keyword-faces
           '(("TODO"       . org-warning)
             ("ON_NOTICE"  . (:foreground "yellow" :background "green" :weight bold))
             ("CANCELED"   . (:foreground "red"))
             ("DISMISS"    . (:foreground "green" :background "red" :weight bold))))

(setq org-clock-persist t)
(org-clock-persistence-insinuate)

(provide 'nd-org-settings)

