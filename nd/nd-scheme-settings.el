;;;; settings for scheme in emacs

(require 'xscheme)
(require 'quack)

(setq quack-pretty-lambda-p t)

(setq auto-mode-alist
      (append '(("\\.scm$" . scheme-mode))
              auto-mode-alist))


(provide 'nd-scheme-settings)
