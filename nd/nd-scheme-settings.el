;;;; settings for scheme in emacs

(require 'xscheme)
(require 'quack)

(setq quack-pretty-lambda-p t)
(setq quack-remap-find-file-bindings-p nil)

(setq auto-mode-alist
      (append '(("\\.scm$" . scheme-mode))
              auto-mode-alist))

(define-key scheme-mode-map [(f5)] 'run-scheme)



(provide 'nd-scheme-settings)
