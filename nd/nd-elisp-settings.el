;; Elisp go-to-definition with M-. and back again with M-,
(when (not (package-installed-p 'elisp-slime-nav))
  (package-install 'elisp-slime-nav))
(require 'elisp-slime-nav)
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
;(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))
