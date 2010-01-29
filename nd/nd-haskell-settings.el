(add-to-list 'load-path "~/.emacs.d/haskell-mode")
(load "~/.emacs.d/haskell-mode/haskell-site-file")

(defun nd/haskell-mode-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
;;  (turn-on-haskell-ghci)
;;  (turn-on-eldoc-mode)
;;  (local-set-key "\C-cl" 'hs-lint)
  (local-set-key "\C-ch" 'haskell-hoogle)
  (local-set-key "\C-c\C-h" 'haskell-hayoo)
  (setq tab-width 4)
;;  (turn-on-haskell-simple-indent)
  (setq haskell-font-lock-symbols t))
(add-hook 'haskell-mode-hook 'nd/haskell-mode-hook)

(setq haskell-program-name "ghci")

(setq auto-mode-alist (append '(("\\.hs$" . haskell-mode)) auto-mode-alist))

(provide 'nd-haskell-settings)