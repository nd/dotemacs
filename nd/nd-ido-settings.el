;;;; ido settings

(require 'ido)
(setq ido-enable-flex-matching t)
(ido-mode t)

;; ido ignore buffers
(setq ido-ignore-buffers 
      '("^ " 
        "\\*Music\\*"
        "\\*Completions\\*"
        "\\*magit-.*"
        "\\*Buffer List\\*"))

(defun nd-ido-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map "\C-\M-h" 'ido-delete-backward-word-updir)
  (define-key ido-completion-map "\C-i"    'ido-copy-current-file-name)
  (define-key ido-completion-map "\C-n" 'ido-next-match)
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map " "    'ido-exit-minibuffer))

(add-hook 'ido-setup-hook 'nd-ido-keys)


(provide 'nd-ido-settings)