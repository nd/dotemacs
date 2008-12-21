;;;; shell settings

(let* ((name "bash")
       (shellname (concat "/bin/" name)))
  (setenv "ESHELL" shellname)
  (setenv "SHELL" shellname)
  (setenv "PAGER" "cat")
  (setq shell-file-name shellname)
  (setq explicit-shell-file-name name))

(defun nd-shell-keys ()
  "Add keybindings for shell mode."
  (define-key shell-mode-map [(f4)] 'bury-buffer)
  (define-key shell-mode-map "\M-?" 'help-command))

(add-hook 'shell-mode-hook 'nd-shell-keys)

;; run shell on F4
(defun nd-shell (&optional n)
  "Run shell n."
  (interactive "P")
  (if (not n)
      (shell)
    (shell (concat "*shell-" 
                   (number-to-string (prefix-numeric-value n)) 
                   "*"))))
(global-set-key  [(f4)] 'nd-shell)


(provide 'nd-shell-settings)