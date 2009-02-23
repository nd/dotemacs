;;;; startup settings

(defun nd-startup-hook ()
  (shell))

(add-hook 'emacs-startup-hook 'nd-startup-hook)


(provide 'nd-startup-settings)
