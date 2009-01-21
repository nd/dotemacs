;;;; util for working with sles

(defun nd-delete-deploy-dirs ()
  (interactive)
  (dired-delete-file "deployed" 'always)
  (dired-delete-file "httpTmp" 'always)
  (dired-delete-file "shared_classes" 'always)
  (revert-buffer nil t))

(define-key dired-mode-map "\C-csc" 'nd-delete-deploy-dirs)


(provide 'nd-sles)