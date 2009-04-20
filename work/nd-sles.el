;;;; util for working with sles

(defconst sles-dir "d:/Workspace/Bercut/SDEP/SLES")


(defun nd-delete-deploy-dirs ()
  (interactive)
  (message "cleaning sles")
  (ignore-errors (dired-delete-file "deployed" 'always))
  (ignore-errors (dired-delete-file "test" 'always))
  (ignore-errors (dired-delete-file "tmp" 'always))
  (ignore-errors (dired-delete-file "shared_classes" 'always))
  (revert-buffer nil t)
  (message "cleaning is finished"))

(define-key dired-mode-map "\C-csc" 'nd-delete-deploy-dirs)
(global-set-key "\C-csd" '(lambda () (interactive) (dired sles-dir)))


(provide 'nd-sles)