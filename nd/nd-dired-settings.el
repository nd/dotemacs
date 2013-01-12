;;;; dired settings

(setq dired-recursive-deletes 'top
      dired-recursive-copies 'always
      dired-dwim-target t)

(setq ls-lisp-dirs-first t)

(defun nd-dired-keys ()
  (define-key dired-mode-map "%n" 'find-name-dired)
  (define-key dired-mode-map "%N" 
    (lambda (pattern)
      (interactive "Mpattern: ")
      (find-name-dired (dired-current-directory) pattern)))
  (define-key dired-mode-map "%s" 'find-grep-dired)
  (define-key dired-mode-map "%S"
    (lambda (pattern)
      (interactive "Mpattern: ")
      (find-grep-dired (dired-current-directory) pattern)))
  (define-key dired-mode-map (kbd "C-c C-o") 'dired-gnome-open-file)
  (define-key dired-mode-map (kbd "C-c C-r") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line))

(add-hook 'dired-mode-hook 'nd-dired-keys)

;; Make dired less verbose
(require 'package)
(when (not (package-installed-p 'dired-details))
  (package-install 'dired-details))
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

(provide 'nd-dired-settings)
