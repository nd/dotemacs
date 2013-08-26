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
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "W") 'dired-copy-full-filename-as-kill))

(defun dired-copy-full-filename-as-kill ()
  (interactive)
  (dired-copy-filename-as-kill 0))

(add-hook 'dired-mode-hook 'nd-dired-keys)

(require 'thread-dump)

(defun thread-dump-open-dired-dir ()
  (interactive)
  (thread-dump-open-dir (dired-current-directory)))

(defun thread-dump-open-marked-files ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (thread-dump-open-files files)))

(add-hook 'dired-mode-hook
          (lambda ()
             (define-key dired-mode-map (kbd "C-c t d") 'thread-dump-open-dired-dir)
             (define-key dired-mode-map (kbd "C-c t f") 'thread-dump-open-marked-files)
             ))

(provide 'nd-dired-settings)
