;;;; dired settings

(require 'dired-single)
(require 'dired-history)
(require 'dired-history-show)

(setq dired-recursive-deletes 'top
      dired-recursive-copies 'always
      dired-dwim-target t)

(setq ls-lisp-dirs-first t)

(defun nd-dired-keys ()
  (define-key dired-mode-map "" (lambda ()
                                    (interactive)
                                    (joc-dired-single-buffer "..")))
  (define-key dired-mode-map "^" (lambda ()
                                   (interactive)
                                   (joc-dired-single-buffer "..")))
  (define-key dired-mode-map (kbd "RET") (lambda ()
                                           (interactive)
                                           (joc-dired-single-buffer)))
  (define-key dired-mode-map "%n" 'find-name-dired)
  (define-key dired-mode-map "%N" (lambda (pattern)
                                    (interactive "Mpattern: ")
                                    (find-name-dired (dired-current-directory) pattern)))
  (define-key dired-mode-map "%s" 'find-grep-dired)
  (define-key dired-mode-map "%S" (lambda (pattern)
                                    (interactive "Mpattern: ")
                                    (find-grep-dired (dired-current-directory) pattern)))
  (define-key dired-mode-map (kbd "C-c C-o") 'dired-gnome-open-file)
  (define-key dired-mode-map (kbd "C-c C-r") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "M-p") 'dired-history-prev)
  (define-key dired-mode-map (kbd "M-n") 'dired-history-next)
  (define-key dired-mode-map (kbd "M-h") 'dired-history-show-select))

(add-hook 'dired-mode-hook 'nd-dired-keys)

(provide 'nd-dired-settings)

