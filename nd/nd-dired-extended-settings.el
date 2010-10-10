;;;; dired extended settings
;;;; (require dired-single)

(require 'dired-single)
(require 'dired-history)
(require 'dired-history-show)

(defun nd-dired-extended-keys ()
  (define-key dired-mode-map "" (lambda ()
                                    (interactive)
                                    (joc-dired-single-buffer "..")))
  (define-key dired-mode-map "^" (lambda ()
                                   (interactive)
                                   (joc-dired-single-buffer "..")))
  (define-key dired-mode-map (kbd "RET") (lambda ()
                                           (interactive)
                                           (joc-dired-single-buffer)))
  (define-key dired-mode-map (kbd "M-p") 'dired-history-prev)
  (define-key dired-mode-map (kbd "M-n") 'dired-history-next)
  (define-key dired-mode-map (kbd "M-h") 'dired-history-show-select))

(add-hook 'dired-mode-hook 'nd-dired-extended-keys)

(provide 'nd-dired-extended-settings)

