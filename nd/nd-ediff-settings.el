(require 'ediff)

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-recurse-to-subdirectories 'yes)
;(ediff-defvar-local ediff-recurse-to-subdirectories 'yes "")

(add-hook 'diff-mode-hook
          (lambda ()
             (define-key diff-mode-map (kbd "n") 'diff-hunk-next)
             (define-key diff-mode-map (kbd "N") 'diff-file-next)
             (define-key diff-mode-map (kbd "p") 'diff-hunk-prev)
             (define-key diff-mode-map (kbd "P") 'diff-file-prev)
             ))

(provide 'nd-ediff-settings)
