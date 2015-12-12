(require 'tc-logs)

(setq auto-mode-alist (append '(("\\.log$" . tc-logs-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.log.[[:digit:]]+$" . tc-logs-mode)) auto-mode-alist))

(provide 'nd-logs)
