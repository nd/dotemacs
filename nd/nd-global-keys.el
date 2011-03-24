;;;; ket bindings for all modes

;; Stevey Yegge said it is more comfortable
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm"    'execute-extended-command)

(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-h"     'backward-delete-char-untabify)

;; C-h delete backward char while search
(define-key isearch-mode-map "\C-h" 'isearch-del-char)
(global-set-key "\C-\M-h"  'backward-kill-word)

;; bindings for search 
(global-set-key "\C-s"    'isearch-forward-regexp)
(global-set-key "\C-r"    'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)
(global-set-key "\M-%"    'query-replace-regexp)

;; revert buffer 
(global-set-key (kbd "C-c r") 'revert-buffer)
;; kill buffer
(defun kill-buffer-or-client ()
  "If buffer has clients - kill 'em, otherwise kill-buffer"
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (ido-kill-buffer)))
(global-set-key (kbd "C-x k") 'kill-buffer-or-client)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key "\C-x\C-h" 'help-command)
;; Help should search more than just commands (from emacs-starter-kit)
(global-set-key "\C-x\C-ha" 'apropos) 

;; I don't use set-goal-column
(global-unset-key "\C-x\C-n")
;; I don't use suspend-frame
(global-unset-key "\C-x\C-z")

;; scroll down
(global-set-key "\C-z" '(lambda () (interactive) (scroll-down -1)))
;; scroll up
(global-set-key "\C-\M-z" '(lambda () (interactive) (scroll-down 1)))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; cua is usefull for rectagle editing
(setq cua-enable-cua-keys nil)
(setq cua-highlight-region-shift-only nil) ;no transient mark mode
(setq cua-toggle-set-mark nil) ;original set-mark behavior, i.e. no transient-mark-mode
(cua-mode)

(defun nd-open-dir-other-buffer ()
  (interactive)
  (let* ((dir (nd-get-dir-to-open))
         (w (split-window-horizontally)))
    (select-window w)
    (dired dir)))

(defun nd-get-dir-to-open ()
  (or dired-directory
      (replace-regexp-in-string "[^/]*$" "" buffer-file-name)))

(global-set-key "\C-c\C-v" 'nd-open-dir-other-buffer)

(provide 'nd-global-keys)