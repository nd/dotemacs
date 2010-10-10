;;;; ido settings

(require 'ido)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point nil
      ido-create-new-buffer 'always)
(ido-mode t)

;; don't do merge:
(setq ido-auto-merge-inhibit-characters-regexp ".*")

;; ido ignore buffers
(setq ido-ignore-buffers 
      '("^ " 
        "\\*Music\\*"
        "\\*Completions\\*"
        "\\*magit-.*"
        "\\*Buffer List\\*"))

;; use ido to open recent files using C-x C-r
(recentf-mode +1)
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key "\C-x\C-r" 'recentf-ido-find-file)

(defun nd-ido-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map "\C-\M-h" 'ido-delete-backward-word-updir)
  (define-key ido-completion-map "\C-i"    'ido-copy-current-file-name)
  (define-key ido-completion-map "\C-n"    'ido-next-match)
  (define-key ido-completion-map "\C-p"    'ido-prev-match)
  (define-key ido-completion-map " "       'ido-exit-minibuffer)
  (define-key ido-completion-map "\t"      'ido-exit-minibuffer))

(add-hook 'ido-setup-hook 'nd-ido-keys)


(provide 'nd-ido-settings)