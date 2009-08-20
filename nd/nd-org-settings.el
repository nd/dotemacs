(add-to-list 'load-path "~/.emacs.d/vendor/org/lisp")
(add-to-list 'load-path "~/.emacs.d/vendor/org/contrib/lisp")

(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELED(c)" "DISMISS(i)")))

(setq org-todo-keyword-faces
           '(("TODO"       . (:foreground "SaddleBrownorg" :weight bold))
             ("CANCELED"   . (:foreground "red"))
             ("DISMISS"    . (:foreground "red" :background "yellow" :weight bold))
             ("DONE"       . (:foreground "ForestGreen"))))

(setq org-clock-persist t)
(org-clock-persistence-insinuate)

(global-set-key "\C-cl" 'org-store-link)


(defun nd-org-dismiss-noticed ()
  "DISMISS all tasks with TODO keyword and status property =
on_notice tag in current file. Do it every morning as in
autofocus 2.0"
  (interactive)
  (org-map-entries
   '(lambda ()
      (org-todo "DISMISS")
      (org-entry-delete (point) "status"))
   "+status=\"on_notice\"/+TODO" 'file))

(defun nd-org-put-task-on-notice ()
  (interactive)
  (org-entry-put (point) "status" "on_notice"))


(defun nd-org-move-old-tasks-label ()
  "Move old tasks label (after this label all tasks are done or
canceled or dismissed) to the position after last active task."
  (interactive)
  (save-excursion
    (save-match-data
      (let ((last-active-task-line
             (car (last
                   (org-map-entries
                    '(lambda ()
                       (line-number-at-pos))
                    "/+TODO|+STARTED" 'file)))))
        (goto-line last-active-task-line)
        (org-forward-same-level 1)
        (open-line 1)
        (insert "* =====================old=====================")
        (re-search-forward "^* =====================old=====================" nil t)
        (beginning-of-line)
        (kill-line)
        (kill-line)))))


(provide 'nd-org-settings)

