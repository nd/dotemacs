(defconst tc-logs-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "j") 'tc-logs-next-entry)
    (define-key map (kbd "k") 'tc-logs-prev-entry)
    (define-key map (kbd "/") 'tc-logs-include)
    (define-key map (kbd "-") 'tc-logs-exclude)
    (define-key map (kbd "w") 'tc-logs-copy-entry)
    map))

(defun tc-logs-mode ()
  (interactive)
  (tc-logs-parse)
  (tc-logs-mode-enable))

(defun tc-logs-mode-enable ()
  (buffer-disable-undo)
  (setq major-mode 'tc-logs-mode
        mode-name "Logs Mode")
  (use-local-map tc-logs-mode-map)
  (run-hooks 'tc-logs-mode-hook)
  (setq buffer-read-only t))

(defmacro do (test &rest body)
  `(progn ,@body (while ,test ,@body)))

(defun tc-logs-parse ()
  (interactive)
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (let ((entry-num 0)
            (prev-entry-start nil))
        (do (= 0 (forward-line 1))
            (let* ((c1 (following-char))
                   (p (point))
                   (c2 (char-after (+ p 24))))
              (if (and (= c1 ?\[) (= c2 ?\]))
                  (progn
                    (put-text-property p (+ p 25) 'face 'bold)
                    (if prev-entry-start
                        (put-text-property prev-entry-start p 'tc-log-entry-num entry-num))
                    (setq prev-entry-start p)
                    (setq entry-num (+ entry-num 1)))))))))
  (set-buffer-modified-p nil))

(defun tc-logs-next-entry ()
  (interactive)
  (let ((p (next-single-property-change (point) 'tc-log-entry-num)))
    (and p (goto-char p))))

(defun tc-logs-prev-entry ()
  (interactive)
  (let ((p (previous-single-property-change (point) 'tc-log-entry-num)))
    (if p
        (goto-char p)
      (goto-char (point-min)))))

(defun tc-logs-copy-entry ()
  (interactive)
  (let ((p (or (previous-single-property-change (+ (point) 1) 'tc-log-entry-num) (point-min)))
        (n (or (next-single-property-change (point) 'tc-log-entry-num) (point-max))))
    (copy-region-as-kill p n)))

(defun tc-logs-include (term)
  (interactive "MInclude: ")
  (tc-logs-filter term #'(lambda (beg end) (re-search-forward term end t))))

(defun tc-logs-exclude (term)
  (interactive "MExclude: ")
  (tc-logs-filter term #'(lambda (beg end) (not (re-search-forward term end t)))))

(defun tc-logs-filter (filter-buffer-name pred)
  (let ((orig-buffer (current-buffer))
        (filter-buffer (get-buffer-create (concat "*filter-log-" filter-buffer-name "*"))))
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((p (point))
                 (next (or (next-single-property-change p 'tc-log-entry-num) (point-max))))
            (if (funcall pred p next)
                (with-current-buffer filter-buffer
                  (insert-buffer-substring orig-buffer p next)))
            (goto-char next)))))
    (set-buffer filter-buffer)
    (goto-char (point-min))
    (tc-logs-mode-enable)
    (switch-to-buffer filter-buffer)))

(provide 'tc-logs)
