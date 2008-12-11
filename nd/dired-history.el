(require 'dired-single)

;;public

(defvar dired-history-history-size
  60
  "*Size of the history.")

(defun dired-history-next ()
  "Go to next dir in the history."
  (interactive)
  (let* ((history (dired-history-get-history))
         (index (dired-history-get-history-index))
         (next-index (- index 1)))
    (if (>= next-index 0)
        (progn
          (setq dired-history-history-index next-index)
          (dired-history-set-history-marker)
          (joc-dired-single-buffer (dired-history-get-nth-elem next-index history)))
      (message "This is the end of the history"))))


(defun dired-history-prev ()
  "Go to previous dir in the history."
  (interactive)
  (let* ((history (dired-history-get-history))
         (index (dired-history-get-history-index))
         (prev-index (+ index 1)))
    (if (< prev-index (dired-history-history-length history))
        (progn
          (setq dired-history-history-index prev-index)
          (dired-history-set-history-marker)
          (joc-dired-single-buffer (dired-history-get-nth-elem prev-index history)))
      (message "This is the start of the history"))))


(defun dired-history-goto-index (index)
  "Go to dir with this index in the history."
  (interactive)
  (let* ((history (dired-history-get-history)))
    (if (and (<  index (dired-history-history-length history))
             (>= index 0))
        (progn
          (setq dired-history-history-index index)
          (dired-history-set-history-marker)
          (joc-dired-single-buffer (dired-history-get-nth-elem index history)))
      (message "This is the start of the history"))))



;;private

(defun dired-history-get-history ()
  "Get history for current dired buffer.
If there is no history - create new one."
  (if (not (local-variable-if-set-p 'dired-history-history (current-buffer)))
      (dired-history-create-history))
  (buffer-local-value 'dired-history-history (current-buffer)))


(defun dired-history-get-history-index ()
  "Get history index for current dired buffer.
If there is no history index - create new one."
  (if (not (local-variable-if-set-p 'dired-history-history-index
                                    (current-buffer)))
      (dired-history-create-history-index)
    (buffer-local-value 'dired-history-history-index (current-buffer))))


(defun dired-history-create-history (&optional history)
  "Create history for current buffer.
Create it as local variable for current buffer. If optional
argument history is not null local history will be set to this
value, otherwise history will contain current dir only."
  (make-local-variable 'dired-history-history)
  (if (not (null history))
      (setq dired-history-history history)
    (setq dired-history-history
          (dired-history-make-history
           (dired-history-expand-dir-name dired-directory)))))


(defun dired-history-create-history-index (&optional history-index)
  "Create history-index for current buffer.
Create it as local variable for current buffer. If optional
argument history-index is not null local history-index will be set to this
value, otherwise history will be set to 0."
  (make-local-variable 'dired-history-history-index)
  (if (not (null history-index))
      (setq dired-history-history-index history-index)
    (setq dired-history-history-index 0)))


(defun dired-history-set-history-marker ()
  "Create history marker
as local variable in current dired buffer, so advice can read it."
  (make-local-variable 'dired-history-history-marker)
  (setq dired-history-history-marker 't))


(defun dired-history-move-in-history? ()
  "Check if history marker set in this buffer."
  (local-variable-if-set-p 'dired-history-history-marker (current-buffer)))


(defun dired-history-move-up-in-tree? (current-dir next-dir)
  "Tests if we move up in directory tree."
  (and (not (null (posix-string-match (regexp-quote next-dir) current-dir)))
       (= (posix-string-match (regexp-quote next-dir) current-dir) 0)))


(defadvice joc-dired-single-buffer (around dired-history-around-advice activate)
  (let ((history (dired-history-get-history))
        (index (dired-history-get-history-index))
        (in-history (dired-history-move-in-history?))
        (before-dir (dired-history-expand-dir-name
                     (dired-history-expand-dir-name dired-directory)))
        (destination-dir (ad-get-arg 0))
;        (next-dir (dired-get-filename))
        )

    (message (concat "before: " before-dir))
    (message (concat "dest: " destination-dir))

    ad-do-it

    (if in-history
        ;;just copy old values of history and index:
        (progn
          (dired-history-create-history history)
          (dired-history-create-history-index index))
      ;;create new history object:
      (dired-history-create-history
       (if (equal (dired-history-expand-dir-name dired-directory)
                   (dired-history-get-nth-elem index history))
           (dired-history-get-nthcdr index history)
         (dired-history-add-dir (dired-history-expand-dir-name dired-directory)
                                (dired-history-get-nthcdr index history))))
      ;;create new history index:
      (dired-history-create-history-index 0))

    ;;move cursor to previous dir if browse up in the tree:
    (let ((after-dir
           (dired-history-expand-dir-name
            (dired-history-expand-dir-name dired-directory))))
      (if (dired-history-move-up-in-tree? before-dir after-dir)
          (dired-history-goto-dir before-dir)))))


(defun dired-history-goto-dir (dir-name)
  "Move cursor to dir with dir-name."
  (let ((prev-dir-name (dired-history-get-short-name-of-dir dir-name)))
    (if (search-forward-regexp (concat "^  d.* "
                                       (regexp-quote prev-dir-name)
                                       "$") nil t)
        (search-backward-regexp (regexp-quote prev-dir-name) nil t))))


(defun dired-history-expand-dir-name (dir-name)
  "Get absolute name of dir, without / in the end."
  (directory-file-name (file-truename dir-name)))


(defun dired-history-get-short-name-of-dir (dir-long-name)
  "Return short name of dir."
  (replace-regexp-in-string "^\\(.\\|/\\)*/"
                            ""
                            (directory-file-name
                             (file-truename dir-long-name))))


(defun dired-history-get-nth-elem (n history)
  "Get nth element from history."
  (nth n history))


(defun dired-history-get-nthcdr (n history)
  "Get nth tail of history."
  (nthcdr n history))


(defun dired-history-make-history (first-element)
  "Create new history object with first-element in it."
  (list first-element))


(defun dired-history-history-length (history)
  "Get number of elements in the history."
  (length history))


(defun dired-history-add-dir (dir history)
  (if (< (length history) dired-history-history-size)
      (cons dir history)
    (cons dir (butlast history))))


(provide 'dired-history)


