(add-to-list 'load-path "~/.emacs.d/vendor/org/lisp")
(add-to-list 'load-path "~/.emacs.d/vendor/gnuplot")
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


(defun get-history-data ()
  "Returns list of task velocities"
  (interactive)
  (org-clock-sum)
  (org-map-entries
   '(lambda ()
      (let ((actual (get-text-property (point) :org-clock-minutes)) 
            ;;(org-entry-get (point) "CLOCKSUM") should work, but don't
            (estimate (org-entry-get (point) "ESTIMATE")))
        (/ (float (hm->minutes estimate))
               (float actual))))
   "/+DONE" 'file))


(defun get-new-estimates ()
  "Returns list of new tasks estimate times in minutes"
  (interactive)
  (org-map-entries
   '(lambda ()
      (let ((estimate (org-entry-get (point) "ESTIMATE")))
        (hm->minutes estimate)))
   "+ESTIMATE={.+}/+TODO" 'file))


(defun hm->minutes (hm)
  "From string in hh:mm format to minutes"
  (if (string-match "\\([0-9]+\\):\\([0-9]\\{2\\}\\)" hm)
      (let ((hours   (string-to-number (match-string 1 hm)))
            (minutes (string-to-number (match-string 2 hm))))
        (+ (* hours 60) minutes))
      (error "Wrong time format -- " hm)))

(defun minutes->hm (minutes)
  "From minutes to hh:mm format"
  (let* ((hours (truncate (/ minutes 60)))
         (mins  (round (- minutes (* 60 hours)))))
    (concat (number-to-string hours)
            ":"
            (and (< mins 10) "0")
            (number-to-string mins))))


(defun random-element (list)
  "Get random element from list"
  (let* ((list-size (length list))
         (random-element-index (random list-size)))
    (nth random-element-index list)))


(setq data 
      '(((actual "2:00") (estimate "1:00"))
        ((actual "1:00") (estimate "0:40"))
        ((actual "7:10") (estimate "0:30"))
        ((actual "2:30") (estimate "1:00"))
        ((actual "2:50") (estimate "1:30"))
        ((actual "2:20") (estimate "2:00"))))


;;velocities
(mapcar (lambda (record) 
          (let ((actual   (cadr (assoc 'actual record)))
                (estimate (cadr (assoc 'estimate record))))
            (/ (float (hm->minutes estimate))
               (float (hm->minutes actual)))))
        data)

(defun predict ()
  (interactive)
  (let* ((velocities (get-history-data))
         (predicted-sum-times 
          (loop repeat 100
                collect 
                (apply '+ 
                       (mapcar (lambda (estimate) 
                                 (/ (float estimate)
                                    (random-element velocities)))
                               (get-new-estimates)))))
         (max-minutes (apply 'max predicted-sum-times)))
    (with-temp-buffer
      (insert "set terminal wxt persist\n")
      (insert "set grid back\n") 
      (insert "plot '-' using 1:2 title 'probability' smooth bezier with lines y1=90\n")
      (mapcar (lambda (time)
                (let ((probability (get-probability-for-time predicted-sum-times time)))
                  (insert (format "%3d %.2f\n" 
                                  (/ time (* 8 60))
                                  probability))
                  probability))
              (loop for x from 0 to (+ max-minutes (* 2 8 60)) by (* 8 60) collect x))
      (insert "e")
      (gnuplot-mode)
      (gnuplot-send-buffer-to-gnuplot)
      )))


(setq velocities
      '(0.5 0.6666666666666666 0.06976744186046512 0.4 0.5294117647058824 0.8571428571428571))

(setq new-estimates '(60 120 180 60 120 180 60 120 180 60 120 180 60 120 180 60 120 180))

(setq predicted-summary-times
      (loop repeat 100
            collect 
            (apply '+ (mapcar (lambda (estimate) 
                                (/ (float estimate)
                                   (random-element velocities)))
                              new-estimates))))

;;функция распределения p(x), x < t
(let ((max-minutes (apply 'max predicted-summary-times))
;      (tmp-file-name (make-temp-file "prediction-plot"))
      )
  (with-temp-buffer
    (insert "set terminal wxt persist\n")
    (insert "plot '-' u 1:2 with lines\n")
    (mapcar (lambda (time)
            (let ((probability (get-probability-for-time time)))
              (insert (format "%3d %.2f\n" 
                              (/ time (* 8 60))
                              probability))
              probability))
          (loop for x from 0 to max-minutes by (* 8 60) collect x))
;    (set-visited-file-name tmp-file-name t)
;    (save-buffer)
    (insert "e")
    (gnuplot-mode)
    (gnuplot-send-buffer-to-gnuplot)
;    (call-process-shell-command (concat "gnuplot " tmp-file-name))
;    (delete-file tmp-file-name)
;    (gnuplot-send-string-to-gnuplot
;     (concat "set terminal wxt persist,"
;             "plot " tmp-file-name " using 1:2 with lines, "" using 1:2 with boxes")
;     'file)
    ))

(defun get-probability-for-time (times time)
  (count time times :test (lambda (time x) (< x time))))


(provide 'nd-org-settings)



