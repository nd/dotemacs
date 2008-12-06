(require 'dired-history)
(require 'ido)
(require 'cl)

;;public

(defun dired-history-show-select ()
  "Select directory to go from history using ido."
  (interactive)
  (dired-history-goto-index (dired-history-show-show-history)))


;;private

(defun dired-history-show-show-history ()
  ;set up ido:
  (let* ((ido-decorations (list "\n"  ""
                                "\n"  " | ..." 
                                "\n[" "]" 
                                " [No match]"
                                " [Matched]"
                                " [Not readable]"
                                " [Too big]"))
         (selection
          (ido-completing-read "History:"
                               (dired-history-show-prepare-history
                                (dired-history-get-history))
                               nil t)))
    (dired-history-show-get-index-from-selection selection)))


(defun dired-history-show-prepare-history (history)
  (do ((x history (cdr x))
       (i 1 (+ i 1))
       (result nil 
               (cons (concat (int-to-string i) ": " (car x))
                     result)))
      ((null x)
       (nreverse result))))


(defun dired-history-show-get-index-from-selection (selection)
  (string-match "^\\(.*\\): " selection)
  (let ((end (match-end 1)))
    (- (string-to-number (substring selection 0 end)) 1)))


(provide 'dired-history-show)