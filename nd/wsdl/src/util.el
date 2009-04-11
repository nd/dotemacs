;;utils


(defun create-object ()
  (lexical-let ((state (make-hash-table))
                (methods (make-hash-table)))

    ;; dispatch function
    (lambda (message &rest args)
      (cond ((eq message 'set) (puthash (nth 0 args) (nth 1 args) state))
            ((eq message 'get) (gethash (nth 0 args) state))
            ((eq message 'defmethod) (puthash (nth 0 args) (nth 1 args) methods))
            ((not (null (gethash message methods)))
             (let ((method (gethash message methods))
                   (this state))
               (apply method args)))
            (t (error (concat "Method " (symbol-name message)  " is not supproted")))))))

(defun set-prop (object prop value)
  (invoke object 'set prop value))

(defun get-prop (object prop value)
  (invoke object 'get prop))

(defun this. (name)
  (gethash name this))

(defun defmethod (object method-name lambda)
  (invoke object 'defmethod method-name lambda))

;(setq o (create-object))
;(invoke o 'set 'name "my first object")
;(invoke o 'get 'name)
;(invoke o 'defmethod 'get-name '(lambda () (concat "Result: " (this. 'name))))
;(invoke o 'defmethod 'print-name '(lambda () "name"))
;(invoke o 'get-name)

