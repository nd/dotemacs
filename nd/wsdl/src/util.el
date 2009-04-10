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
           (let ((method (gethash message methods)))
             (apply method args)))
          (t (error (concat "Method " (symbol-name message)  " is not supproted")))))))

(setq o (create-object))

(invoke o 'set 'name "my first object")
(invoke o 'get 'name)

(invoke o 'defmethod 'get-name '(lambda () (concat "Result: " (invoke o 'print-name))))
(invoke o 'defmethod 'print-name '(lambda () "name"))
(invoke o 'get-name)

(defmethod obj name
  code)

(defmacro defmethod (object name &rest body)
  `(invoke ,o ,name '(lambda (&rest args) (body))))

(macroexpand (defmethod o 'get-name nil))

