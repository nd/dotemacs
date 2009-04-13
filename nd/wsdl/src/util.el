;;util

(require 'nxml-parse)

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

(defun invoke (object method &rest args)
  (apply object method args))

(defun xml/new-qname (namespace localname)
  (lexical-let ((this-namespace namespace)
                (this-name localname))
    (lambda (message &rest args)
      (cond ((eq message 'get-localname) this-name)
            ((eq message 'get-namespace)  this-namespace)
            ((eq message 'equal) 
             (cond ((or (null args) (null (car args))) nil)
                   (t (and (equal this-namespace (invoke (car args) 'get-namespace))
                           (equal this-name (invoke (car args) 'get-localname))))))
            ((eq message 'to-string) (concat "{" this-namespace ":" this-name "}"))
            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))

(defun xml/get-elements-by-name (parent-node name)
  (filter (nxml-node-children parent-node)
          (lambda (n)
            (nxml/name-equal n name))))

(defun xml/get-attribute-value (node attribute-name)
  (cdr (car (filter (nxml-node-attributes node)
                    (lambda (x) (equal (car x) attribute-name))))))

(defun nxml/name-equal (node name)
  (let ((node-name (nxml-node-name node)))
    (cond ((and (stringp node-name) (stringp name))
           (equal node-name name))

          ((and (listp node-name) (listp name))
           (and (equal (symbol-name (car name))
                       (symbol-name (car node-name)))
                (equal (cdr name) (cdr node-name))))

          (t nil))))

(defun xml/get-ns-aliases (node)
  (let ((attributes (nxml-node-attributes node)))
    (mapcar (lambda (a) (cons (cdar a) (cdr a)))
            (filter attributes
                    (lambda (attr) 
                      (and (listp (nxml-node-name attr))
                           (eq (car (nxml-node-name attr)) 
                               :http://www.w3.org/2000/xmlns/)))))))

(defun wsdl/get-ns->location (node)
  (mapcar 
     (lambda (import) 
       (cons (xml/get-attribute-value import "namespace") 
             (xml/get-attribute-value import "location")))
     (xml/get-elements-by-name node '(:http://schemas.xmlsoap.org/wsdl/ . "import"))))

(defun nxml-node-name (node)
  (car node))

(defun nxml-node-attributes (nxml-node)
  (cadr nxml-node))

(defun nxml-node-children (nxml-node)
  (filter (cddr nxml-node) (lambda (n) (listp n))))

(defun filter (list predicat)
  "TODO: make it better"
  (if (null list)
      '()
    (if (funcall predicat (car list))
        (append (list (car list)) (filter (cdr list) predicat))
      (filter (cdr list) predicat))))

(defun xml/expand-qname (string-value targetNamespace ns-aliases)
  (let ((split-list (split-string string-value ":")))
    (if (and (not (null split-list)) (eq (length split-list) 2))
      (xml/new-qname (cdr (assoc (car split-list) ns-aliases))
                     (cadr split-list))
    (xml/new-qname targetNamespace string-value))))

(defun util/select (object-list to-string-function prompt)
  "Select from item from object-list presented at minibuffer using ido"
  (let* ((ido-decorations (list "\n"  ""
                                "\n"  " | ..." 
                                "\n[" "]" 
                                " [No match]"
                                " [Matched]"
                                " [Not readable]"
                                " [Too big]"))
         (map (make-hash-table))
         (selection
          (ido-completing-read prompt
                               (mapcar (lambda (x) 
                                         (let ((str-value (funcall to-string-function x)))
                                           (puthash str-value x map)
                                           str-value))
                                       object-list)
                               nil t)))
    (gethash selection map)))

(defun xml/parse-document-at-location (location)
  (if (or (equal (elt (url-generic-parse-url location) 1) "http")
          (equal (elt (url-generic-parse-url location) 1) "https")
          (equal (elt (url-generic-parse-url location) 1) "ftp"))
      ;;this is valid url.. do something
      (xml/parse-file-from-location location)
    ;;asume this is local file path 
    (nxml-parse-file location)))

(defun xml/expand-location (location base-location)
  "Expand location from import"
  (cond ((elt (url-generic-parse-url location) 1) location) ;;location is url - simply return it

        ((and (elt (url-generic-parse-url base-location) 1) ;; base-location is url
              (not (elt (url-generic-parse-url location) 1))) ;; and imported location is not
         (error (concat "wrong import location " location)))

        ((and (not (elt (url-generic-parse-url base-location) 1)) ;; base-location is file
              (not (elt (url-generic-parse-url location) 1))) ;; and imported location is file
         
         (if (file-name-absolute-p location)
             location
           (concat (file-name-directory (expand-file-name base-location)) location)))))

(defun xml/parse-file-from-location (location)
  (let ((buf (url-retrieve-synchronously location))
        (tmp-filename (make-temp-file "soap-request-builder")))
    (set-buffer buf)
    (xml/delete-http-header)
    (set-visited-file-name tmp-filename t)
    (save-buffer)
    (let ((result (nxml-parse-file tmp-filename)))
      (delete-file tmp-filename)
      result)))

(defun xml/delete-http-header ()
  "Delete http header from current buffer"
  (goto-char (point-min))
  (while (not (looking-at "^$"))
    (kill-line)
    (next-line))
  (next-line)
  (delete-region (point) (point-min)))

(provide 'util)

