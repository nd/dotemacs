;;;; xsd parser


(require 'util)

(defun xsd/create-build-in-type (name sample)
  (lexical-let* ((name name)
                 (sample sample))

    ;; dispatch function
    (lambda (message &rest args)
      (cond ((eq message 'get-name) name)

            ((eq message 'get-sample) sample)

            ((eq message 'get-element-sample) 
             (let ((element-name (car args)))
               (concat
                "<" (invoke element-name 'get-localname) ">" 
                sample 
                "</" (invoke element-name 'get-localname) ">\n" )))

            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))

(defconst build-in-types
  (list 
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "string")
                             "string")
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "decimal") 
                             "1.0")
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "date") 
                             "1999-05-31")
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "NMTOKEN")
                             "US")
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "int")
                             "1")
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "dateTime")
                             "1999-05-31T13:20:00.000-05:00")))

(defun xsd/get-standart-schema ()
  (let* ((xsd (create-object))
         (targetNamespace "http://www.w3.org/2001/XMLSchema"))

    (set-prop xsd 'targetNamespace targetNamespace)
    (set-prop xsd 'simpleTypes build-in-types)

    (defmethod xsd 'get-element
      (lambda (element-name)
        (car (filter (this. 'elements)
                     (lambda (element) 
                       (invoke (invoke element 'get-name) 'equal element-name))))))

    (defmethod xsd 'get-type
      (lambda (type-name)
        (car (filter (append (this. 'simpleTypes) (this. 'complexTypes) build-in-types)
                     (lambda (type) 
                       (invoke (invoke type 'get-name) 'equal type-name))))))

    (defmethod xsd 'get-targetNamespace
      (lambda () (this. 'targetNamespace)))

    xsd))


(defun xsd/create-xsd (location-or-node)
  "Create xsd from it's node"
  (let* ((xsd (create-object))
         (schema-node (if (listp location-or-node)
                          location-or-node
                        (xml/parse-document-at-location location-or-node)))
         (targetNamespace (xml/get-attribute-value schema-node "targetNamespace"))
         (ns-aliases (xml/get-ns-aliases schema-node)))

    (set-prop xsd 'schema-node schema-node)
    (set-prop xsd 'targetNamespace targetNamespace)
    (set-prop xsd 'ns-aliases ns-aliases)
    (set-prop xsd 'elements
              (mapcar (lambda (node) (xsd/create-element targetNamespace ns-aliases node xsd))
                      (xml/get-elements-by-name schema-node '(:http://www.w3.org/2001/XMLSchema . "element"))))
    (set-prop xsd 'simpleTypes
              (mapcar (lambda (node) (xsd/create-simpleType targetNamespace ns-aliases node xsd))
                      (xml/get-elements-by-name schema-node '(:http://www.w3.org/2001/XMLSchema . "simpleType"))))
    (set-prop xsd 'complexTypes
              (mapcar (lambda (node) (xsd/create-complexType targetNamespace ns-aliases node xsd))
                      (xml/get-elements-by-name schema-node '(:http://www.w3.org/2001/XMLSchema . "complexType"))))

    (defmethod xsd 'get-element
      (lambda (element-name)
        (car (filter (this. 'elements)
                     (lambda (element) 
                       (invoke (invoke element 'get-name) 'equal element-name))))))

    (defmethod xsd 'get-type
      (lambda (type-name)
        (car (filter (append (this. 'simpleTypes) (this. 'complexTypes) build-in-types)
                     (lambda (type) 
                       (invoke (invoke type 'get-name) 'equal type-name))))))

    (defmethod xsd 'get-targetNamespace
      (lambda () (this. 'targetNamespace)))

    xsd))


(defun xsd/create-element (targetNamespace ns-aliases element-node xsd)
  "Create xsd element from it's node"
  (lexical-let* ((node element-node)
                 (xsd xsd)
                 (name (xml/new-qname targetNamespace (xml/get-attribute-value node "name")))
                 (type 
                  (if (xml/get-attribute-value node "type")
                      (xml/expand-qname (xml/get-attribute-value node "type") targetNamespace ns-aliases)
                    nil)))
    (message (concat "create element " (invoke name 'to-string)))
    ;; dispatch function
    (lambda (message &rest args)
      (cond ((eq message 'get-name) name)

            ((eq message 'get-type) type)

            ((eq message 'get-sample)
             (invoke (invoke xsd 'get-type type) 'get-element-sample (or (car args) name)))

            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))

(defun xsd/create-simpleType (targetNamespace ns-aliases simpleType-node xsd)
  "Create xsd simpleType from it's node"
  (lexical-let* ((node simpleType-node)
                 (xsd xsd)
                 (targetNamespace targetNamespace)
                 (ns-aliases ns-aliases)
                 (name (xml/new-qname targetNamespace (xml/get-attribute-value node "name")))
                 (restriction (car (xml/get-elements-by-name node '(:http://www.w3.org/2001/XMLSchema . "restriction"))))
                 (list (car (xml/get-elements-by-name node '(:http://www.w3.org/2001/XMLSchema . "list")))))

    ;; dispatch function
    (lambda (message &rest args)
      (cond ((eq message 'get-name) name)

            ((eq message 'get-element-sample) 
             (let ((element-name (car args)))

               (cond ((and restriction)
                      (let ((base (xml/expand-qname (xml/get-attribute-value restriction "base") 
                                                    targetNamespace ns-aliases)))
                        (invoke (invoke xsd 'get-type base) 'get-element-sample element-name)))

                     ((and list)
                      (let ((itemType (xml/expand-qname (xml/get-attribute-value list "itemType") 
                                                        targetNamespace ns-aliases)))
                        (invoke (invoke xsd 'get-type itemType) 'get-element-sample element-name)))

                     (t (concat "sample of " (invoke name 'to-string) "\n")))))

            ((eq message 'get-sample) 

               (cond ((and restriction)
                      (let ((base (xml/expand-qname (xml/get-attribute-value restriction "base") 
                                                    targetNamespace ns-aliases)))
                        (invoke (invoke xsd 'get-type base) 'get-sample)))

                     ((and list)
                      (let ((itemType (xml/expand-qname (xml/get-attribute-value list "itemType") 
                                                        targetNamespace ns-aliases)))
                        (invoke (invoke xsd 'get-type itemType) 'get-sample)))

                     (t (concat "sample of " (invoke name 'to-string) "\n"))))

            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun xsd/create-complexType (targetNamespace ns-aliases complexType-node xsd)
  "Create xsd complexType from it's node"
  (lexical-let* ((node complexType-node)
                 (targetNamespace targetNamespace)
                 (ns-aliases ns-aliases)
                 (xsd xsd)
                 (name (xml/new-qname targetNamespace (xml/get-attribute-value node "name")))
                 (sequence (car (xml/get-elements-by-name node '(:http://www.w3.org/2001/XMLSchema . "sequence"))))
                 (content (xml/get-elements-by-name 
                           sequence
                           '(:http://www.w3.org/2001/XMLSchema . "element")))
                 (attributes (xml/get-elements-by-name
                              node
                              '(:http://www.w3.org/2001/XMLSchema . "attribute"))))

    ;; dispatch function
    (lambda (message &rest args)
      (cond ((eq message 'get-name) name)

            ((eq message 'get-element-sample) 
             (let ((element-name (car args)))
                  
               (concat
                "<" (invoke element-name 'get-localname) 

                (apply 'concat
                       (mapcar 
                        (lambda (att-node) 
                          (invoke (xsd/create-attribute targetNamespace ns-aliases att-node xsd) 'get-sample))
                        attributes))

                ">\n" 
                (apply 'concat 

                       (mapcar
                        (lambda (n)
                          (cond ((xml/get-attribute-value n "name")
                                 (invoke (xsd/create-element targetNamespace ns-aliases n xsd) 'get-sample))
                                
                                ((not (null (xml/get-attribute-value n "ref")))
                                 (invoke (invoke xsd 'get-element
                                                 (xml/expand-qname
                                                  (xml/get-attribute-value n "ref")
                                                  targetNamespace ns-aliases))
                                         'get-sample))))

                        content))
                "</" (invoke element-name 'get-localname) ">\n")
               ))

            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun xsd/create-attribute (targetNamespace ns-aliases attribute-node xsd)
  "Create xsd attribute from it's node"
  (lexical-let* ((node attribute-node)
                 (name (xml/new-qname targetNamespace (xml/get-attribute-value node "name")))
                 (cur-xsd xsd)
                 (type 
                  (and (xml/get-attribute-value node "type")
                       (xml/expand-qname (xml/get-attribute-value node "type") targetNamespace ns-aliases))))

    ;; dispatch function
    (lambda (message)
      (cond ((eq message 'get-name) name)

            ((eq message 'get-type) type)

            ((eq message 'get-sample)
             (let ((att-type (invoke cur-xsd 'get-type type)))
               (concat " " (invoke name 'get-localname) "=" 
                     "\""
                     (invoke att-type 'get-sample)
                     "\"")))

            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun xsd/get-build-in-type (qname)
  (car (filter build-in-types
               (lambda (type) 
                 (invoke (invoke type 'get-name) 'equal qname)))))


(provide 'xsd)