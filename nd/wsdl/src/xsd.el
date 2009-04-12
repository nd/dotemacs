;;;; xsd parser


(defconst build-in-types
  (list 
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "string") "string")
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "decimal") "1.0")))


(defun xsd/create-xsd (location)
  "Create xsd from it's node"
  (let* ((xsd (create-object))
        (schema-node (xml/parse-document-at-location location))
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

    xsd))


(defun xsd/create-element (targetNamespace ns-aliases element-node xsd)
  "Create xsd element from it's node"
  (lexical-let* ((node element-node)
                 (name (xml/new-qname targetNamespace (xml/get-attribute-value node "name")))
                 (type 
                  (if (xml/get-attribute-value node "type")
                      (xml/expand-qname (xml/get-attribute-value node "type") targetNamespace ns-aliases)
                    nil)))

    ;; dispatch function
    (lambda (message)
      (cond ((eq message 'get-name) name)

            ((eq message 'get-type) type)

            ((eq message 'get-sample)
             (invoke (invoke xsd 'get-type type) 'get-element-sample name))

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
                 (content (xml/get-elements-by-name sequence '(:http://www.w3.org/2001/XMLSchema . "element"))))

    ;; dispatch function
    (lambda (message &rest args)
      (cond ((eq message 'get-name) name)

            ((eq message 'get-element-sample) 
             (let ((element-name (car args)))
                  
               (concat
                "<" (invoke element-name 'get-localname) ">\n" 
                (apply 'concat 

                       (mapcar
                        (lambda (n)
                          (cond ((xml/get-attribute-value n "name")
                                 (invoke (xsd/create-element targetNamespace ns-aliases n xsd) 'get-sample))
                                
                                ((xml/get-attribute-value n "ref")
                                 (invoke (invoke xsd 'get-element
                                                 (xml/expand-qname
                                                  (xml/get-attribute-value n "ref")
                                                  targetNamespace ns-aliases))
                                         'get-sample))))

                        content))
                "</" (invoke element-name 'get-localname) ">\n")
               ))

            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


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


(defun xsd/get-build-in-type (qname)
  (car (filter build-in-types
               (lambda (type) 
                 (invoke (invoke type 'get-name) 'equal qname)))))


(provide 'xsd)