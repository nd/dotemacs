;;;; xsd parser

(require 'util)


(defun xsd/create-build-in-type (name sample)
  "Create build-in xsd type"
  (let ((type (create-object)))
    (set-prop type 'name name)
    (set-prop type 'sample sample)

    (defmethod type 'get-name (lambda () (this. 'name)))
    (defmethod type 'get-sample (lambda () (this. 'sample)))
    (defmethod type 'get-element-sample 
      (lambda (element-name)
        (concat
         "<" (invoke element-name 'get-localname) ">" 
         (this. 'sample) 
         "</" (invoke element-name 'get-localname) ">\n" )))
    type))


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
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "integer")
                             "1")
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "dateTime")
                             "1999-05-31T13:20:00.000-05:00")
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "boolean")
                             "true")))

(defun xsd/get-standart-schema ()
  (let* ((xsd (create-object))
         (targetNamespace "http://www.w3.org/2001/XMLSchema"))

    (set-prop xsd 'targetNamespace targetNamespace)

    (defmethod xsd 'get-element (lambda (element-name) (list)))

    (defmethod xsd 'get-type
      (lambda (type-name)
        (car (filter build-in-types
                     (lambda (type) (invoke (invoke type 'get-name) 'equal type-name))))))

    (defmethod xsd 'get-targetNamespace (lambda () (this. 'targetNamespace)))

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
  (let* ((element (create-object))
         (name (xml/new-qname targetNamespace (xml/get-attribute-value element-node "name")))
         (type 
          (if (xml/get-attribute-value element-node "type")
              (xml/expand-qname (xml/get-attribute-value element-node "type") targetNamespace ns-aliases)
            nil)))

    (set-prop element 'name name)
    (set-prop element 'type type)
    (set-prop element 'xsd  xsd)
    
    (defmethod element 'get-name (lambda () (this. 'name)))
    (defmethod element 'get-type (lambda () (this. 'type)))
    (defmethod element 'get-sample
      (lambda (&optional name-for-sample)
        (let ((elem-type (invoke (this. 'xsd) 'get-type (this. 'type))))
          (if (null elem-type)
              (error (concat "Type " (invoke type 'to-string) " not found"))
            (invoke elem-type 'get-element-sample (or name-for-sample (this. 'name)))))))

    element))


(defun xsd/create-simpleType (targetNamespace ns-aliases simpleType-node xsd)
  "Create xsd simpleType from it's node"
  (let* ((simpleType (create-object))
         (name (xml/new-qname targetNamespace (xml/get-attribute-value simpleType-node "name")))
         (restriction (car (xml/get-elements-by-name node '(:http://www.w3.org/2001/XMLSchema . "restriction"))))
         (list (car (xml/get-elements-by-name node '(:http://www.w3.org/2001/XMLSchema . "list"))))
         (union (car (xml/get-elements-by-name node '(:http://www.w3.org/2001/XMLSchema . "union")))))

    (set-prop simpleType 'name name)
    (set-prop simpleType 'xsd  xsd)

    (cond ((and restriction)
           (let ((base (xml/expand-qname (xml/get-attribute-value restriction "base") 
                                         targetNamespace ns-aliases)))
             (set-prop simpleType 'use-restriction t)
             (set-prop simpleType 'restriction-base base)))

          ((and list)
           (let ((itemType (xml/expand-qname (xml/get-attribute-value list "itemType")
                                             targetNamespace ns-aliases)))
             (set-prop simpleType 'use-list t)
             (set-prop simpleType 'itemType itemType)))

          ((and union)
           (do-smth-else)))

    (defmethod simpleType 'get-name (lambda () (this. 'name)))

    (defmethod simpleType 'get-parent-type
      (lambda ()
        (cond ((this. 'use-restriction)
               (let ((base-type (invoke (this. 'xsd) 'get-type (this. 'restriction-base))))
                 (if (null base-type)
                     (error (concat "Type " (invoke base-type 'to-string) " not found"))
                   base-type)))
              ((this. 'use-list)
               (let ((item (invoke (this. 'xsd) 'get-type (this. 'itemType))))
                 (if (null item)
                     (error (concat "Type " (invoke item 'to-string) " not found"))
                   item)))
              ;; other cases
              )))

    (defmethod simpleType 'get-sample
      (lambda ()
        (invoke (invoke 'this 'get-parent-type) 'get-sample)))

    (defmethod simpleType 'get-element-sample
      (lambda (element-name)
        (invoke (invoke 'this 'get-parent-type) 'get-element-sample element-name)))

    simpleType))


(defun xsd/create-complexType (targetNamespace ns-aliases complexType-node xsd)
  "Create xsd complexType from it's node"
  (let* ((complexType (create-object))
         (name (xml/new-qname targetNamespace (xml/get-attribute-value complexType-node "name")))
         (sequence (car (xml/get-elements-by-name complexType-node '(:http://www.w3.org/2001/XMLSchema . "sequence"))))

         (elements (xml/get-elements-by-name sequence '(:http://www.w3.org/2001/XMLSchema . "element")))

         (attributes 
          (mapcar 
           (lambda (attr-node) 
             (xsd/create-attribute targetNamespace ns-aliases attr-node xsd))
           (xml/get-elements-by-name sequence '(:http://www.w3.org/2001/XMLSchema . "attribute")))))

    (set-prop complexType 'name name)
    (set-prop complexType 'xsd  xsd)
    (set-prop complexType 'elements elements)
    (set-prop complexType 'attributes attributes)
    (set-prop complexType 'targetNamespace targetNamespace)
    (set-prop complexType 'ns-aliases ns-aliases)

    (defmethod complexType 'get-name (lambda () (this. 'name)))

    (defmethod complexType 'get-element-sample
      (lambda (element-name)
        (concat
         "<" (invoke element-name 'get-localname) 
         (apply 'concat (mapcar (lambda (attribute) (invoke attribute 'get-sample)) (this. 'attributes)))
         ">\n" 

         (apply 'concat 
                (mapcar 
                 (lambda (element-node) 
                   (let ((elem 
                          (cond ((xml/get-attribute-value element-node "name")
                                 (xsd/create-element (this. 'targetNamespace) (this. 'ns-aliases) element-node (this. 'xsd)))
                                ((xml/get-attribute-value element-node "ref")
                                 (invoke (this. 'xsd) 'get-element 
                                         (xml/expand-qname (xml/get-attribute-value element-node "ref")
                                                           (this. 'targetNamespace)
                                                           (this. 'ns-aliases)))))))
                     (invoke elem 'get-sample)))
                 (this. 'elements)))

         "</" (invoke element-name 'get-localname) ">\n")))
    
    complexType))


(defun xsd/create-attribute (targetNamespace ns-aliases attribute-node xsd)
  "Create xsd attribute from it's node"
  (let* ((attribute (create-object))
         (name (xml/new-qname targetNamespace (xml/get-attribute-value complexType-node "name")))
         (type 
          (and (xml/get-attribute-value node "type")
               (xml/expand-qname (xml/get-attribute-value node "type") targetNamespace ns-aliases))))

    (set-prop attribute 'name name)
    (set-prop attribute 'type type)
    (set-prop attribute 'xsd  xsd)

    (defmethod attribute 'get-name (lambda () (this. 'name)))
    (defmethod attribute 'get-type (lambda () (this. 'type)))
    (defmethod attribute 'get-sample 
      (lambda () 
        (let ((att-type (invoke (this. 'xsd) 'get-type (this. type))))
          (if (null att-type)
              (error (concat "Type " (invoke (this. type) 'to-string) " not found"))
            (concat " " (invoke (this. 'name) 'get-localname) "=" 
                    "\"" (invoke att-type 'get-sample) "\"")))))

    attribute))

(provide 'xsd)