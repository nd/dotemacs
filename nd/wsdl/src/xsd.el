;;;; xsd parser


(defconst build-in-types
  (list 
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "string") "string")
   (xsd/create-build-in-type (xml/new-qname "http://www.w3.org/2001/XMLSchema" "decimal") "1.0")))


(defun xsd/create-xsd (location)
  "Create wsdl model from result of parsing wsdl"
  (lexical-let* ((schema-node (xml/parse-document-at-location location))

                 (targetNamespace 
                  (xml/get-attribute-value schema-node "targetNamespace"))

                 (ns-aliases 
                  (xml/get-ns-aliases schema-node))

;                 (simpleTypes
;                  (mapcar (lambda (node) (xsd/create-simpleType targetNamespace ns-aliases node))
;                          (xml/get-elements-by-name schema-node '(:http://www.w3.org/2001/XMLSchema . "simpleType"))))
                 
                 (elements
                  (mapcar (lambda (node) (xsd/create-element targetNamespace ns-aliases node))
                          (xml/get-elements-by-name schema-node '(:http://www.w3.org/2001/XMLSchema . "element")))))

    ;; dispatch function
    (lambda (message &rest args) 
      (cond ((eq message 'get-type) (get-type-by-name-some-how))
            ((eq message 'get-element) 
             (car (filter elements
                          (lambda (element) 
                            (invoke (invoke element 'get-name) 'equal (car args))))))
            ((eq message 'get-element-sample) (xsd/get-element-sample (car args)))
            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun xsd/create-element (targetNamespace ns-aliases element-node)
  "Create xsd simpleType from it's node"
  (lexical-let* ((node element-node)
                 (name (xml/new-qname targetNamespace (xml/get-attribute-value node "name")))
                 (type (xml/expand-qname (xml/get-attribute-value node "type") targetNamespace ns-aliases)))

    ;; dispatch function
    (lambda (message)
      (cond ((eq message 'get-name) name)

            ((eq message 'get-type) type)

            ((eq message 'get-sample)
             (let ((build-in-type (xsd/get-build-in-type type)))
               (if (not (null build-in-type))
                   (invoke build-in-type 'get-sample name)
                 (do-smth-else))))

            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun xsd/create-build-in-type (name sample)
  (lexical-let* ((name name)
                 (sample sample))

    ;; dispatch function
    (lambda (message &rest args)
      (cond ((eq message 'get-name) name)

            ((eq message 'get-sample) 
             (let ((element-name (car args)))
               (concat
                "<" (invoke element-name 'get-localname) ">\n" 
                sample "\n"
                "</" (invoke element-name 'get-localname) ">\n" )))

            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun xsd/get-build-in-type (qname)
  (car (filter build-in-types
               (lambda (type) 
                 (invoke (invoke type 'get-name) 'equal qname)))))


(provide 'xsd)