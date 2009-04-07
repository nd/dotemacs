;;;; xsd parser


(defun xsd/create-xsd (xsd-schema-node)
  "Create wsdl model from result of parsing wsdl"
  (lexical-let* ((xml xsd-schema-node)

                 (targetNamespace 
                  (xml/get-attribute-value schema-node "targetNamespace"))

                 (ns-aliases 
                  (xml/get-ns-aliases schema-node))

                 )

    ;; dispatch function
    (lambda (message &rest args) 
      (cond ((eq message 'get-type-sample) (xsd/get-type-sample (car args) (cadr args)))
            ((eq message 'get-element-sample) (xsd/get-element-sample (car args)))
            (t (error "Operation is not supproted"))))))

(defun xsd/get-type-sample (type-qname tag-name)
  (concat "<"  (invoke tag-name 'get-localname) " xmlns=\"" (invoke tag-name 'get-namespace) "\">"
          "    sample for type " (invoke type-qname 'to-string)
          "</" (invoke tag-name 'get-localname) ">"))

(provide 'xsd)