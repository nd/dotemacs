(require 'util)
(require 'xsd)
(require 'cl)

(defstruct source
  xml           ;; result of parsing using nxml-parse-file
  tns           ;; targetNamespace of document
  aliases       ;; alist prefix->namespace
  used-sources  ;; alist namespace->location-or-node
  location)
 


(defun wsdl2/create-request (wsdl-location)
  "Build soap request to one of wsdl's operations in separate buffer. second version"
  (interactive "swsdl: ")
  (with-temp-buffer
    (make-local-variable 'wsdl-location)  ;; wsdl-location
    (make-local-variable 'loaded-sources) ;; location->source
    (make-local-variable 'tns->source)    ;; map targetNamespace of source -> source
    (make-local-variable 'port)           ;; port we request
    (make-local-variable 'port-location)  ;; location of port we request
    (make-local-variable 'current-source) ;; xml-source we currently work with
    (make-local-variable 'binding)        ;; binding of port
    (make-local-variable 'portType)       ;; portType we request
    (make-local-variable 'operation)      ;; operation we request
    (make-local-variable 'message)        ;; input message of operation we request


    (setq wsdl-location wsdl-location)
    (setq loaded-sources (make-hash-table))
    (setq tns->source (make-hash-table))
    (setq current-source (register-source-maybe wsdl-location))

    ;; choose port:
    (setq port (util/select
                (wsdl2/get-ports (source-xml current-source))
                (lambda (port-node) (xml/get-attribute-value port-node "name"))
                "Port: "))
    (setq port-location (wsdl2/get-port-location port))

    (let ((binding-name (wsdl2/get-port-binding port (source-tns current-source))))
      (setq current-source (wsdl2/resolve-ns current-source (nxml-get-namespace binding-name)))
      (setq binding (wsdl2/get-binding binding-name)))

    (let ((portType-name (wsdl2/get-binding-portType binding (source-tns current-source))))
      (setq current-source (wsdl2/resolve-ns current-source (nxml-get-namespace portType-name)))
      (setq portType (wsdl2/get-portType portType-name)))
    
    (setq operation (util/select
                     (wsdl2/get-operations portType)
                     (lambda (operation-node) (xml/get-attribute-value operation-node "name"))
                     "Operation: "))

    (let* ((input (wsdl2/get-input operation))
           (message-name (wsdl2/get-input-message input (source-tns current-source))))
      (setq current-source (wsdl2/resolve-ns current-source (nxml-get-namespace message-name)))
      (setq message (wsdl2/get-message message-name)))

    ;; build request
    (concat "soap request to " (xml/get-attribute-value portType "name"))))


(defun wsdl2/get-ports (wsdl)
  "Get list of port nodes from wsdl. Wsdl is a result of nxml-parse-file."
  (mapcan 
   (lambda (service-node)
     (xml/get-elements-by-name 
      service-node
      '(:http://schemas.xmlsoap.org/wsdl/ . "port")))
   (xml/get-elements-by-name wsdl '(:http://schemas.xmlsoap.org/wsdl/ . "service"))))


(defun wsdl2/get-port-location (port)
  "Get port's soap:address location from port's node"
  (let ((soap-address 
         (car (xml/get-elements-by-name port '(:http://schemas.xmlsoap.org/wsdl/soap/ . "address")))))
    (if (null soap-address)
        (error "port doesn't contain soap:address")
      (xml/get-attribute-value soap-address "location"))))


(defun wsdl2/get-port-binding (port tns)
  "Get qualified name of port's binding from port's node. 
ns-aliases is map from prefix to namespace.
tns is targetNamespace"
  (let ((string-value (xml/get-attribute-value port "binding"))
        (ns-aliases (source-aliases (gethash tns tns->source))))
    (xml/expand-qname2 string-value tns ns-aliases)))


(defun wsdl2/get-binding-portType (binding tns)
  "Get binding's portType qname"
  (let ((string-value (xml/get-attribute-value binding "type"))
        (ns-aliases (source-aliases (gethash tns tns->source))))
    (xml/expand-qname2 string-value tns ns-aliases)))


(defun wsdl2/get-used-sources (wsdl base-location)
  "Get alist (namespace->location-or-node) of used sources.
Used sources are imported wsdl's, inner xsd's (???)"
  (mapcar 
   (lambda (import-node)
     (let* ((namespace  (xml/get-attribute-value import-node "namespace"))
            (location  (xml/get-attribute-value import-node "location"))
            (expanded-location (xml/expand-location location base-location)))
       (cons (nxml-make-namespace namespace) expanded-location)))
   (xml/get-elements-by-name wsdl '(:http://schemas.xmlsoap.org/wsdl/ . "import"))))


(defun wsdl2/resolve-ns (source namespace)
  "Get xml source that define this namespace.
SOURCE is xml source that use this namespace."
  (if (eq (source-tns source) namespace) 
      source
    (let* ((ns (nxml-get-namespace (nxml-node-name (source-xml source))))
           (imports (xml/get-elements-by-name (source-xml source) (cons ns "import")))
           (resolved (register-source-maybe
                      (xml/expand-location
                       (xml/get-attribute-value 
                        (find namespace imports
                              :test (lambda (name import-node)
                                      (equal (xml/get-attribute-value import-node "namespace") 
                                             (nxml-namespace-name namespace))))
                        "location")
                       (source-location source)))))
      (or resolved
           (error (concat "Can not resolve " (nxml-namespace-name namespace)))))))


(defun wsdl2/get-bindings (wsdl)
  "Get list of binding nodes from wsdl. Wsdl is a result of nxml-parse-file."
  (xml/get-elements-by-name wsdl '(:http://schemas.xmlsoap.org/wsdl/ . "binding")))


(defun wsdl2/get-portTypes (wsdl)
  "Get list of portType nodes from wsdl. Wsdl is a result of nxml-parse-file."
  (xml/get-elements-by-name wsdl '(:http://schemas.xmlsoap.org/wsdl/ . "portType")))


(defun wsdl2/get-binding (binding-name)
  "Get binding node by binding name"
  (let ((wsdl (source-xml (gethash (nxml-get-namespace binding-name) tns->source))))
    (find (nxml-get-localname binding-name) 
          (wsdl2/get-bindings wsdl)
          :test (lambda (name binding-node)
                  (equal (xml/get-attribute-value binding-node "name") name)))))


(defun wsdl2/get-portType (portType-name)
  "Get portType node by portType-name"
  (let ((wsdl (source-xml (gethash (nxml-get-namespace portType-name) tns->source))))
    (find (nxml-get-localname portType-name) 
          (wsdl2/get-portTypes wsdl)
          :test (lambda (name portType-node)
                  (equal (xml/get-attribute-value portType-node "name") name)))))


(defun register-source-maybe (location)
  (if (not (null (gethash location loaded-sources)))
      (gethash location loaded-sources)
    (let ((source (make-source)))
      (setf (source-xml source) (xml/parse-document-at-location location))
      (setf (source-tns source) (nxml-make-namespace (xml/get-attribute-value (source-xml source) "targetNamespace")))
      (setf (source-aliases source) (xml/get-ns-aliases2 (source-xml source)))
      (setf (source-used-sources source) (wsdl2/get-used-sources (source-xml source) location))
      (setf (source-location source) location)
      (puthash location source loaded-sources)
      (puthash (source-tns source) source tns->source)
      source)))

(defun wsdl2/get-operations (portType)
  (xml/get-elements-by-name portType '(:http://schemas.xmlsoap.org/wsdl/ . "operation")))

(defun wsdl2/get-input (operation)
  (let ((inputs (xml/get-elements-by-name operation '(:http://schemas.xmlsoap.org/wsdl/ . "input"))))
    (if (null inputs)
        (error "operation has no input")
      (car inputs))))

(defun wsdl2/get-input-message (input tns)
  (let ((string-value (xml/get-attribute-value input "message"))
        (ns-aliases (source-aliases (gethash tns tns->source))))
    (xml/expand-qname2 string-value tns ns-aliases)))

(defun wsdl2/get-messages (wsdl)
  "Get list of message nodes from wsdl. Wsdl is a result of nxml-parse-file."
  (xml/get-elements-by-name wsdl '(:http://schemas.xmlsoap.org/wsdl/ . "message")))

(defun wsdl2/get-message (message-name)
  "Get message node by message-name"
  (let ((wsdl (source-xml (gethash (nxml-get-namespace message-name) tns->source))))
    (find (nxml-get-localname message-name) 
          (wsdl2/get-messages wsdl)
          :test (lambda (name message-node)
                  (equal (xml/get-attribute-value message-node "name") name)))))

(provide 'wsdl2)