;;;; wsdl parser


(require 'util)
(require 'xsd)

(defun wsdl/create-wsdl (location)
  "Create wsdl model from result of parsing wsdl"
  (lexical-let* 
      ((xml (xml/parse-document-at-location location))

       (targetNamespace 
        (xml/get-attribute-value xml "targetNamespace"))

       (ns-aliases 
        (xml/get-ns-aliases xml))

       (imports 
        (mapcar (lambda (node) (wsdl/process-import node location))
                (xml/get-elements-by-name xml '(:http://schemas.xmlsoap.org/wsdl/ . "import"))))

       (xsds
        (cons (xsd/get-standart-schema)
              (mapcar (lambda (schema-node) (xsd/create-xsd schema-node))
                      (xml/get-elements-by-name 
                       (car (xml/get-elements-by-name xml '(:http://schemas.xmlsoap.org/wsdl/ . "types")))
                       '(:http://www.w3.org/2001/XMLSchema . "schema")))))

       (messages 
        (append (mapcar (lambda (node) (wsdl/create-message targetNamespace ns-aliases node))
                        (xml/get-elements-by-name xml '(:http://schemas.xmlsoap.org/wsdl/ . "message")))
                (loop for imported in imports
                      append (invoke imported 'get-messages))))

       (portTypes
        (append (mapcar (lambda (node) (wsdl/create-portType targetNamespace ns-aliases node))
                        (xml/get-elements-by-name xml '(:http://schemas.xmlsoap.org/wsdl/ . "portType")))
                (loop for imported in imports
                      append (invoke imported 'get-portTypes))))

       (bindings 
        (append (mapcar (lambda (node) (wsdl/create-binding targetNamespace ns-aliases node))
                        (xml/get-elements-by-name xml '(:http://schemas.xmlsoap.org/wsdl/ . "binding")))
                (loop for imported in imports
                      append (invoke imported 'get-bindings))))
       
       (services 
        (mapcar (lambda (node) (wsdl/create-service targetNamespace ns-aliases node))
                (xml/get-elements-by-name xml '(:http://schemas.xmlsoap.org/wsdl/ . "service"))))

       (ports 
        (let ((p (list)))
          (mapcar (lambda (service) (setq p (append p (invoke service 'get-ports)))) services)
          p)))

    ;; dispatch function
    (lambda (message &rest args) 
      (cond ((eq message 'get-targetNamespace) targetNamespace)
            ((eq message 'get-messages) messages)
            ((eq message 'get-services) services)
            ((eq message 'get-bindings) bindings)
            ((eq message 'get-portTypes) portTypes)
            ((eq message 'get-ports) ports)
            ((eq message 'get-binding) 
             (car (filter bindings
                          (lambda (binding) 
                            (invoke (invoke binding 'get-name) 'equal (car args))))))
            ((eq message 'get-portType)
             (car (filter portTypes
                          (lambda (portType) 
                            (invoke (invoke portType 'get-name) 'equal (car args))))))
            ((eq message 'get-message)
             (car (filter messages
                          (lambda (message) 
                            (invoke (invoke message 'get-name) 'equal (car args))))))
            ((eq message 'get-element)
             (invoke (car (filter xsds
                                  (lambda (xsd)
                                    (equal (invoke xsd 'get-targetNamespace) 
                                           (invoke (car args) 'get-namespace)))))
                     'get-element (car args)))
            ((eq message 'get-type)
             (invoke (car (filter xsds
                                  (lambda (xsd)
                                    (equal (invoke xsd 'get-targetNamespace) 
                                           (invoke (car args) 'get-namespace)))))
                     'get-type (car args)))
            ;;port + operation
            (t (error (concat "Operation " (symbol-name message)  " is not supproted")))))))


(defun wsdl/create-service (target-namespace ns-aliases service-node)
  "Create wsdl service from it's node"
  (lexical-let* ((node service-node)
                 (namespace target-namespace)
                 (name (xml/new-qname target-namespace (xml/get-attribute-value node "name")))
                 (ports 
                  (mapcar (lambda (n) (wsdl/create-port target-namespace ns-aliases n))
                     (xml/get-elements-by-name node '(:http://schemas.xmlsoap.org/wsdl/ . "port")))))

    ;; dispatch function
    (lambda (message &rest args)
      (cond ((eq message 'get-name) name)
            ((eq message 'get-ports) ports)
            ((eq message 'get-port) 
             (car (filter ports 
                          (lambda (port) 
                            (invoke (invoke port 'get-name) 'equal (car args))))))
            (t (error "Operation " (symbol-name message) " is not supported"))))))


(defun wsdl/create-port (target-namespace ns-aliases port-node)
  "Create wsdl port from it's node"
  (lexical-let* ((node port-node)
                 (name (xml/new-qname target-namespace (xml/get-attribute-value node "name")))
                 (binding-name (xml/expand-qname (xml/get-attribute-value node "binding") target-namespace ns-aliases)))

    ;; dispatch function
    (lambda (message)
      (cond ((eq message 'get-name) name)
            ((eq message 'get-binding-name) binding-name)
            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun wsdl/create-message (target-namespace ns-aliases message-node)
  "Create wsdl message from it's node"
  (lexical-let* ((node message-node)
                 (name (xml/new-qname target-namespace (xml/get-attribute-value node "name")))
                 (parts (mapcar (lambda (n) (wsdl/create-part target-namespace ns-aliases n))
                                (xml/get-elements-by-name node '(:http://schemas.xmlsoap.org/wsdl/ . "part")))))

    ;; dispatch function
    (lambda (message)
      (cond ((eq message 'get-name) name)
            ((eq message 'get-parts) parts)
            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun wsdl/create-portType (target-namespace ns-aliases portType-node)
  "Create wsdl portType from it's node"
  (lexical-let* ((node portType-node)
                 (name (xml/new-qname target-namespace (xml/get-attribute-value node "name")))
                 (operations (mapcar (lambda (n) (wsdl/create-operation target-namespace ns-aliases n))
                                     (xml/get-elements-by-name node '(:http://schemas.xmlsoap.org/wsdl/ . "operation")))))

    ;; dispatch function
    (lambda (message)
      (cond ((eq message 'get-name) name)
            ((eq message 'get-operations) operations)
            ((eq message 'get-operation) (error "Not implemented yet"))
            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun wsdl/create-operation (target-namespace ns-aliases operation-node)
  "Create wsdl portType from it's node"
  (lexical-let* ((node operation-node)
                 (name (xml/get-attribute-value node "name"))
                 (input 
                  (let ((input-node (car (xml/get-elements-by-name 
                                          node 
                                          '(:http://schemas.xmlsoap.org/wsdl/ . "input")))))
                    (if (null input-node)
                        nil
                      (wsdl/create-input target-namespace ns-aliases input-node)))))

    ;; dispatch function
    (lambda (message)
      (cond ((eq message 'get-name) name)
            ((eq message 'get-input) input)
            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun wsdl/create-input (target-namespace ns-aliases input-node)
  "Create wsdl operation input from it's node"
  (lexical-let* ((node input-node)
                 (name (xml/get-attribute-value node "name"))
                 (message-name 
                  (xml/expand-qname (xml/get-attribute-value node "message") target-namespace ns-aliases)))

    ;; dispatch function
    (lambda (message)
      (cond ((eq message 'get-name) name)
            ((eq message 'get-message-name) message-name)
            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun wsdl/create-binding (target-namespace ns-aliases binding-node)
  "Create wsdl binding from it's node"
  (lexical-let* ((node binding-node)

                 (name (xml/new-qname target-namespace (xml/get-attribute-value node "name")))

                 (portType-name (xml/expand-qname (xml/get-attribute-value node "type") target-namespace ns-aliases))

                 (default-binding-style 
                   (let ((soapBinding (car (xml/get-elements-by-name 
                                            node 
                                            '(:http://schemas.xmlsoap.org/wsdl/soap/ . "binding")))))
                     (cond ((null (xml/get-attribute-value soapBinding "style")) 'document)
                           ((equal (xml/get-attribute-value soapBinding "style") "rpc") 'rpc)
                           (t 'document)))))

    ;; dispatch function
    (lambda (message &rest args)
      (cond ((eq message 'get-name) name)
            ((eq message 'get-operation-binding-style) default-binding-style)
            ((eq message 'get-portType-name) portType-name)
            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun wsdl/create-part (target-namespace ns-aliases part-node)
  "Create wsdl message part from it's node"
  (lexical-let* ((node      part-node)
                 (use-type?  (not (null (xml/get-attribute-value node "type"))))
                 (name (xml/new-qname target-namespace (xml/get-attribute-value node "name")))
                 (typename 
                  (let ((str-value (if use-type? 
                                       (xml/get-attribute-value node "type")
                                     (xml/get-attribute-value node "element"))))
                    (xml/expand-qname str-value target-namespace ns-aliases))))

    ;; dispatch function
    (lambda (message)
      (cond ((eq message 'get-name) name)
            ((eq message 'get-typename) typename)
            ((eq message 'use-type?) use-type?)
            (t (error (concat "Operation '" (symbol-name message) "' is not supported")))))))


(defun wsdl/process-import (import-node base-location)
  "Process wsdl:import"
  (let* ((location  (xml/get-attribute-value import-node "location"))
         (expanded-location (xml/expand-location location base-location)))

    (wsdl/create-wsdl expanded-location)))


;; wsdl-model accessors:

(defun wsdl/get-services (wsdl)
  "Get service list from wsdl model"
  (invoke wsdl 'get-services))

(defun wsdl/get-messages (wsdl)
  "Get message list from wsdl model"
  (invoke wsdl 'get-messages))


(defun create-soap-request (message operation binding wsdl)
  "Create string that contains soap-request for this message in
  this binding" 

  ;;pre-conditions: 
  (cond ((and (eq binding 'document) 
              (not (equal (length (invoke message 'get-parts)) 1))) 
         (error "in document binding message should contain only one part"))
        ((and (eq binding 'document)
              (invoke (car (invoke message 'get-parts)) 'use-type?))
         (error "in document binding message part should use element, not type"))
        ((and (eq binding 'rpc)
              (not (invoke (car (invoke message 'get-parts)) 'use-type?))
              (not (eql (length (invoke message 'get-parts)) 1)))
         (error "in rpc binding if message part use element should be only one part")))

  (concat "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\">\n" 
          "<soapenv:Body>\n"

          (cond ((eq binding 'document)
                 (let ((element (invoke wsdl 'get-element)))
                   (invoke element 'get-sample (invoke message 'get-name))))

;                 (concat "here soon will be request for message " (invoke (invoke message 'get-name) 'to-string) 
;                         " in " (symbol-name binding) " binding\n"))
                ((eq binding 'rpc)
                 (if (invoke (car (invoke message 'get-parts)) 'use-type?)
                     (concat "<"  (invoke operation 'get-name) " xmlns=\"" (invoke wsdl 'get-targetNamespace) "\">\n"
                             (apply 'concat 
                                    (mapcar 
                                     (lambda (part) 
                                       (let* ((part-name (invoke part 'get-name))
                                              (type-name (invoke part 'get-typename))
                                              (type (invoke wsdl 'get-type type-name)))
                                         (invoke type 'get-element-sample part-name)))
                                     (invoke message 'get-parts)))
                             "</" (invoke operation 'get-name) ">\n")
                   
                   (let ((type-name (invoke (car (invoke message 'get-parts)) 'get-typename)))
                     (message (invoke type-name 'to-string))
                     (invoke (invoke wsdl 'get-element type-name) 'get-sample
                             (xml/new-qname (invoke wsdl 'get-targetNamespace) (invoke operation 'get-name)))))))

          "</soapenv:Body>\n"
          "</soapenv:Envelope>"))


(defun wsdl/create-request (wsdl-location)
  "Build soap request to one of wsdl's operations in separate buffer"
  (interactive "swsdl: ")
  ;;TODO: make these bindings buffer local, and remember them
  (let* ((wsdl (wsdl/create-wsdl wsdl-location))
         (port (util/select (invoke wsdl 'get-ports)
                            (lambda (port) 
                              (let ((qname (invoke port 'get-name)))
                                (invoke qname 'to-string)))
                               "Port: "))
         (binding (invoke wsdl 'get-binding (invoke port 'get-binding-name)))
         (portType (invoke wsdl 'get-portType (invoke binding 'get-portType-name)))
         (operation (util/select (invoke portType 'get-operations)
                                 (lambda (op) 
                                   (invoke op 'get-name))
                                 "Operation: "))
         (input (invoke operation 'get-input))
         (message (invoke wsdl 'get-message (invoke input 'get-message-name)))
         (request (create-soap-request message 
                                       operation
                                       (invoke binding 'get-operation-binding-style operation)
                                       wsdl))
         (buf (set-buffer (get-buffer-create (generate-new-buffer-name "*soap-request*")))))
    
    (set-window-buffer (selected-window) buf)
    (insert request)
    (nxml-mode)
    (indent-region (point-min) (point-max))
    buf))


(defun wsdl/view-wsdl-at-location (location)
  (interactive "swsdl: ")
  (let ((buf (url-retrieve-synchronously location)))
    (set-buffer buf)
    (rename-buffer location)
    (xml/delete-http-header)
    (set-window-buffer (selected-window) buf)
    (nxml-mode)))


(provide 'wsdl)

