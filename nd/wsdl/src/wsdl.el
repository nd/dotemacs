;;;; wsdl parser


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
        (mapcar (lambda (schema-node) (xsd/create-xsd schema-node))
                (xml/get-elements-by-name 
                 (xml/get-elements-by-name xml '(:http://schemas.xmlsoap.org/wsdl/ . "types"))
                 '(:http://www.w3.org/2001/XMLSchema . "schema"))))

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
      (cond ((eq message 'get-messages) messages)
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
            ((eq message 'create-request) (error "not implemented yet"))
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
  (funcall wsdl 'get-services))

(defun wsdl/get-messages (wsdl)
  "Get message list from wsdl model"
  (funcall wsdl 'get-messages))

;; common accessors:

(defun invoke (object method &rest args)
  (apply object method args))

(defun wsdl/get-name (smth)
  "Get name of something"
  (funcall smth 'get-name))

;; message accessors: -------------------------------

(defun wsdl/get-parts (message)
  "Get parts of message"
  (funcall message 'get-parts))

;; utilities 
;; TODO: move to separate file

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


(defun xml/get-namespace (qname)
  (funcall qname 'get-namespace))

(defun xml/get-localname (qname)
  (funcall qname 'get-localname))

(defun xml/get-elements-by-name (parent-node name)
  (filter (nxml-node-children parent-node)
          (lambda (n)
            (and (listp n)
                 (funcall (xml/name-equal name) n)))))

(defun xml/get-attribute-value (node attribute-name)
  (cdr (car (filter (nxml-node-attributes node)
                    (lambda (x) (equal (car x) attribute-name))))))

(defun xml/name-equal (name)
  (lambda (node)
    (let ((node-name (nxml-node-name node)))
      (cond ((and (stringp node-name) (stringp name))
             (equal node-name name))
            ((and (listp node-name) (listp name))
             (and (equal (symbol-name (car name))
                         (symbol-name (car node-name)))
                  (equal (cdr name) (cdr node-name))))
            (t nil)))))

(defun xml/get-ns-aliases (node)
  (let ((attributes (nxml-node-attributes node)))
    (mapcar (lambda (a) (cons (cdar a) (cdr a)))
            (filter attributes
                    (lambda (attr) 
                      (and (listp (nxml-node-name attr))
                           (eq (car (nxml-node-name attr)) 
                               :http://www.w3.org/2000/xmlns/)
                           ))))))

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
  (cddr nxml-node))

(defun filter (list predicat)
  "TODO: make it better"
  (if (null list)
      '()
    (if (funcall predicat (car list))
        (append (list (car list)) (filter (cdr list) predicat))
      (filter (cdr list) predicat))))


(defun create-soap-request (message operation binding)
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
              (not (invoke (car (invoke message 'get-parts)) 'use-type?)))
         (error "in rpc binding message part should use type, not element")))

  (concat "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\">\n" 
          "<soapenv:Body>\n"

          (if (eq binding 'document)
              (concat "here soon will be request for message " (invoke (invoke message 'get-name) 'to-string) 
                      " in " (symbol-name binding) " binding\n")
            (concat "<"  (invoke operation 'get-name) " xmlns=\"some-namespace\">\n"
;                    (mapcar (lambda (part) "bla-bla\n")
;                            (invoke message 'get-parts))
                    "</" (invoke operation 'get-name) ">\n"))

          "</soapenv:Body>\n"
          "</soapenv:Envelope>"))


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


(defun wsdl/create-request (wsdl-file)
  "Build soap request to one of wsdl's operations in separate buffer"
  (interactive "fwsdl: ")
  ;;TODO: make these bindings buffer local, and remember them
  (let* ((wsdl (wsdl/create-wsdl wsdl-file))
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
         (request (create-soap-request message operation (invoke binding 'get-operation-binding-style operation)))
         (buf (set-buffer (get-buffer-create (generate-new-buffer-name "*soap-request*")))))
    
    (set-window-buffer (selected-window) buf)
    (insert request)
    (nxml-mode)
    (indent-region (point-min) (point-max))
    buf))


(defun xml/parse-document-at-location (location)
  (if (elt (url-generic-parse-url location) 1)
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
    (let ((result(nxml-parse-file tmp-filename)))
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


(provide 'wsdl)

