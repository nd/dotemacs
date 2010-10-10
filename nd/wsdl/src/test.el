(require 'nxml-parse)
(require 'nxml-util)
(require 'cl)

(message-to-xml (ns-message
 (let ((ns (parse-wsdl "~/.emacs.d/nd/wsdl/test-data/simple/simple.wsdl")))
   ns) 
 "GetLastTradePriceInput"))

(defconst xsd-ns 
  (let ((ns (make-ns "http://www.w3.org/2001/XMLSchema")))
    (add-type ns (make-type-with-sample "string" ns "string"))
    (add-type ns (make-type-with-sample "int" ns "1"))
    (add-type ns (make-type-with-sample "decimal" ns "1.0"))
    (add-type ns (make-type-with-sample "date" ns "2009-10-25"))
    (add-type ns (make-type-with-sample "NMTOKEN" ns "US"))
    (add-type ns (make-type-with-sample "integer" ns "1"))
    (add-type ns (make-type-with-sample "dateTime" ns "2009-10-25T23:11:00.000-05:00"))
    (add-type ns (make-type-with-sample "boolean" ns "true"))
    ns))

(defun parse-wsdl (path)
  (let ((nxml-tree (nxml-parse-file path)))
    (let ((ns (make-ns (target-namespace nxml-tree)))
          (msgs (messages nxml-tree))
          (aliases (node-prefixes nxml-tree)))
      (add-import ns "http://www.w3.org/2001/XMLSchema" xsd-ns)
      (mapc (lambda (msg-node) (add-message ns (make-message-from-node msg-node ns))) msgs)
      (mapc (lambda (alias) (add-alias ns (car alias) (cdr alias))) aliases)
      ns)))

;;----------
;; namespace
;;----------
(defun make-ns (name)
  (let ((ns (make-hash-table :test 'equal)))
    (puthash "name" name ns)
    (puthash "aliases" (make-hash-table :test 'equal) ns)
    (puthash "imports" (make-hash-table :test 'equal) ns)
    (puthash "messages" (make-hash-table :test 'equal) ns)
    (puthash "types" (make-hash-table :test 'equal) ns)
    ns))
(defun ns-name (ns) (gethash "name" ns))
(defun ns-messages (ns) (gethash "messages" ns))
(defun ns-types (ns) (gethash "types" ns))
(defun ns-aliases (ns) (gethash "aliases" ns))
(defun ns-imports (ns) (gethash "imports" ns))

(defun add-alias (ns prefix fullname)
  (let ((aliases (ns-aliases ns)))
      (puthash prefix fullname aliases)))
(defun ns-alias (ns prefix) 
  (let ((aliases (ns-aliases ns)))    
    (if (null aliases)
        '()
      (gethash prefix aliases))))

(defun add-import (ns imported-ns-name imported-ns)
  (let ((imports (ns-imports ns)))
      (puthash imported-ns-name imported-ns imports)))
(defun ns-import (ns imported-ns-name) 
  (let ((imports (ns-imports ns)))    
    (if (null imports)
        '()
      (gethash imported-ns-name imports))))

(defun get-import-by-prefix (ns prefix)
  (let ((namespace (ns-alias ns prefix)))
    (if (null namespace)
        (error (concat "Namespace for prefix "
                       prefix
                       " not found"))
      (let ((imp-ns (ns-import ns namespace)))
        (if (null imp-ns)
            (error (concat "Namespace for name "
                           namespace
                           " not found"))
          imp-ns)))))

(defun add-message (ns message)
  (if (ns-has-message? ns (message-name message))
      (error (concat "Dublicate message definition --- " 
                     "message: " (message-name message)
                     "namespace: " (ns-name ns)))
    (let ((messages (ns-messages ns)))
      (puthash (message-name message) message messages))))
(defun ns-has-message? (ns name) (not (null (ns-message ns name))))
(defun ns-message (ns name) 
  (let ((msgs (ns-messages ns)))    
    (if (null msgs)
        '()
      (gethash name msgs))))

(defun add-type (ns type)
  (if (ns-has-type? ns (type-name type))
      (error (concat "Dublicate type definition --- " 
                     "type: " (type-name type)
                     "namespace: " (ns-name ns)))
    (let ((types (ns-types ns)))
      (puthash (type-name type) type types))))
(defun ns-type (ns type-name) 
  (let ((types (ns-types ns)))    
    (if (null types)
        '()
      (gethash type-name types))))
(defun find-type (ns type-name)
  (let ((qname (split-string type-name ":")))
    (if (= (length qname) 2)
        (let* ((prefix (car qname))
               (name (cadr qname))
               (imp (get-import-by-prefix ns prefix))
               (type (ns-type imp name)))
          (if (null type)
              (error (concat "Type " type-name " is not found"))
            type))
      (let ((type (ns-type ns type-name)))
        (if (null type)
            (error (concat "Type " type-name " is not found"))
          type)))))
(defun ns-has-type? (ns name) (not (null (ns-type ns name))))


;;-------
;;message
;;-------
(defun make-message (name ns)
  (let ((message (make-hash-table :test 'equal)))
    (puthash "ns" ns message)
    (puthash "name" name message)
    (puthash "parts" (make-hash-table :test 'equal) message)
    message))
(defun message-ns (message) (gethash "ns" message))
(defun message-name (message) (gethash "name" message))
(defun message-parts (message) (gethash "parts" message))
(defun message-part (message part-name) (gethash part-name (message-parts message)))
(defun message-has-part? (message part-name) (not (null (message-part message part-name))))
(defun message-to-string (message)
  (concat "Message " (message-name message) " "
          (loop for part being the hash-values of (message-parts message)
                concat (concat (part-to-string part) " "))))
(defun message-to-xml (message)
  (concat "<" (message-name message) ">\n"
          (loop for part being the hash-values of (message-parts message)
                concat (concat (part-to-xml part (message-ns message)) "\n"))
          "</" (message-name message) ">"))

(defun make-message-from-node (msg-node ns)
  (let ((msg (make-message (node-attribute-value msg-node "name") ns))
        (part-nodes (wsdl-message-parts msg-node)))
    (mapc (lambda (part-node) (add-part msg (make-part-from-node part-node))) part-nodes)
    msg))

(defun add-part (message part)
  (if (message-has-part? message (part-name part))
      (error (concat "Dublicate part definition --- " 
                     "part: " (part-name part)
                     "message: " (message-name message)))
    (puthash (part-name part) part (message-parts message))))

(defun make-part (name)
  (let ((part (make-hash-table :test 'equal)))
    (puthash "name" name part)
    part))
(defun part-name (part) (gethash "name" part))
(defun set-part-type (part type) (puthash "type" type part))
(defun part-type (part) (gethash "type" part))
(defun set-part-element (part element) (puthash "element" element part))
(defun part-element (part) (gethash "element" part))
(defun use-type? (part) (not (null (part-type part))))
(defun use-element? (part) (not (null (part-element part))))
(defun part-data (part) 
  (if (use-type? part)
      (part-type part)
    (part-element part)))
(defun make-part-from-node (part-node)
  (let ((part (make-part (node-attribute-value part-node "name"))))
    (cond ((not (null (node-attribute-value part-node "type")))
           (set-part-type part (node-attribute-value part-node "type")))
          ((not (null (node-attribute-value part-node "element")))
           (set-part-element part (node-attribute-value part-node "element")))
          (else (error (concat "Type and element undefined, part " (part-name part)))))
    part))

(defun part-to-string (part)
  (concat "part " (part-name part)))
(defun part-to-xml (part ns)
  (concat "<" (part-name part) ">"
          (if (use-type? part)
              (type-to-xml (find-type ns (part-type part)))
            (element-to-xml (find-element ns (part-element part))))
          "</" (part-name part) ">"))

;;----
;;type
;;----
(defun make-type (name ns)
  (let ((type (make-hash-table :test 'equal)))
    (puthash "ns" ns type)
    (puthash "name" name type)
    type))
(defun make-type-with-sample (name ns sample)
  (let ((type (make-type name ns)))
    (set-type-sample type sample)
    type))
(defun type-name (type) (gethash "name" type))
(defun set-type-sample (type sample) (puthash "sample" sample type))
(defun type-sample (type) (gethash "sample" type))
(defun type-to-xml (type)
  (let ((sample (type-sample type)))
    (if (not (null sample))
        sample
      (error "Composite types unsupported yet"))))


;; abstraction over nxml-tree structure:
(defun attribute-name (attribute) (car attribute))
(defun attribute-value (attribute) (cdr attribute))
(defun node-attributes (node) (cadr node))
(defun node-attribute-value (node attribute-name)
  (attribute-value (car (filter (lambda (a) (equal (attribute-name a) attribute-name))
                                (node-attributes node)))))
(defun target-namespace (node) (node-attribute-value node "targetNamespace"))
(defun node-aliases (node)
  (defun prefix? (attr)
    (let ((attr-name (attribute-name attr)))
      (and (consp attr-name) 
           (equal (car attr-name) :http://www\.w3\.org/2000/xmlns/))))
  (mapcar (lambda (attr) 
            (let ((attr-name (attribute-name attr)))
              (cons (cdr attr-name) (attribute-value attr))))
          (filter 'prefix? (node-attributes node))))

(defun node-name (node) (car node))
(defun node-childs (node) (filter (lambda (n) (listp n)) (cddr node)))


;; functions to get wsdl-specific nodes:
(defun wsdl-messages (definitions-node) 
  (filter (lambda (n) (equal (node-name n) (cons :http://schemas\.xmlsoap\.org/wsdl/ "message")))
          (node-childs definitions-node)))
(defun wsdl-message-parts (message-node)
  (filter (lambda (n) (equal (node-name n) (cons :http://schemas\.xmlsoap\.org/wsdl/ "part")))
          (node-childs message-node)))


(defun filter (predicat items)
  (defun iter (its)
    (if (null its)
        '()
      (if (funcall predicat (car its))
          (cons (car its) (iter (cdr its)))
        (iter (cdr its)))))
  (iter items))