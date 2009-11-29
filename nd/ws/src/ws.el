(require 'nxml-parse)
(require 'nxml-util)
(require 'cl)

(defun parse-wsdl (path)
  (let ((nxml-tree (nxml-parse-file path)))
    (let ((ns (create-namespace (target-namespace nxml-tree)))
          (msgs (wsdl-messages nxml-tree))
          (aliases (node-aliases nxml-tree))
          (schemas (wsdl-embedded-schemes nxml-tree)))
      (mapc (lambda (msg-node) (define-message ns msg-node)) msgs)
      (mapc (lambda (alias) (add-alias! ns (car alias) (cdr alias))) aliases)
      (mapc (lambda (schema) (add-import! ns (parse-xsd-tree schema aliases))) schemas)
      ns)))

(defun parse-xsd (path) (parse-xsd-tree (nxml-parse-file path)))

(defun parse-xsd-tree (nxml-tree &optional additional-aliases)
  (let ((ns            (create-namespace (target-namespace nxml-tree)))
        (aliases       (node-aliases nxml-tree))
        (simple-types  (xsd-simple-types nxml-tree))
        (complex-types (xsd-complex-types nxml-tree))
        (elements      (xsd-elements nxml-tree)))
    (mapc (lambda (alias) (add-alias! ns (car alias) (cdr alias))) (append additional-aliases aliases))
    (mapc (lambda (simple-type) (define-simple-type ns simple-type)) simple-types)
    (mapc (lambda (complex-type) (define-complex-type ns complex-type)) complex-types)
    (mapc (lambda (element) (define-element ns element)) elements)
    ns))


;; namespaces
;;-----------
(defun create-namespace (name) 
  (let ((ns (create-empty-namespace name)))
    (add-import! ns xsd-ns)
    ns))

(defun create-empty-namespace (ns-name) 
  (list (cons 'name ns-name)
        (cons 'imports  '())
        (cons 'aliases  '())
        (cons 'types    '())
        (cons 'elements '())
        (cons 'messages '())))

(defun namespace-name (namespace) (cdr (assoc 'name namespace)))

(defun add-namespace-entry! (namespace entries-key new-entry)
  (let ((entries (assoc entries-key namespace)))
    (setcdr entries (cons new-entry (cdr entries)))))

(defun add-import! (namespace imported-namespace)
  (add-namespace-entry! namespace 'imports (cons (namespace-name imported-namespace) imported-namespace)))

(defun add-alias! (namespace alias namespace-name)
  (if (equal? alias "xmlns")
      (add-namespace-entry! namespace 'aliases (cons "" namespace-name))
    (add-namespace-entry! namespace 'aliases (cons alias namespace-name))))

(defun add-message! (namespace name message)
  (add-namespace-entry! namespace 'messages (cons name message)))

(defun add-type! (namespace name type)
  (add-namespace-entry! namespace 'types (cons name type)))

(defun add-element! (namespace name element)
  (add-namespace-entry! namespace 'elements (cons name element)))

(defun expand-alias (namespace alias-name)
  (let ((aliases (cdr (assoc 'aliases namespace))))
    (let ((alias-entry (assoc alias-name aliases)))
      (if (null? alias-entry)
          nil
        (cdr alias-entry)))))

(defun get-imported-namespace-by-name (namespace imported-namespace-name)
  (let ((imports (cdr (assoc 'imports namespace))))
    (let ((import-entry (assoc imported-namespace-name imports)))
      (if (null? import-entry)
          nil
        (cdr import-entry)))))

(defun get-imported-namespace-by-alias (namespace alias)
  (let ((imported-namespace-name (expand-alias namespace alias)))
    (get-imported-namespace-by-name namespace imported-namespace-name)))

;(defun get-all-messages (namespace) (cdr (assoc 'messages namespace)))

(defun lookup (namespace entries-key name)
  "Lookup entry in this namespace definitions only"
  (let* ((entries (cdr (assoc entries-key namespace)))
         (entry (assoc (get-local-name name) entries)))
    (if (null? entry)
        nil
      (apply-partially (cdr entry) namespace))))

(defun lookup-with-imports (namespace entries-key name)
  "Lookup entry in this namespace definitions and in imported
  namespaces" 
  (let ((this-namespace-entry (lookup namespace entries-key name)))
    (if (null? this-namespace-entry)
        (let* ((prefix (get-prefix name))
               (imported-ns (get-imported-namespace-by-alias namespace prefix)))
          (if (null? imported-ns)
              nil
            (lookup imported-ns entries-key name)))
      this-namespace-entry)))

(defun get-message (namespace name)
  (lookup-with-imports namespace 'messages name))

(defun get-type (namespace name)
  (lookup-with-imports namespace 'types name))

(defun get-element (namespace name)
  (lookup-with-imports namespace 'elements name))


;; messages
;;---------
(defun define-message (namespace message-node)
  (let ((message (create-message-print-function namespace message-node))
        (name (node-attribute-value message-node "name")))
    (add-message! namespace name message)))

(defun create-message-print-function (namespace message-node)
  "Create function that print message"
  `(lambda (my-ns tag-name)
     (concat "<"  tag-name ">\n"
             ,@(mapcar 'print-message-part (wsdl-message-parts message-node))
             "</" tag-name ">\n")))

(defun print-message-part (message-part-node)
  (let ((part-name (node-attribute-value message-part-node "name"))
        (type? (not (null (node-attribute-value message-part-node "type")))))
    (if type?
        `(funcall (get-type  my-ns ,(node-attribute-value message-part-node "type")) ,part-name)
      `(funcall (get-element my-ns ,(node-attribute-value message-part-node "element")) ,part-name))))


;; types
;;------
(defun define-build-in-type (namespace type-name type-sample)
  (let ((type (create-build-in-type-print-function type-sample)))
    (add-type! namespace type-name type)))

(defun create-build-in-type-print-function (type-sample)
  `(lambda (my-ns tag-name) 
     (concat "<" tag-name ">" ,type-sample "</"tag-name ">\n")))

(defun define-simple-type (namespace type-node)
  (let ((simple-type (create-simple-type-print-function type-node))
        (name (node-attribute-value type-node "name")))
    (add-type! namespace name simple-type)))

(defun create-simple-type-print-function (type-node)
  (cond ((xsd-simple-type-by-restriction? type-node)
         `(lambda (my-ns tag-name) 
            (funcall (get-type my-ns ,(xsd-get-restriction-base-type type-node)) tag-name)))
        (t `(lambda (my-ns tag-name) (cons tag-name "Simple type not by restriction, unsupported yet")))))

(defun define-complex-type (namespace type-node)
  (let ((complex-type (create-complex-type type-node))
        (name (node-attribute-value type-node "name")))
    (add-type! namespace name complex-type)))

(defun create-complex-type (type-node)
  (cond ((xsd-complex-type-with-sequence? type-node)
         `(lambda (my-ns tag-name)
            (concat
             "<" tag-name ">\n"
             ,@(mapcar 'create-local-element (xsd-get-sequence-elements type-node))
             "</" tag-name ">\n")))
        (t `(lambda (my-ns tag-name) "Unknown complex type"))))


;; elements
;;---------
(defun define-element (namespace element-node)
  (let ((element (create-element element-node))
        (name (node-attribute-value element-node "name")))
    (add-element! namespace name element)))

(defun create-element (element-node)
  (let ((element-name (node-attribute-value element-node "name")))
    (cond ((not (null? (node-attribute-value element-node "type")))
           `(lambda (my-ns tag-name) 
              (funcall (get-type my-ns ,(node-attribute-value element-node "type")) 
                       (or tag-name ,element-name))))
          ((not (null? (node-attribute-value element-node "ref")))
           `(lambda (my-ns tag-name)
              (funcall (get-element my-ns ,(node-attribute-value element-node "ref")) 
                       (or tag-name ,element-name))))
          (t `(lambda (my-ns tag-name) "Unknown element\n")))))

(defun create-local-element (element-node)
  (cond ((not (null? (node-attribute-value element-node "type")))
         `(funcall (get-type my-ns ,(node-attribute-value element-node "type")) 
                   ,(node-attribute-value element-node "name")))
        ((not (null? (node-attribute-value element-node "ref")))
         `(funcall (get-element my-ns ,(node-attribute-value element-node "ref")) nil))
        (t "Unknown element\n")))


;;build-in xsd namespace
(defconst xsd-ns 
  (let ((ns (create-empty-namespace "http://www.w3.org/2001/XMLSchema")))
    (define-build-in-type ns "string"   "string")
    (define-build-in-type ns "int"      "1")
    (define-build-in-type ns "decimal"  "1.0")
    (define-build-in-type ns "date"     "2009-10-25")
    (define-build-in-type ns "NMTOKEN"  "US")
    (define-build-in-type ns "integer"  "1")
    (define-build-in-type ns "dateTime" "2009-10-25T23:11:00.000-05:00")
    (define-build-in-type ns "boolean"  "true")
    ns))


;;--------------------------------------
;; abstraction over nxml-tree structure:
;;--------------------------------------
(defun attribute-name (attribute) (car attribute))
(defun attribute-value (attribute) (cdr attribute))
(defun node-attributes (node) (cadr node))
(defun node-attribute-value (node attribute-name)
  (attribute-value (car (filter (lambda (a) (equal (attribute-name a) attribute-name))
                                (node-attributes node)))))
(defun target-namespace (node) (node-attribute-value node "targetNamespace"))

(defun node-aliases (node)
  (defun alias? (attr)
    (let ((attr-name (attribute-name attr)))
      (and (consp attr-name) 
           (equal (car attr-name) :http://www\.w3\.org/2000/xmlns/))))
  (mapcar (lambda (attr) (cons (cdr (attribute-name attr)) (attribute-value attr)))
          (filter 'alias? (node-attributes node))))

(defun node-name (node) (car node))
(defun node-childs (node) (filter (lambda (n) (listp n)) (cddr node)))

(defun get-prefix(qname) 
  (let ((split (split-string qname ":")))
    (if (equal? (length split) 2)
        (car (split-string qname ":"))
    "")))
(defun get-local-name (qname) 
  (let ((split (split-string qname ":")))
    (if (equal? (length split) 2)
        (cadr (split-string qname ":"))
      (car (split-string qname ":")))))


;; functions to get wsdl-specific nodes:
(defun wsdl-messages (definitions-node) 
  (filter (lambda (n) (equal (node-name n) (cons :http://schemas\.xmlsoap\.org/wsdl/ "message")))
          (node-childs definitions-node)))
(defun wsdl-message-parts (message-node)
  (filter (lambda (n) (equal (node-name n) (cons :http://schemas\.xmlsoap\.org/wsdl/ "part")))
          (node-childs message-node)))
(defun wsdl-embedded-schemes (definitions-node)
  (filter (lambda (n) (equal (node-name n) (cons :http://www\.w3\.org/2001/XMLSchema "schema")))
          (node-childs 
           (car (filter (lambda (n) (equal (node-name n) (cons :http://schemas\.xmlsoap\.org/wsdl/ "types")))
                        (node-childs definitions-node))))))


;; functions to get xsd-specific nodes:
(defun xsd-simple-types (schema-node) 
  (filter (lambda (n) (equal (node-name n) (cons :http://www\.w3\.org/2001/XMLSchema "simpleType")))
          (node-childs schema-node)))

(defun xsd-simple-type-by-restriction? (simple-type-node)
  (let ((childs (node-childs simple-type-node)))
    (and (not (null? childs))
         (equal (node-name (car childs)) (cons :http://www\.w3\.org/2001/XMLSchema "restriction")))))

(defun xsd-get-restriction-base-type (simple-type-node)
  (let ((restriction (car (node-childs simple-type-node))))
    (node-attribute-value restriction "base")))

(defun xsd-complex-types (schema-node) 
  (filter (lambda (n) (equal (node-name n) (cons :http://www\.w3\.org/2001/XMLSchema "complexType")))
          (node-childs schema-node)))

(defun xsd-complex-type-with-sequence? (complex-type-node)
  (let ((childs (node-childs complex-type-node)))
    (and (not (null? childs))
         (equal (node-name (car childs)) (cons :http://www\.w3\.org/2001/XMLSchema "sequence")))))

(defun xsd-get-sequence-elements (complex-type-node)
  (let ((sequence (car (node-childs complex-type-node))))
    (node-childs sequence)))

(defun xsd-elements (schema-node) 
  (filter (lambda (n) (equal (node-name n) (cons :http://www\.w3\.org/2001/XMLSchema "element")))
          (node-childs schema-node)))
    

(defun filter (predicat items)
  (defun iter (its)
    (if (null its)
        '()
      (if (funcall predicat (car its))
          (cons (car its) (iter (cdr its)))
        (iter (cdr its)))))
  (iter items))

(defalias 'null? 'null)
(defalias 'equal? 'equal)


(provide 'ws)