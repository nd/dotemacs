(add-to-list 'load-path "~/.emacs.d/nd/ws/src")
(require 'cl)
(require 'ws)

(defun assert-not-null (smth) (assert (not (null? smth))))

(let* ((ns (create-namespace "http://bercut.com/wsdl"))
       (message (create-message-function ns '())))
  (funcall message "abc"))

(let ((ns (create-namespace "http://bercut.com/wsdl")))
  (assert (equal (namespace-name ns) "http://bercut.com/wsdl"))
  'ok)

(let ((nxml-tree (nxml-parse-file "~/.emacs.d/nd/wsdl/test-data/simple/simple.wsdl"))
      (wsdl-ns (create-namespace "http://bercut.com/wsdl")))
  (add-alias! wsdl-ns "xsd" "http://www.w3.org/2001/XMLSchema")
  (assert-not-null (expand-alias wsdl-ns "xsd"))
  (define-message wsdl-ns (car (wsdl-messages nxml-tree)))
  (assert (not (equal 0 (length (cdr (assoc 'messages wsdl-ns))))) "message was not added")
  (get-message wsdl-ns "GetLastTradePriceInput")
  (funcall (get-message wsdl-ns "GetLastTradePriceInput") "test")
;  'ok
  )

(let ((wsdl (parse-wsdl "~/.emacs.d/nd/wsdl/test-data/simple/with-embedded-schema.wsdl")))
  (assert-not-null (expand-alias wsdl "xsd"))
  (assert-not-null (expand-alias wsdl "tns"))
  (assert-not-null (expand-alias wsdl "soap"))
  (assert-not-null (expand-alias wsdl "schema"))
  (assert-not-null (expand-alias wsdl ""))
  (assert-not-null (get-imported-namespace-by-alias wsdl "schema"))
  (assert-not-null (get-type wsdl "schema:SKU"))
  (funcall (get-type wsdl "schema:SKU") "test")
  (funcall (get-type wsdl "schema:USAddress") "test")
  (funcall (get-message wsdl "GetLastTradePriceInput") "test")
  )


(funcall (lexical-let ((ns xsd-ns))
           (eval '(lambda (tag-name) (funcall (get-type ns "xsd:string") tag-name)))) "test")



