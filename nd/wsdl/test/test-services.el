(require 'cl)

(defun empty? (list)
  (null list))

(let* ((wsdl (wsdl/create-wsdl "~/.emacs.d/nd/wsdl/test-data/wsdl-spec-example/test.wsdl"))
       (services (wsdl/get-services wsdl)))
  ;;tests:
  (assert (listp services) t 
          "wsdl/get-services should return list")
  (assert (not (empty? services)) t 
          "wsdl/get-services return empty list")
  (assert (eq (safe-length services) 1) t 
          "wsdl/get-services should return only one service for this wsdl")
  (assert (not (null (wsdl/get-name (car services)))) t
          "service name is null")

  (let* ((service (car services))
         (qname    (wsdl/get-name service)))
    (assert (equal (xml/get-localname qname) "StockQuoteService") t "wrong service name")
    (assert (equal (xml/get-namespace qname) "http://example.com/stockquote.wsdl") t 
            (concat "wrong service namespace should be http://example.com/stockquote.wsdl" 
                    " but was " 
                    (xml/get-namespace qname)))
    
    ;; ports
    (assert (not (null (invoke service 'get-ports))))
    (let ((port (car (invoke service 'get-ports))))
      (assert (not (null (invoke port 'get-binding-name))))
      (assert (equal (xml/get-localname (invoke port 'get-binding-name)) "StockQuoteSoapBinding")
              t "wrong binding name")
      (assert (equal (xml/get-namespace (invoke port 'get-binding-name)) "http://example.com/stockquote.wsdl")
              t "wrong binding namespace"))
    
    (assert (not (null (invoke service 
                               'get-port 
                               (xml/new-qname "http://example.com/stockquote.wsdl" "StockQuotePort")))))
    )
  
  'ok)
