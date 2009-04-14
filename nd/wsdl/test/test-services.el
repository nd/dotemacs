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
  (assert (not (null (invoke (car services) 'get-name))) t
          "service name is null")

  (let* ((service (car services))
         (qname   (invoke service 'get-name)))
    (assert (equal (invoke qname 'get-localname) "StockQuoteService") t "wrong service name")
    (assert (equal (invoke qname 'get-namespace) "http://example.com/stockquote.wsdl") t 
            (concat "wrong service namespace should be http://example.com/stockquote.wsdl" 
                    " but was " 
                    (invoke qname 'get-namespace)))
    
    ;; ports
    (assert (not (null (invoke service 'get-ports))))
    (let ((port (car (invoke service 'get-ports))))
      (assert (not (null (invoke port 'get-binding-name))))
      (assert (equal (invoke (invoke port 'get-binding-name) 'get-localname) "StockQuoteSoapBinding")
              t "wrong binding name")
      (assert (equal (invoke (invoke port 'get-binding-name) 'get-namespace) "http://example.com/stockquote.wsdl")
              t "wrong binding namespace"))
    
    (assert (not (null (invoke service 
                               'get-port 
                               (xml/new-qname "http://example.com/stockquote.wsdl" "StockQuotePort")))))
    )
  
  'ok)


(let* ((wsdl (wsdl/create-wsdl "http://127.0.0.1:4040/SeTestBpelSend/SeTestSendPort?wsdl"))))
