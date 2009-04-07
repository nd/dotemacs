(require 'cl)

(defun empty? (list)
  (null list))

(let ((doc (xml/parse-document-at-location "~/.emacs.d/nd/wsdl/test-data/wsdl-spec-example/test.wsdl")))
  (let* ((wsdl (wsdl/create-wsdl "~/.emacs.d/nd/wsdl/test-data/wsdl-spec-example/test.wsdl"))
         (messages (wsdl/get-messages wsdl)))
    ;;tests:
    (assert (listp messages) t 
            "wsdl/get-messages should return list")
    (assert (not (empty? messages)) t 
            "wsdl/get-messages return empty list")
    (assert (eq (length messages) 2) t 
            "wsdl/get-messages should list with size = 2")

    (let* ((message1 (car messages))
           (message2 (cadr messages))
           (part1    (car (wsdl/get-parts message1)))
           (part2    (car (wsdl/get-parts message2))))
      ;; check qnames:
      (assert (equal (xml/get-localname (wsdl/get-name message1)) "GetLastTradePriceInput") t 
              "wrong message name")
      (assert (equal (xml/get-namespace (wsdl/get-name message1)) "http://example.com/stockquote.wsdl") t 
              "wrong message namespace")
      (assert (equal (xml/get-localname (wsdl/get-name message2)) "GetLastTradePriceOutput") t 
              "wrong message name")
      (assert (equal (xml/get-namespace (wsdl/get-name message2)) "http://example.com/stockquote.wsdl") t 
              "wrong message namespace")

      ;;check parts:

      (assert (eq (length (wsdl/get-parts message1)) 1) t "message should have only one part")
      (assert (eq (length (wsdl/get-parts message2)) 1) t "message should have only one part")

      (assert (equal (xml/get-localname (wsdl/get-name (car (wsdl/get-parts message1)))) "body")
              "wrong part name")
      (assert (equal (xml/get-localname (wsdl/get-name (car (wsdl/get-parts message2)))) "body")
              "wrong part name")

      (assert (equal (xml/get-namespace (invoke part1 'get-typename)) "http://example.com/stockquote.xsd"))
      (assert (equal (xml/get-localname (invoke part1 'get-typename)) "comment"))
      (assert (equal (xml/get-namespace (invoke part2 'get-typename)) "http://example.com/stockquote.xsd"))
      (assert (equal (xml/get-localname (invoke part2 'get-typename)) "comment"))
      (assert (equal (invoke part1 'use-type?) 't))
      (assert (equal (invoke part2 'use-type?) 't))

      )

    'ok))






