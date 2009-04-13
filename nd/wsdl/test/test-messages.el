(require 'cl)
(require 'util)
(require 'wsdl)

(defun empty? (list)
  (null list))

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
         (part1    (car (invoke message1 'get-parts)))
         (part2    (car (invoke message2 'get-parts))))
    ;; check qnames:
    (assert (equal (invoke (invoke message1 'get-name) 'get-localname) "GetLastTradePriceInput") t 
            "wrong message name")
    (assert (equal (invoke (invoke message1 'get-name) 'get-namespace) "http://example.com/stockquote.wsdl") t 
            "wrong message namespace")
    (assert (equal (invoke (invoke message2 'get-name) 'get-localname) "GetLastTradePriceOutput") t 
            "wrong message name")
    (assert (equal (invoke (invoke message2 'get-name) 'get-namespace) "http://example.com/stockquote.wsdl") t 
            "wrong message namespace")

    ;;check parts:

    (assert (eq (length (invoke message1 'get-parts)) 1) t "message should have only one part")
    (assert (eq (length (invoke message2 'get-parts)) 1) t "message should have only one part")

    (assert (equal (invoke (invoke (car (invoke message1 'get-parts)) 'get-name) 'get-localname) "body")
            "wrong part name")
    (assert (equal (invoke (invoke (car (invoke message2 'get-parts)) 'get-name) 'get-localname) "body")
            "wrong part name")

    (assert (equal (invoke (invoke part1 'get-typename) 'get-namespace) "http://example.com/stockquote.xsd"))
    (assert (equal (invoke (invoke part1 'get-typename) 'get-localname) "comment"))
    (assert (equal (invoke (invoke part2 'get-typename) 'get-namespace) "http://example.com/stockquote.xsd"))
    (assert (equal (invoke (invoke part2 'get-typename) 'get-localname) "comment"))
    (assert (equal (invoke part1 'use-type?) 't))
    (assert (equal (invoke part2 'use-type?) 't))

    )

  'ok)