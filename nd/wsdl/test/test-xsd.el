(let* ((xsd (xsd/create-xsd "~/.emacs.d/nd/wsdl/test-data/wsdl-spec-example/test.xsd")))
  ;;tests:

  (let ((comment-element (invoke xsd 'get-element (xml/new-qname "http://bercut.com/types" "comment")))
        (purchaseOrder (invoke xsd 'get-element (xml/new-qname "http://bercut.com/types" "purchaseOrder")))
        (skuElement (invoke xsd 'get-element (xml/new-qname "http://bercut.com/types" "skuElement")))
        (listOfString (invoke xsd 'get-element (xml/new-qname "http://bercut.com/types" "listOfString")))
        (usAddress (invoke xsd 'get-element (xml/new-qname "http://bercut.com/types" "usAddress"))))
    (assert (not (null comment-element)) "comment element is null")
    (assert (equal (invoke comment-element 'get-sample) "<comment>string</comment>\n")
            "wrong element of build-in type")

    (assert (equal (invoke skuElement 'get-sample) "<skuElement>string</skuElement>\n")
            "wrong element of build-in type")
    
    (assert (equal (invoke listOfString 'get-sample) "<listOfString>string</listOfString>\n")
            "wrong element of build-in type")

;    (assert (equal (invoke usAddress 'get-sample)
;                   "<usAddress>\n<name>string</name>\n<street>string</street>\n<city>string</city>\n<state>string</state>\n<zip>1.0</zip>\n</usAddress>\n")
;                   "wrong sample of element of complexType")

    (invoke purchaseOrder 'get-sample))

;  'ok
)
