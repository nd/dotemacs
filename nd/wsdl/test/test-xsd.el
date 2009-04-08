
(let* ((xsd (xsd/create-xsd "~/.emacs.d/nd/wsdl/test-data/wsdl-spec-example/test.xsd")))
  ;;tests:

  (let ((comment-element (invoke xsd 'get-element (xml/new-qname "http://bercut.com/types" "comment"))))
    (assert (not (null comment-element)) "comment element is null")
    (assert (equal (invoke comment-element 'get-sample) "<comment>\nstring\n</comment>\n")
            "wrong element of build-in type")
    )

  'ok)