
(let ((doc (nxml-parse-file "~/.emacs.d/nd/wsdl/test-data/wsdl-import/test.wsdl")))
  (let* ((wsdl (wsdl/create-wsdl "~/.emacs.d/nd/wsdl/test-data/wsdl-import/test.wsdl")))

    ;;tests:
    
    'ok
    ))



