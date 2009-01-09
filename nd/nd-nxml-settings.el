;;;; nxml settings

(setq nxml-child-indent 4)
(setq nxml-slash-auto-complete-flag t)

(add-to-list 'auto-mode-alist '("\\.xml" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.wsdl" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.bpel" . nxml-mode))

(provide 'nd-nxml-settings)