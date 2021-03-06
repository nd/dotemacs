;;;; nxml settings

(setq nxml-child-indent 4)
(setq nxml-slash-auto-complete-flag t)

(add-to-list 'auto-mode-alist '("\\.xml" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.wsdl" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.bpel" . nxml-mode))


(defun nd-xml-pretty-print ()
  "Pretty print xml buffer.
Print each element and each text-content from new line"
  (interactive)
  (save-excursion
    (save-match-data
      ;;make all open-tags end with new line
      (goto-char (point-min))
      (while (re-search-forward ">\\([^\n]\\)" nil t)
        (replace-match ">\n\\1"))

      ;;make all close-tags start with new line
      (goto-char (point-min))
      (while (re-search-forward "\\([^ \n]+\\) *</" nil t)
        (replace-match "\\1\n</"))

      ;;remove new-lines from elements that contain only text (not other elements)
      ;;and have lenght < 80 (element + text)
      (goto-char (point-min))
      (while (re-search-forward "<\\(\\([^ ]*\\)\\( .*\\)?\\)>\n *\\([^<]*?\\) *\n</\\2>" nil t)
        (let ((element (match-string 0)))
          (if (< (length element) 80)
              (replace-match "<\\1>\\4</\\2>"))))

      (indent-region (point-min) (point-max))

      ;;prettify attributes: if there is no than 2 of them, start each of them 
      ;;from new line if length of tag with attributes is more than 80
      (goto-char (point-min))
      (while (re-search-forward "^ *<\\([^ ]+\\)\\(\\( +[a-zA-Z0-9:]+=\"[a-zA-Z0-9:/. ]*\"\\)\\{2,\\}\\)/?>" nil t)
        (let ((tag        (match-string 0))
              (attributes (match-string 2)))
          (if (> (length tag) 80)
              (let ((pretty-att 
                     (save-match-data
                       (replace-regexp-in-string "\" +\\([^ ]\\)" 
                                                 "\"\n\\1" 
                                                 attributes))))
                (replace-match pretty-att nil nil nil 2)))))

      (indent-region (point-min) (point-max)))))


(defun nd-nxml-keys ()
  (define-key nxml-mode-map "\C-cpp" 'nd-xml-pretty-print))

(add-hook 'nxml-mode-hook 'nd-nxml-keys)


(provide 'nd-nxml-settings)