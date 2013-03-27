(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(setq org-return-follows-link 't)

(add-to-list 'load-path "~/sandbox/3rdparty/org-mode/contrib/lisp")
(require 'org-drill)

(setq org-capture-templates
      `(("d"
         "deutsch"
         entry
         (file "~/org/deutsch.org")
         ,(concat "* Translate                                                           :drill:\n"
                  "  :PROPERTIES:\n"
                  "  :DRILL_CARD_TYPE: twosided\n"
                  "  :END:\n"
                  "** Deutsch\n"
                  "%^{Deutsch}\n"
                  "** Russian\n"
                  "%^{Russian}"
                  ))))

(provide 'nd-org-settings)
