;; To disable graphical prompt
;; - add 'allow-emacs-pinentry' line to /home/nd/.gnupg/gpg-agent.conf
;; - run 'gpgconf --reload gpg-agent'
;; - set epa-pinentry-mode:
(setq epa-pinentry-mode 'loopback)

(defun nd-sec-copy-pwd ()
  (interactive)
  (let* ((secrets (nd-sec-read-secrets))
         (names (mapcar #'(lambda (e) (plist-get e :name)) secrets))
         (name (ido-completing-read "Name: " names))
         (entry (car (seq-filter #'(lambda (e) (equal (plist-get e :name) name)) secrets)))
         (pwd (plist-get entry :pass)))
    (with-temp-buffer
      (insert pwd)
      (goto-char (point-min))
      (kill-whole-line))))

(defun nd-sec-show ()
  (interactive))

(defun nd-sec-add (name user pass)
  (interactive "sName: \nsUser: \nsPass: ")
  (let ((secrets (nd-sec-read-secrets)))
    (if (seq-filter #'(lambda (e) (equal (plist-get e :name) name)) secrets)
        (error "Entry with such a name already exist")
      (push `(:name ,name :user ,user :pass ,pass) secrets)
      (nd-sec-write-secrets secrets))))

(defun nd-sec-read-secrets ()
  (with-temp-buffer
    (insert-file-contents "~/pwd2.gpg")
    (goto-char (point-min))
    (let ((res (list)))
      (while (not (eobp))
        (let* ((str (buffer-substring (line-beginning-position) (line-end-position)))
               (parts (split-string str "\t")))
          (push `(:name ,(elt parts 0) :user ,(elt parts 1) :pass ,(elt parts 2)) res))
        (forward-line))
      res)))

(defun nd-sec-write-secrets (secrets)
  (with-temp-buffer
    (mapc #'(lambda (e)
              (insert (plist-get e :name))
              (insert "\t")
              (insert (plist-get e :user))
              (insert "\t")
              (insert (plist-get e :pass))
              (insert "\n"))
          secrets)
    (write-file "~/pwd2.gpg")))

(provide 'nd-sec)
