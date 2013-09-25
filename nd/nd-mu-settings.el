(add-to-list 'load-path "/Users/nd/sandbox/3rdparty/mu/mu4e/")

(require 'mu4e)

;; default
(setq mu4e-maildir "~/JBMaildir")
(setq mu4e-sent-folder "/INBOX")
(setq mu4e-drafts-folder "/INBOX.drafts")
(setq mu4e-trash-folder "/INBOX.Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'sent)

(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "/usr/local/bin/offlineimap")

;; something about ourselves
(setq user-mail-address "Dmitry.Neverov@jetbrains.com"
      user-full-name  "Dmitry Neverov"
      message-signature
      (concat "Dmitry Neverov\n"
              "JetBrains\n"
              "http://www.jetbrains.com\n"
              "\"Develop with pleasure!\""))

;; Try to display images in mu4e
(setq  mu4e-view-show-images t
       mu4e-view-image-max-width 800)

(setq mu4e-confirm-quit nil
      mu4e-headers-date-format "%d %b %Y %H:%M" ; date format
      mu4e-html2text-command "/usr/local/bin/html2text -utf8 -width 7")

(require 'smtpmail)
(setq message-send-mail-function 'message-smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "mail.intellij.net"
      smtpmail-smtp-server "mail.intellij.net"
      smtpmail-smtp-service 2525
      smtpmail-smtp-user "Dmitry.Neverov@jetbrains.com"
      starttls-gnutls-program "/opt/local/bin/gnutls-cli")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(add-hook 'mu4e-headers-mode-hook
          (lambda ()
            (define-key mu4e-headers-mode-map (kbd "j") 'next-line)
            (define-key mu4e-headers-mode-map (kbd "k") 'previous-line)))

(add-hook 'mu4e-view-mode-hook
          (lambda ()
            (define-key mu4e-view-mode-map (kbd "C-c C-s") 'org-store-link)))

(setq mu4e-mu-binary "/usr/local/bin/mu")

(global-set-key (kbd "C-c m") 'mu4e)


(defun nd-mu-jb-set-from-address ()
  "Set the From address for feedback."
  (let ((msg mu4e-compose-parent-message))
    (if msg
        (setq user-mail-address
            (cond
             ((mu4e-message-contact-field-matches msg :to "teamcity-feedback@jetbrains.com")
              "teamcity-feedback@jetbrains.com")
             (t "Dmitry.Neverov@jetbrains.com")))
      (setq user-mail-address "Dmitry.Neverov@jetbrains.com"))))

(add-hook 'mu4e-compose-pre-hook 'nd-mu-jb-set-from-address)

(defun nd-mu-jb-add-feedback-bcc ()
  "Add a Bcc: feedback header."
  (let ((msg mu4e-compose-parent-message))
    (when (and msg (mu4e-message-contact-field-matches msg :to "teamcity-feedback@jetbrains.com"))
      (save-excursion (message-add-header "Bcc: teamcity-feedback@jetbrains.com\n")))))

(add-hook 'mu4e-compose-mode-hook 'nd-mu-jb-add-feedback-bcc)

(defun mu4e-action-archive-message (msg)
  (mu4e-action-retag-message msg "+Archived"))

(defun nd-mu-archive-selected ()
  (interactive)
  (mu4e-headers-for-each
   '(lambda (msg)
      (when (mu4e-mark-docid-marked-p (mu4e-message-field msg :docid))
        (mu4e-action-archive-message msg)))))

(add-to-list 'mu4e-view-actions
             '("earchive-message" . mu4e-action-archive-message) t)
(add-to-list 'mu4e-headers-actions
             '("earchive-message" . mu4e-action-archive-message) t)

(add-to-list 'mu4e-bookmarks '("not tag:Archived and date:20130530.." "Unified inbox" ?i))


(defun mu4e-action-find-thread (msg)
  (let* ((msgid (mu4e-message-field-raw msg :message-id))
     (query (concat "i:" msgid))
     (q mu4e-headers-include-related)
     (q2 mu4e-headers-show-threads)
     )
    (setq mu4e-headers-include-related t)
    (setq mu4e-headers-show-threads nil)
    (mu4e-headers-search query)
    (setq mu4e-headers-include-related q)
    (setq mu4e-headers-show-threads q2)
    ))

(add-to-list 'mu4e-headers-actions
         '("find thread" . mu4e-action-find-thread) t)
(add-to-list 'mu4e-view-actions
         '("find thread" . mu4e-action-find-thread) t)


(provide 'nd-mu-settings)
