(add-to-list 'load-path "~/.emacs.d/vendor/emacs-jabber")

(require 'jabber)

(setq jabber-account-list
      '(("dmitry.neverov@gmail.com" 
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))

(setq
  jabber-history-enabled t
  jabber-use-global-history nil
  jabber-backlog-number 40
  jabber-backlog-days 30)

(define-key jabber-chat-mode-map (kbd "RET") 'newline)
(define-key jabber-chat-mode-map [M-return] 'jabber-chat-buffer-send)

(add-hook 'jabber-chat-mode-hook 'goto-address)

(setq jabber-alert-presence-message-function 
      '(lambda (who oldstatus newstatus statustext) nil))

(defvar libnotify-program "/usr/bin/notify-send")

(defun notify-send (title message)
  (start-process "notify" " notify"
		 libnotify-program "--expire-time=4000" title message))

(defun libnotify-jabber-notify (from buf text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat messages via libnotify"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buf))))
    (if (jabber-muc-sender-p from)
        (notify-send (format "(PM) %s"
                       (jabber-jid-displayname (jabber-jid-user from)))
               (format "%s: %s" (jabber-jid-resource from) text)))
      (notify-send (format "%s" (jabber-jid-displayname from))
             "")))

(add-hook 'jabber-alert-message-hooks 'libnotify-jabber-notify)

(add-hook 'jabber-chat-mode-hook 'auto-fill-mode)

(defadvice jabber-chat-send (before removing-newlines
                                    (jc body))
  "Remove unnecessary new lines inserted by auto-fill-mode."
  (ad-set-arg 1 (replace-regexp-in-string "\\([[:word:]]\\)\n\\([[:word:]]\\)" "\\1 \\2" body)))

(ad-activate 'jabber-chat-send)

(setq jabber-vcard-avatars-retrieve nil)

(provide 'nd-jabber-settings)
