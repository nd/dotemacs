(add-to-list 'load-path "/Users/nd/Downloads/notmuch-0.14/emacs")
(require 'notmuch)

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq notmuch-search-oldest-first nil)
(setq notmuch-fcc-dirs nil)

(define-key notmuch-search-mode-map "y"
  (lambda ()
    "archive thread"
    (interactive)
    (notmuch-show-tag-thread "+spam" "-inbox")))


(require 'gnus-alias)
(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)

(setq gnus-alias-identity-alist
      '(("default"
         nil ;; Does not refer to any other identity
         "Dmitry Neverov <dmitry.neverov@jetbrains.com>" ;; Sender address
         nil ;; No organization header
         nil ;; No extra headers
         nil ;; No extra body text
         "~/.signature")
        ("feedback"
         nil
         "Dmitry Neverov <teamcity-feedback@jetbrains.com>"
         nil
         (("Bcc" . "teamcity-feedback@jetbrains.com"))
         nil
         "~/.signature")))

(setq gnus-alias-default-identity "default")

;; Define rules to match work identity
(setq gnus-alias-identity-rules
      '(("feedback" ("any" "teamcity-feedback@jetbrains.com" both) "feedback")))
;; Determine identity when message-mode loads
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)

(defun nd-notmuch-reply ()
  (interactive)
  (let* ((to (notmuch-show-get-header :To))
         (sender (if (string-match "teamcity-feedback@jetbrains.com" to)
                     "Dmitry Neverov <teamcity-feedback@jetbrains.com>"
                   "Dmitry Neverov <dmitry.neverov@jetbrains.com>")))
    (notmuch-mua-reply (notmuch-show-get-message-id) sender nil)))
(define-key notmuch-show-mode-map "r" 'nd-notmuch-reply)
