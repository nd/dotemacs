(add-to-list 'load-path "/Users/nd/Downloads/notmuch-0.16/emacs")
(require 'notmuch)
(require 'nd-mail)

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq notmuch-search-oldest-first nil)
(setq notmuch-fcc-dirs nil)

(setq notmuch-archive-tags '("-inbox" "-unread"))

(define-key notmuch-search-mode-map "y"
  (lambda ()
    (interactive)
    (notmuch-search-archive-thread)
    ;(notmuch-search-refresh-view)
    ))

(define-key notmuch-search-mode-map "j" 'notmuch-search-next-thread)
(define-key notmuch-search-mode-map "k" 'notmuch-search-previous-thread)


(define-key notmuch-search-mode-map "g" 'notmuch-search-refresh-view)
(define-key notmuch-hello-mode-map "g" 'notmuch-hello-update)


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

(defun nd-notmuch-archive ()
  (interactive)
  (notmuch-show-archive-message)
  (nd-show-go-to-search)
  (notmuch-search-refresh-view))

(defun nd-show-go-to-search ()
  (interactive)
  (let ((parent-buffer notmuch-show-parent-buffer))
    (notmuch-kill-this-buffer)
    (when (buffer-live-p parent-buffer)
      (switch-to-buffer parent-buffer))))

(define-key notmuch-show-mode-map "y" 'nd-notmuch-archive)

(global-set-key (kbd "C-c m") 'notmuch)


;; fast save all attachments to special dir:

(setq base-attachment-dir "/Users/nd/issues/feedback/")

(defun get-attachments-parent-dir ()
  (interactive)
  (let ((dir (concat base-attachment-dir
          (format-time-string "%y.%m.%d" (date-to-time (notmuch-show-get-date)))
          " "
          (notmuch-show-strip-re (notmuch-show-get-subject)))))
    dir))

(defun nd-notmuch-save-all-attachments ()
  "Save all attachments from the current message."
  (interactive)
  (let ((dir (get-attachments-parent-dir)))
    (mkdir dir 't)
    (with-current-notmuch-show-message
     (let ((mm-handle (mm-dissect-buffer)))
       (notmuch-count-attachments mm-handle)
       (nd-notmuch-save-attachments mm-handle dir)))
    (dired dir)))

(defun nd-notmuch-save-attachments (mm-handle dir)
  (notmuch-foreach-mime-part
   (lambda (p)
     (let ((disposition (mm-handle-disposition p)))
       (and (listp disposition)
            (or (equal (car disposition) "attachment")
                (and (equal (car disposition) "inline")
                     (assq 'filename disposition)))
            (nd-mm-save-part p dir))))
   mm-handle))

(defun nd-mm-save-part (handle dir)
  "Write HANDLE to a file.
PROMPT overrides the default one used to ask user for a file name."
  (let ((filename (or (mail-content-type-get (mm-handle-disposition handle) 'filename)
                      (mail-content-type-get (mm-handle-type handle) 'name)))
        file)
    (when filename
      (setq filename (gnus-map-function mm-file-name-rewrite-functions (file-name-nondirectory filename))))
    (setq file (expand-file-name filename dir))
    (setq mm-default-directory (file-name-directory file))
    (and (or (not (file-exists-p file))
             (yes-or-no-p (format "File %s already exists; overwrite? "
                                  file)))
         (progn
           (mm-save-part-to-file handle file)
           file))))

(define-key notmuch-show-mode-map "w" 'nd-notmuch-save-all-attachments)




;; persist sent mail
;; ability to archive a whole thread, both from email and from search

(provide 'nd-notmuch)
