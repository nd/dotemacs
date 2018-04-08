;;;; common setup

(add-to-list 'load-path "~/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/vendor")

;; moving cursor down at bottom scrolls
;; only a single line, not half page
(setq-default scroll-step 1)
(setq-default scroll-conservatively 0)
(setq-default scroll-margin 2)

;; typed text replaces the selection
(delete-selection-mode t)

;; y and n instead of yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;; do not ask additional question if I want to kill a buffer with a
;; live process attached to it, I always aswer 'yes' anyway
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; disable mouse
(when (not (package-installed-p 'disable-mouse))
  (package-install 'disable-mouse))
(require 'disable-mouse)
(global-disable-mouse-mode)

;; avoid mouse then typing
(mouse-avoidance-mode 'banish)

;; cutting and pasting uses the clipboard
(setq x-select-enable-clipboard t)

;; autosave configuration
(setq auto-save-list-file-prefix "/tmp/emacs/auto-save-list/.saves-")

;; tramp history file
(setq tramp-persistency-file-name "/home/nd/tmp/tramp")

;; no backup files - use git instead
(setq make-backup-files nil)

;; bury *scratch* buffer instead of kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; create directories automaticaly
;; if we open file in directory that doesn't exist yet
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;; save minibuffer history between sessions
(savehist-mode 1)


(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(winner-mode 1)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell.
(http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable)"
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; seems to help a bit with long lines:
;; http://comments.gmane.org/gmane.emacs.devel/159671,
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=13675
(setq-default cache-long-line-scans t)
(setq-default bidi-display-reordering nil)

(provide 'nd-common-settings)
