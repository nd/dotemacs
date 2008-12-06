;;;; common setup

(add-to-list 'load-path "~/emacs/site-lisp")

;; no scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; no toolbar
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
;; no menu
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))

;; maximum font decoration
(setq font-lock-mode-maximum-decoration t)
(global-font-lock-mode t)

;; show marked text
(setq transient-mark-mode 1)

;; show parents
(show-paren-mode 1)

;; not blinking cursor 
(blink-cursor-mode -1)

;; tab settings
(setq default-tab-width  2)
;; insert space instead of tab
(setq-default indent-tabs-mode nil) 

;; show column number in status bar
(setq column-number-mode t)

;; empty scratch message
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)

;; moving cursor down at bottom scrolls 
;; only a single line, not half page
(setq-default scroll-step 1)
(setq-default scroll-conservatively 0)
(setq-default scroll-margin 2)

;; mark current line:
(global-hl-line-mode 1)

;; color for current line:
(set-face-background 'hl-line "#e0f8ff")

;; typed text replaces the selection
(delete-selection-mode t)

;; y and n instead of yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;; avoid mouse then typing
(mouse-avoidance-mode 'banish)


(provide 'nd-common-settings)
