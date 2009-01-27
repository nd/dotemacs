;;;; look and feel

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

;; mark current line:
(global-hl-line-mode 1)

;; color for current line:
(set-face-background 'hl-line "#e0f8ff")

(load-library "time")
(setq display-time-24hr-format t
      display-time-form-list (list 'time)
      display-time-default-load-average nil
      display-time-load-average-threshold 1
      display-time-day-and-date t)
(display-time)

(setq battery-mode-line-format " [%p%%%b,~%t] ")
(display-battery-mode)

(if (equal system-type 'gnu/linux)
    (set-frame-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-*"))

(custom-set-faces
 '(comint-highlight-prompt ((t (:foreground "dark blue" :weight ultra-bold)))))


(provide 'nd-decorations)