;;;; look and feel

(load-library "time")
(setq display-time-24hr-format t
      display-time-mail-file t
      display-time-form-list (list 'time 'load)
      display-time-day-and-date t)
(display-time)

(set-frame-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-*")


(provide 'nd-decorations)