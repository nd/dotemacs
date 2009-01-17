;;;; gpg settings


(if (equal system-type 'gnu/linux)
    (progn
      (require 'epa)
      (epa-file-enable)))


(provide 'nd-gpg-settings)