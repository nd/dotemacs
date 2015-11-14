;;;; environment settings

(set-language-environment 'Russian)

(if (or (equal system-type 'gnu/linux)
        (equal system-type 'darwin))
    (progn (set-default-coding-systems    'utf-8-unix)
           (set-buffer-file-coding-system 'utf-8-unix)
           (prefer-coding-system          'utf-8-unix)))

(if (equal system-type 'windows-nt)
    (progn (set-default-coding-systems    'utf-8-dos)
           (set-buffer-file-coding-system 'utf-8-dos)
           (prefer-coding-system          'utf-8-dos)))

(provide 'nd-environment-settings)
