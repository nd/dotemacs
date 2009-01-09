;;;; environment settings

(set-language-environment 'Russian)

(if (equal system-type 'gnu/linux)
    (progn (set-default-coding-systems    'utf-8-unix)
           (set-buffer-file-coding-system 'utf-8-unix)
           (prefer-coding-system          'utf-8-unix)))

(if (equal system-type 'ms-dos)
    (progn (set-default-coding-systems    'windows-1251-dos)
           (set-buffer-file-coding-system 'windows-1251-dos)
           (prefer-coding-system          'windows-1251-dos)))

(provide 'nd-environment-settings)