(bind-key "RET"         'newline-and-indent)
(bind-key "M-j"         'join-line)
(bind-key "C-c m"       'execute-extended-command)
(bind-key "C-x C-b"     'ibuffer)
(bind-key "C-c l"       'view-mode)
(bind-key "C-a"         'back-to-indentation)
(bind-key "M-m"         'beginning-of-line)
(bind-key "S-C-<left>"  'shrink-window-horizontally)
(bind-key "S-C-<right>" 'enlarge-window-horizontally)
(bind-key "S-C-<down>"  'shrink-window)
(bind-key "S-C-<up>"    'enlarge-window)
(bind-key "C-S-d"       'duplicate-line)
(bind-key "M-`"         'other-frame)
(bind-key "C-c r"       'revert-buffer)

(bind-key "C-c C-d"     '4lex1v/delete-current-file)

;; HIDESHOW ;;;;;;;;;;;;;;;;;;;;;
(bind-key "C-c [" 'hs-hide-block)
(bind-key "C-c ]" 'hs-show-block)

(provide 'keys)
