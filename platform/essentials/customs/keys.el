(bind-key "RET"    'newline-and-indent)
(bind-key "M-j"    'join-line)
(bind-key "C-a"    'back-to-indentation)
(bind-key "M-m"    'beginning-of-line)
(bind-key "M-`"    'other-frame)
(bind-key "C-c r"  'revert-buffer)
(bind-key "C-x \\" 'align-regexp)
(bind-key "C-;"    'toggle-comment-on-line)
(bind-key "C-q"    '4lex1v/close-buffer)
(bind-key "M-q"    '4lex1v:w/close-other-window)
(bind-key "C-S-d"  '4lex1v/duplicate-line)

;; Bad thingy
(bind-key "M-o"
            #'(lambda (arg)
                (interactive "p")
                (end-of-line)
                (open-line arg)
                (next-line 1)
                (indent-according-to-mode)))