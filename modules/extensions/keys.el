
(general-emacs-define-key 'global :prefix ""
  "RET"    'newline-and-indent
  "M-t"    #'sp-transpose-sexp
  "M-j"    'join-line
  "C-a"    'back-to-indentation
  "M-m"    'beginning-of-line
  "M-`"    'other-frame
  "C-c r"  'revert-buffer
  "C-x \\" 'align-regexp
  "C-;"    'toggle-comment-on-line
  "C-q"    '4lex1v/close-buffer
  "M-q"    '4lex1v:w/close-other-window
  "C-S-d"  '4lex1v/duplicate-line
  "<M-backspace>"  'sp-backward-kill-sexp
  "M-o" #'(lambda (arg)
            (interactive "p")
            (end-of-line)
            (open-line arg)
            (next-line 1)
            (indent-according-to-mode)))

(provide 'keys)
