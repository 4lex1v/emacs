(bind-key "RET"         'newline-and-indent)
(bind-key "M-j"         'join-line)
(bind-key "C-j"         (lambda () (interactive) (delete-indentation t)))
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
(bind-key "C-x \\"      'align-regexp)

(bind-key "<f9>"        'list-packages)
(bind-key "<f10>"       'customize-themes)

(bind-key "C-+"         'text-scale-increase)
(bind-key "C--"         'text-scale-decrease)

(bind-key "M-_"         'hs-hide-level)
(bind-key "M-+"         'hs-show-all)

(unbind-key "C-c m")

(defun 4lex1v/closeBuffer (&optional arg)
  "Close currently opened file (i.e assosiated buffer) and
if the arg is not nil and it's not the single window, close it as well"
  (interactive "P")
  (kill-buffer (current-buffer))
  (if (and (not (equal arg 'nil))
           (> (count-windows) 1))
      (delete-window)))

(bind-key "C-q" '4lex1v/closeBuffer)

(provide                'keys)
