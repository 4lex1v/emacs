(defun 4lex1v:gui:frame (&rest configs)
  "Helper function for simpler frame configuration"
  (pcase-let* ((`(,active . ,inactive) (plist-get configs :transparency))
               (`(,active-cursor . ,inactive-cursor) (plist-get configs :cursor)))
    
    (setq-default cursor-type active-cursor
                  cursor-in-non-selected-windows inactive-cursor)

    (set-frame-parameter (selected-frame) 'alpha (cons active inactive))
    (add-to-list 'default-frame-alist (cons 'alpha (cons active inactive)))))
