(defun change-font-size (value)
  (interactive
   (list
    (read-number (format "Font current font size [%d] to: " font-size))))
  (setq font-size value)
  (4lex1v:gui:font font-name :size value))
