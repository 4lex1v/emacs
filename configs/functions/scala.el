;; In multiline comments inserts a new line with indented asterisk 
(defun scala-functions:newline-or-comment ()
  (interactive)
  (indent-new-comment-line)
  (scala-indent:insert-asterisk-on-multiline-comment))
