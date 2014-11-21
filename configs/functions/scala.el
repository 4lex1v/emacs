;; IN multiline comments inserts a new line with indented asterisk 
(defun scala-functions:new-comment-line ()
  (interactive)
  (indent-new-comment-line)
  (scala-indent:insert-asterisk-on-multiline-comment))
