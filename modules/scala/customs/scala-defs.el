(defun format-all (project)
  (interactive "D")
  (-select
   (lambda (file)
     (f-ext-p file "scala"))
   (f-files project 'identity t)))

(defun newline-or-comment ()
  "Insert * if in the middle of the comment"
  (interactive)
  (indent-new-comment-line)
  (scala-indent:insert-asterisk-on-multiline-comment))

(defun 4lex1v/indent-in-braces (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun 4lex1v/fix-scala-fonts ()
  (interactive)
  (mapc
   (lambda (kw)
     (let ((face-ref (intern (format "scala-font-lock:%s-face" kw))))
       (copy-face font-lock-keyword-face face-ref)))
   '("final" "private" "protected" "implicit" "abstract" "sealed" "lazy" "override"
     "inline")))

(defun scala-join-lines ()
  (interactive)
  (scala-indent:join-line t))
