;; Clear buffer in Shell-mode
(defun shell-clear ()
	(interactive)
	(let ((comint-buffer-maximum-size 0))
		(comint-truncate-buffer)))
