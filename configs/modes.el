;; Config for plugins and modes

(defun 4lex1v/load-custom-configs ()
	(mapcar (lambda (config-file) (load config-file))
					(directory-files "~/.emacs.d/configs/modes/" t ".el")))

(4lex1v/load-custom-configs)
