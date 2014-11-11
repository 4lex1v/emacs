;; Configuration file for custom defined functions

(defun load-functions (list-of-files)
	(mapc (lambda (file)
					(load (concat "~/.emacs.d/configs/functions/" file ".el")))
				list-of-files))

;;;;;;;;;;;;;;
(load-functions '("shell"))

					
