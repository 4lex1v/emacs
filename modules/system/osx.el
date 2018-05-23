(defun finder (path-to-file)
  "Opens file directory in Finder."
  (interactive "F")
  (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file.")))

(provide 'osx)
