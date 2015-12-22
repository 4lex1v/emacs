(add-to-list 'load-path (concat user-emacs-directory "packages/scala/scala-mode"))
(add-to-list 'load-path (concat user-emacs-directory "packages/scala/sbt-mode"))
(add-to-list 'load-path (concat user-emacs-directory "packages/scala/ensime"))

(defun 4lex1v/ensime-project-p ()
  "Load ensime mode for scala only if there is an ensime
project file .ensime defined in the root directory"
  (let* ((root-dir (projectile-project-root))
         (ensime-project-file (concat root-dir ".ensime")))
    (file-exists-p ensime-project-file)))

(defun sbt-ext:open-build-file ()
  (interactive)
  (let ((sbt-build-file (concat (projectile-project-root) "build.sbt")))
    (if (file-exists-p sbt-build-file)
        (find-file sbt-build-file)
      (error "build.sbt is not defined"))))

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

(defun 4lex1v/ensime-running-p ()
  (if (4lex1v/ensime-project-p)
      (file-exists-p
       (concat (projectile-project-root)
               ".ensime_cache/port"))))

(defun 4lex1v/connect-running-ensime ()
  (if (4lex1v/ensime-running-p)
      (ensime-mode 1)))

(defun 4lex1v/start-ensime ()
  (interactive)
  (if (4lex1v/ensime-project-p)
      (let ((port-file (concat (projectile-project-root) ".ensime_cache/port"))
            (config-file (concat (projectile-project-root) ".ensime")))
        (if (file-exists-p port-file) (delete-file port-file))
        (ensime--1 config-file))
    (message "Not an ENSIME project")))

(defun 4lex1v/ensime-cleanup ()
  (interactive)
  (if (4lex1v/ensime-project-p)
      (let ((ensime-file (concat (projectile-project-root) ".ensime"))
            (ensime-cache-folder (concat (projectile-project-root) ".ensime_cache")))
        ;; Drop ensime cache folder
        (if (file-exists-p ensime-cache-folder) (delete-directory ensime-cache-folder t))
        (if (file-exists-p ensime-file) (delete-file ensime-file)))))

(defun configure-backends (backends)
  (lambda ()
    (add-to-list 'company-backends 'ensime-company)))

(provide 'scala)
