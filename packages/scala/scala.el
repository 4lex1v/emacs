(add-to-list 'load-path (concat user-emacs-directory "packages/scala/scala-mode"))
(add-to-list 'load-path (concat user-emacs-directory "packages/scala/sbt-mode"))
(add-to-list 'load-path (concat user-emacs-directory "packages/scala/ensime"))

(defun 4lex1v/ensime-project-p (project-root-dir)
  "Load ensime mode for scala only if there is an ensime
project file .ensime defined in the root directory"
  (let ((ensime-project-file (concat project-root-dir ".ensime")))
    (file-exists-p ensime-project-file)))

(defun sbt-ext:open-build-file (project-root-dir)
  (interactive)
  (let ((sbt-build-file (concat project-root-dir "build.sbt")))
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

(defun 4lex1v/ensime-running-p (project-root-dir)
  (if (4lex1v/ensime-project-p project-root-dir)
      (file-exists-p (concat project-root-dir ".ensime_cache/port"))
    (error "Not an ENSIME project")))

(defun 4lex1v/connect-running-ensime (project-root-dir)
  (if (4lex1v/ensime-running-p project-root-dir)
      (ensime-mode 1)))

(defun 4lex1v/start-ensime (project-root-dir)
  (interactive)
  (if (4lex1v/ensime-project-p project-root-dir)
      (let ((port-file (concat project-root-dir ".ensime_cache/port"))
            (config-file (concat project-root-dir ".ensime")))
        (if (file-exists-p port-file) (delete-file port-file))
        (ensime--1 config-file))
    (error "Not an ENSIME project")))

(defun 4lex1v/ensime-cleanup (project-root-dir)
  (interactive)
  (if (4lex1v/ensime-project-p project-root-dir)
      (let ((ensime-file (concat project-root-dir ".ensime"))
            (ensime-cache-folder (concat project-root-dir ".ensime_cache")))
        ;; Drop ensime cache folder
        (if (file-exists-p ensime-cache-folder) (delete-directory ensime-cache-folder t))
        (if (file-exists-p ensime-file) (delete-file ensime-file)))))

(defun configure-backends (backends)
  (lambda ()
    (add-to-list 'company-backends 'ensime-company)))

(defun 4lex1v/fix-scala-fonts ()
  (mapc
   (lambda (kw)
     (let ((face-ref (intern (format "scala-font-lock:%s-face" kw))))
       (copy-face font-lock-keyword-face face-ref)))
   '("final" "private" "protected" "implicit" "abstract" "sealed" "lazy" "override")))

(provide 'scala)
