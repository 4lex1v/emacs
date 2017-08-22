(defun 4lex1v/ensime-project-p (project-root-dir)
  "Load ensime mode for scala only if there is an ensime
project file .ensime defined in the root directory"
  (let ((ensime-project-file (concat project-root-dir ".ensime")))
    (file-exists-p ensime-project-file)))

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

(defun 4lex1v:update-ensime-build ()
    (interactive)
    (sbt-command "ensimeConfig"))

(defun 4lex1v:smart-ensime-loader ()
  (interactive)
  (lambda () 
    (if (and (if-bound-f projectile-project-p)
             (if-bound-f projectile-project-root))
        (if (4lex1v/ensime-project-p (projectile-project-root))
            (4lex1v/connect-running-ensime (projectile-project-root)))
      (progn
        (message (format "Projectile is not loaded, using %s" default-directory))
        (4lex1v/connect-running-ensime default-directory)))))

(defun 4lex1v:ensime-eldoc-support ()
  (when (ensime-connected-p)
    (let ((err (ensime-print-errors-at-point)))
      (or (and err (not (string= err "")) err)
          (ensime-print-type-at-point)))))
