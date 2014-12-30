;; Open build.sbt
(defun sbt-ext:open-build-file ()
  (interactive)
  (let ((sbt-build-file (concat (projectile-project-root) "build.sbt")))
    (if (file-exists-p sbt-build-file)
        (find-file sbt-build-file)
      (error "build.sbt is not defined"))))


