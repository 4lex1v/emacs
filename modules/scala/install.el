(load-file (expand-file-name "modules/scala/custom.el" user-emacs-directory))

(use-package scala-mode
  :load-path "modules/scala/scala-mode"
  :after (yasnippet company)
  :commands scala-mode
  :mode ("\\.\\(scala\\|sbt\\|sc\\)\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)

  :bind
  (:map scala-mode-map
   ("C-c b"         . sbt-ext:open-build-file)
   ("<C-return>"    . newline-or-comment)
   ("M-j"           . scala-indent:join-line)
   ("C-c c"         . sbt-command)
   ("C-<backspace>" . contextual-backspace))

  :init
  (setq-default initial-major-mode 'scala-mode)
  (setq scala-indent:use-javadoc-style t
        scala-mode:debug-messages nil)

  :config
  (with-package smartparens-mode
    (use-package smartparens-scala
      :init
      (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
      (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))))
  
  (with-package hideshowvis
    (push '(scala-mode "\\({\\|(\\)" "\\(}\\|)\\)" "/[*/]" nil nil)
          hs-special-modes-alist))

  (4lex1v/hook-into-modes #'4lex1v/fix-scala-fonts 'scala-mode-hook))

(use-package sbt-mode
  :load-path "modules/scala/sbt-mode"
  :after scala-mode
  :commands (sbt-start sbt-command))

(use-package ensime
  :after scala
  :load-path "modules/scala/ensime"
  :commands ensime
  :bind
  (:map scala-mode-map
   ("C-c e" . ensime-print-errors-at-point)
   ("C-c t" . ensime-print-type-at-point)
   ("C-c o" . ensime-import-type-at-point)
   ("C-M-." . ensime-edit-definition-other-window))

  :init
  (setq ensime-server-version        "1.0.0"
        ensime-default-buffer-prefix "ENSIME-"
        ensime-startup-notification   nil
        ensime-startup-snapshot-notification nil)

  (with-mode which-key
    (which-key-declare-prefixes-for-mode 'scala-mode
      "C-c C-d" "ensime/debug"
      "C-c C-c" "ensime/compiler"
      "C-c C-r" "ensime/refactoring"
      "C-c C-t" "ensime/tests"
      "C-c C-v" "ensime/general"
      "C-c C-b" "ensime/sbt"))
  
  (use-package popup :ensure t)

  (defun 4lex1v:smart-ensime-loader ()
    (lambda () 
      (if (and (if-bound-f projectile-project-p)
               (if-bound-f projectile-project-root))
          (4lex1v/connect-running-ensime (projectile-project-root))
        (progn
          (message (format "Projectile is not loaded, using %s" default-directory))
          (4lex1v/connect-running-ensime default-directory)))))

  ;; Smart Ensime loader
  (4lex1v/hook-into-modes #'4lex1v:smart-ensime-loader 'scala-mode-hook)

  ;; Eldoc support in Scala + Ensime mode
  (setq-local eldoc-documentation-function
              #'(lambda ()
                  (when (ensime-connected-p)
                    (let ((err (ensime-print-errors-at-point)))
                      (or (and err (not (string= err "")) err)
                          (ensime-print-type-at-point))))))

  :config
  (unbind-key "M-p" ensime-mode-map)
  (use-package ensime-company
    :init
    (add-to-list 'company-backends 'ensime-company)))
