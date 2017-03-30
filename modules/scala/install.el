(use-package scala-mode
  :defer
  :mode        ("\\.\\(scala\\|sbt\\|sc\\)\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)

  :bind
  (:map scala-mode-map
   ("<C-return>"    . newline-or-comment)
   ("M-j"           . scala-indent:join-line)
   ("C-<backspace>" . contextual-backspace))
  
  :general
  (:keymaps 'scala-mode-map
    "s" '(:ignore t :which-key "Scala"))

  :init
  (setq scala-indent:use-javadoc-style t
        scala-mode:debug-messages nil)

  (add-hook 'scala-mode-hook #'yas-minor-mode)
  (add-hook 'scala-mode-hook #'global-company-mode)
  (add-hook 'scala-mode-hook #'4lex1v/fix-scala-fonts)

  :config
  (load "defs")
  (push '(scala-mode "\\({\\|(\\)" "\\(}\\|)\\)" "/[*/]" nil nil) hs-special-modes-alist))

(use-package smartparens-scala
  :after scala-mode
  :init
  (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))

(use-package sbt-mode
  :after scala-mode
  :general
  (:keymaps 'scala-mode-map
    "sb" '(:ignore t :which-key "SBT")
    "sbb" #'sbt-ext:open-build-file
    "sbs" #'sbt-start
    "sbr" #'sbt-command))

(use-package ensime
  :after scala-mode
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

  (setq-local eldoc-documentation-function
              #'(lambda ()
                  (when (ensime-connected-p)
                    (let ((err (ensime-print-errors-at-point)))
                      (or (and err (not (string= err "")) err)
                          (ensime-print-type-at-point))))))

  (defun 4lex1v:smart-ensime-loader ()
    (interactive)
    (lambda () 
      (if (and (if-bound-f projectile-project-p)
               (if-bound-f projectile-project-root))
          (4lex1v/connect-running-ensime (projectile-project-root))
        (progn
          (message (format "Projectile is not loaded, using %s" default-directory))
          (4lex1v/connect-running-ensime default-directory)))))

  (defun 4lex1v:update-ensime-build ()
    (interactive)
    (sbt-command "ensimeConfig"))
  
  (general-define-key :keymaps 'scala-mode-map
    "se" '(:ignore t :which-key "Ensime")
    "ses"  #'ensime
    "seu"  #'4lex1v:update-ensime-build)
  
  (general-define-key :keymaps 'ensime-mode-map
    "ser"  #'ensime-inf-run-scala
    "seb" '(:ignore t :which-key "Build")
    "sebr" #'ensime-sbt-do-run
    "sebc" #'ensime-sbt-do-compile)

  (which-key-declare-prefixes-for-mode 'ensime-mode
    "C-c C-d" "ensime/debug"
    "C-c C-c" "ensime/compiler"
    "C-c C-r" "ensime/refactoring"
    "C-c C-t" "ensime/tests"
    "C-c C-v" "ensime/general")

  ;; Smart Ensime loader
  (4lex1v/hook-into-modes #'4lex1v:smart-ensime-loader 'scala-mode-hook)

  :config
  (unbind-key "M-p" ensime-mode-map))

(use-package ensime-company
  :after (ensime company))

(use-package popup
  :after scala-mode
  :ensure t)
