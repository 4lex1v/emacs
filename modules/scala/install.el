(use-package scala-mode
  :defer
  :mode        ("\\.\\(scala\\|sbt\\|sc\\)\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  
  :general
  (:keymaps 'scala-mode-map
   "s" '(:ignore t :which-key "Scala"))
  
  (general-define-key :keymaps 'scala-mode-map
    :prefix ""
    "<C-return>"     #'newline-or-comment
    "M-j"            #'scala-indent:join-line
    "C-<backspace>"  #'contextual-backspace)

  :init
  (setq scala-indent:use-javadoc-style t
        scala-mode:debug-messages nil)

  :config
  (load "scala-defs")

  (add-hook 'scala-mode-hook #'yas-minor-mode)
  (add-hook 'scala-mode-hook #'global-company-mode)
  (add-hook 'scala-mode-hook #'4lex1v/fix-scala-fonts)
  (add-hook 'scala-mode-hook #'hideshowvis-enable)
  (add-hook 'scala-mode-hook #'hs-minor-mode)

  (push '(scala-mode "\\({\\|(\\)" "\\(}\\|)\\)" "/[*/]" nil nil) hs-special-modes-alist))

(use-package smartparens-scala
  :after scala-mode
  
  :config
  (add-hook 'scala-mode-hook #'smartparens-mode)
  (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))

(use-package sbt-mode
  :after scala-mode
  
  :general
  (:keymaps 'scala-mode-map
   "sb" '(:ignore t :which-key "SBT")
   "sbb" '(4lex1v:open-sbt-build-file :which-key "build.sbt")
   "sbs" #'sbt-start
   "sbr" #'sbt-command
   "sbc" `(4lex1v:sbt-compile-command :which-key "compile"))
  
  :config
  (load "sbt-defuns"))

(use-package ensime
  :after scala-mode
  :commands ensime
  :bind
  (:map scala-mode-map
   ("C-c e" . ensime-print-errors-at-point)
   ("C-c t" . ensime-print-type-at-point)
   ("C-c o" . ensime-import-type-at-point)
   ("C-M-." . ensime-edit-definition-other-window))

  :general
  (:keymaps 'ensime-mode-map
   "ser"  #'ensime-inf-run-scala
   "seb" '(:ignore t :which-key "Build")
   "sebr" #'ensime-sbt-do-run
   "sebc" #'ensime-sbt-do-compile)
  
  (:keymaps 'scala-mode-map
    "se" '(:ignore t :which-key "Ensime")
    "ses"  #'ensime
    "seu"  #'4lex1v:update-ensime-build)
  
  :init
  (setq ensime-server-version        "1.0.0"
        ensime-default-buffer-prefix "ENSIME-"
        ensime-startup-notification   nil
        ensime-startup-snapshot-notification nil)

  (setq-local eldoc-documentation-function #'4lex1v:ensime-eldoc-support)

  :config
  (load "ensime-defuns")
  (unbind-key "M-p" ensime-mode-map)
  
  (add-hook 'scala-mode-hook #'4lex1v:smart-ensime-loader)
  
  (which-key-declare-prefixes-for-mode 'ensime-mode
    "C-c C-d" "ensime/debug"
    "C-c C-c" "ensime/compiler"
    "C-c C-r" "ensime/refactoring"
    "C-c C-t" "ensime/tests"
    "C-c C-v" "ensime/general"))

(use-package ensime-company
  :after (ensime company))

(use-package popup
  :after scala-mode
  :ensure t)
