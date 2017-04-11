(use-package scala-mode
  :defer
  :mode        ("\\.\\(scala\\|sbt\\|sc\\)\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  
  :general
  (:keymaps 'scala-mode-map
   "s" '(:ignore t :which-key "Scala"))
  
  (:keymaps 'scala-mode-map
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
   :prefix "<SPC> sb"
   ""  '(:ignore t :which-key "SBT")
   "b" '(4lex1v:open-sbt-build-file :which-key "build.sbt")
   "s" 'sbt-start
   "r" 'sbt-command
   "c" '(4lex1v:sbt-compile-command :which-key "compile"))
 
  :config
  (load "sbt-defuns"))

(use-package ensime
  :after scala-mode
  :commands ensime

  :general
  (:keymaps 'scala-mode-map
   :prefix "<SPC> se"
   ""  '(:ignore t :which-key "Ensime")
   "s" 'ensime
   "u" '4lex1v:update-ensime-build)
  
  (:keymaps 'ensime-mode-map
   :prefix "<SPC> se"
   ""  '(:ignore t :which-key "Ensime")
   "r"  'ensime-inf-run-scala
   "b"  '(:ignore t :which-key "Build")
   "br" 'ensime-sbt-do-run
   "bc" 'ensime-sbt-do-compile)

  (:keymaps 'scala-mode-map
   :prefix ","
   "e" 'ensime-print-errors-at-point
   "t" 'ensime-print-type-at-point
   "o" 'ensime-import-type-at-point
   "g" 'ensime-edit-definition-other-window)
  
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
