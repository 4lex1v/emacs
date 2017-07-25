(use-package scala-mode
  :defer t
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
  
  (eval-after-load 'org-mode
    (lambda ()
      (add-to-list 'org-babel-load-languages '(scala . t))))
  
  :hooks (4lex1v/fix-scala-fonts
          hs-minor-mode
          hideshowvis-enable
          yas-minor-mode
          company-mode)
  :config
  (load "scala-defs")

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
  (load "sbt-defuns")
  (evil-set-initial-state 'sbt-mode 'emacs))

;; TODO :: override the major mode segment for Ensime activated projects
(use-package ensime
  :after scala-mode
  :commands ensime
  :general
  (:keymaps 'scala-mode-map
   :prefix "<SPC> se"
   ""  '(:ignore t :which-key "Ensime")
   "e" 'ensime
   "u" '4lex1v:update-ensime-build)
  
  (:keymaps 'ensime-mode-map
   :prefix "<SPC> se"
   ""  '(:ignore t :which-key "Ensime")
   "r"  'ensime-inf-run-scala
   "b"  '(:ignore t :which-key "Build")
   "br" 'ensime-sbt-do-run
   "bc" 'ensime-sbt-do-compile)

  ;; Looks like a perfect candidate for a Hydra?
  (:keymaps 'ensime-mode-map
   :prefix ","
   "e" 'ensime-print-errors-at-point
   "t" 'ensime-print-type-at-point
   "o" 'ensime-import-type-at-point
   "g" 'ensime-edit-definition-other-window
   "," 'ensime-pop-find-definition-stack
   "." 'ensime-edit-definition-of-thing-at-point)
  
  :init
  (setq ensime-server-version        "2.0.0-SNAPSHOT"
        ensime-default-buffer-prefix "ENSIME-"
        ensime-startup-notification   nil
        ensime-startup-snapshot-notification nil)

  (setq-local eldoc-documentation-function #'4lex1v:ensime-eldoc-support)

  :config
  (load "ensime-defuns")
  (unbind-key "M-p" ensime-mode-map)
  
  (add-hook 'scala-mode-hook #'4lex1v:smart-ensime-loader)

  ;; The one defined by the Scala-mode for integration with Ensime
  (require 'ob-scala))

(use-package ensime-company
  :after (ensime company))

(use-package popup
  :after scala-mode
  :ensure t)
