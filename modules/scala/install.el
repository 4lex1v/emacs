;; #TODO(4lex1v, 08/28/17) :: For some reason Scala marks `=` as a keyword and highlights it... this needs to be fixed
(use-package scala-mode
  :defer t
  :mode        ("\\.\\(scala\\|sbt\\|sc\\)\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  
  :general
  (:keymaps 'scala-mode-map
   "s" '(:ignore t :which-key "Scala"))
  
  (:keymaps 'scala-mode-map :states '(normal insert) :prefix ""
   "<C-return>"     #'newline-or-comment
   "C-<backspace>"  #'contextual-backspace)

  (:keymaps 'scala-mode-map :states '(normal) :prefix ""
   "C-S-j" #'next-error
   "C-S-k" #'previous-error
   "J" #'scala-join-lines)

  :init
  (setq scala-indent:use-javadoc-style t
        scala-mode:debug-messages nil)
  
  (setq-mode-local scala-mode comment-note-comment-prefix "//")

  (with-eval-after-load "org"
    (add-to-list 'org-babel-load-languages '(scala . t))
    (message "Scala added to the list of Babel"))
  
  :hooks (4lex1v/fix-scala-fonts
          hs-minor-mode
          hideshowvis-enable
          yas-minor-mode
          company-mode)
  
  :config
  (load "scala-defs")

  (configure-company-backends-for-mode scala-mode
    '(company-dabbrev
      company-keywords
      company-yasnippet
      company-capf
      company-files))
  
  (push '(scala-mode "\\({\\|(\\)" "\\(}\\|)\\)" "/[*/]" nil nil) hs-special-modes-alist))

(use-package smartparens-scala
  :after scala-mode
  :config
  (add-hook 'scala-mode-hook #'smartparens-mode))

(use-package sbt-mode
  :after scala-mode
  :general
  (:keymaps 'scala-mode-map :prefix "<SPC> sb"
   ""  '(:ignore t :which-key "SBT")
   "b" '(4lex1v:open-sbt-build-file :which-key "build.sbt")
   "s" 'sbt-start
   "r" 'sbt-command
   "c" '(4lex1v:sbt-compile-command :which-key "compile"))
  
  (:keymaps 'scala-mode-map :prefix ","
   "c" '(4lex1v:sbt-compile-command :which-key "compile")
   "p" 'sbt-run-previous-command)
  
  (:keymaps 'sbt-mode-map :states '(normal insert) :prefix ""
   "C-j" 'compilation-next-error
   "C-k" 'compilation-previous-error)

  :init
  (setq sbt:program-name "sbt -mem 2048 -v")
  
  :config
  (load "sbt-defuns")
  (evil-set-initial-state 'sbt-mode 'normal))

;; TODO :: override the major mode segment for Ensime activated projects
(use-package ensime
  :defer t
  :commands ensime-mode
  :general
  (:keymaps 'scala-mode-map :prefix "<SPC> se"
   ""  '(:ignore t :which-key "Ensime")
   "e" 'ensime
   "u" '4lex1v:update-ensime-build)
  
  (:keymaps 'ensime-mode-map :prefix "<SPC> se"
   ""  '(:ignore t :which-key "Ensime")
   "r"  'ensime-inf-run-scala
   "b"  '(:ignore t :which-key "Build")
   "br" 'ensime-sbt-do-run
   "bc" 'ensime-sbt-do-compile)

  ;; Looks like a perfect candidate for a Hydra?
  (:keymaps 'ensime-mode-map :prefix ","
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
  
  ;;(add-hook 'scala-mode-hook #'4lex1v:smart-ensime-loader)

  ;; The one defined by the Scala-mode for integration with Ensime
  (require 'ob-scala))

(use-package ensime-company
  :after (ensime company))

(use-package popup
  :after scala-mode
  :ensure t)
