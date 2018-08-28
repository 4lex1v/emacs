;; #TODO(4lex1v, 08/28/17) :: For some reason Scala marks `=` as a keyword and highlights it... this needs to be fixed
;; #TODO :: Turn off flycheck mode in SBT files or find a proper check extension
(use-package scala-mode 
  :mode        ("\\.\\(scala\\|sbt\\|sc\\)\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  
  :hooks
  (4lex1v/fix-scala-fonts
   smartparens-mode
   yas-minor-mode
   company-mode
   hs-minor-mode
   hideshowvis-minor-mode)
  
  :general
  (:keymaps 'scala-mode-map
   "s" '(:ignore t :which-key "Scala"))
  
  (:keymaps 'scala-mode-map
   :states  '(normal insert)
   :prefix   nil
   
   "<C-return>"     #'newline-or-comment)

  (:keymaps 'scala-mode-map
   :states  'normal
   :prefix   nil
   
   "J" #'scala-join-lines)

  :init
  (setq scala-indent:use-javadoc-style t
        scala-mode:debug-messages nil)

  (setq-mode-local scala-mode comment-note-comment-prefix "//")
  
  :config
  (load "scala-defs")

  (with-eval-after-load 'company
    (configure-company-backends-for-mode scala-mode
      '(company-dabbrev
        company-keywords
        company-capf
        company-yasnippet
        company-files)))
  
  (with-eval-after-load 'hideshowvis
    (push '(scala-mode "\\({\\|(\\)" "\\(}\\|)\\)" "/[*/]" nil nil) hs-special-modes-alist)))

(use-package smartparens-scala :demand t
  :after (:all smartparens scala-mode)
  :config
  (message "Smartparens for Scala has been configured"))

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
   "r" 'sbt-run-previous-command
   "i" '4lex1v/open-in-intellij)

  :init
  (setq sbt:program-name "sbt shell -mem 2048 -v"
        sbt:prompt-regexp  "^\\(\\(scala\\|\\[[^\]]*\\] \\)?[>$]\\|[ ]+|\\)[ ]*")
  
  :config
  (load "sbt-defuns")
  (setq-default truncate-lines t)
  (evil-set-initial-state 'sbt-mode 'insert))

;; TODO :: override the major mode segment for Ensime activated projects
(use-package ensime :disabled t
  :after scala-mode
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
  (setq ensime-default-buffer-prefix "ENSIME-"
        ensime-startup-notification   nil
        ensime-startup-snapshot-notification nil)

  :config
  (load "ensime-defuns")
  (unbind-key "M-p" ensime-mode-map)

  (setq-mode-local scala-mode eldoc-documentation-function #'4lex1v:ensime-eldoc-support)

  ;; The one defined by the Scala-mode for integration with Ensime
  (with-eval-after-load 'org
   (require 'ob-scala)))

(use-package ensime-company
  :after (:all ensime company))

(use-package popup :after scala-mode :ensure t)
