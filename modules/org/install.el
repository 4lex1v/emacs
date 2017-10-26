(use-package org
  :defer t
  :load-path "modules/org/org-mode/lisp"
  :after flyspell
  
  :bind* ("C-'"  . ace-window)
  
  :general
  ;; Global Org-mode fucntionality
  ("o" '(:ignore t :which-key "Global Org")
   "oc" 'org-capture
   "ol" 'org-store-link
   "oa" 'org-agenda) 

  (:prefix "" :keymaps 'org-mode-map
   "C-M-j" 'org-metadown
   "C-M-k" 'org-metaup)
  
  :init
  (setq org-log-done                   t
        org-src-fontify-natively       t
        org-descriptive-links          t
        org-startup-with-inline-images t
        org-ellipsis                   "⬎"
        org-hide-leading-stars         nil
        org-startup-indented           nil
        org-line-spacing               5
        org-notes-font                "Menlo"
        
        org-babel-load-languages      '((sql . t))
        
        ;; Agenda files to cycle
        org-agenda-files '("~/Sandbox/GTD/game_dev.org"
                           "~/Sandbox/GTD/work.org")

        ;; Templates configuration
        org-capture-templates '(("a" "Article" entry (file+headline "~/Sandbox/Articles/Articles.org" "Article") "* %i%?")
                                ("t" "Task"    entry (file+headline "~/Sandbox/GTD/inbox.org" "Tasks")    "* %i%?")
                                ("p" "Project" entry (file+headline "~/Sandbox/GTD/inbox.org" "Projects") "* %i%?"))

        ;; Keywords
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  :config
  ;; Since this config depends on the runtime value, this should be configured in this section
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  
  (add-to-list 'org-structure-template-alist '("scala" "#+BEGIN_SRC scala \n\t?\n#+END_SRC"))
  
  (add-hook 'org-mode-hook #'(lambda () (setq-local line-spacing org-line-spacing)))
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'toggle-truncate-lines))

(use-package ob :after org)

(use-package org-beautify
  :after org
  :ensure org-beautify-theme
  :config
  (load-theme 'org-beautify t))

