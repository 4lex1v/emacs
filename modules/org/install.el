(use-package org
  :defer t
  :load-path "modules/org/org-mode/lisp"
  :after flyspell
  :bind* ("C-'"  . ace-window)
  :general
  ;; Global Org-mode fucntionality
  ("eo" '(:ignore t :which-key "Global Org")
   "eoc" 'org-capture
   "eol" 'org-store-link
   "eoa" 'org-agenda) 
  
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
        
        ;; Agenda files to cycle
        org-agenda-files '("~/Sandbox/GTD/main.org")

        ;; Templates configuration
        org-capture-templates '(("t" "Task"    entry (file+headline "~/Sandbox/GTD/main.org" "Tasks")    "* %i%?")
                                ("p" "Project" entry (file+headline "~/Sandbox/GTD/main.org" "Projects") "* %i%?"))

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

(use-package org-attach-screenshot :after org)
