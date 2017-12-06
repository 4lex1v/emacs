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
   "oa" 'org-agenda
   "oq" 'org-make-quick-note) 

  (:prefix "" :keymaps 'org-mode-map
   "C-M-j" 'org-metadown
   "C-M-k" 'org-metaup)
  
  :init
  (setq org-log-done                   t
        org-src-fontify-natively       t
        org-descriptive-links          t
        org-startup-with-inline-images t
        
        ;; Std "..." don't look that good...
        org-ellipsis                   "⬎"
        
        org-startup-indented           nil
        org-adapt-indentation          nil
        
        org-hide-leading-stars         nil
        org-line-spacing               5
        org-notes-font                "Menlo"
        
        org-babel-load-languages      '((sql . t)
                                        (shell . t)
                                        (plantuml . t))
        
        org-plantuml-jar-path         "/usr/local/Cellar/plantuml/1.2017.19/libexec/plantuml.jar"
        
        ;; Agenda files to cycle
        org-agenda-files '("~/Sandbox/GTD/game_dev.org"
                           "~/Sandbox/GTD/work.org")
        
        org-quick-note-folder "~/Sandbox/Notes/quick_notes/"

        ;; Templates configuration
        org-capture-templates '(("t" "Task"    entry (file+headline "~/Sandbox/GTD/inbox.org" "Tasks")    "* TODO %t %i%?")
                                ("w" "Work"    entry (file+headline "~/Sandbox/GTD/work.org"  "Tasks")    "* TODO %t %i%?") 
                                ("i" "Ideas"   entry (file+headline "~/Sandbox/GTD/inbox.org" "Ideas")    "* %i%?")
                                ("p" "Project" entry (file+headline "~/Sandbox/GTD/inbox.org" "Projects") "* %i%?")
                                ("n" "Notes"   entry (file+headline "~/Sandbox/GTD/inbox.org" "Notes")    "* %i%?"))

        ;; Keywords
        
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")
                            (sequence "WAITING(w)" "|" "SOMEDAY(s)" "CANCELLED(c)")))

  (defun org-make-quick-note (name)
    (interactive "B")
    (let ((note-path (concat org-quick-note-folder name ".org")))
      (unless (not (file-exists-p note-path))
        (write-region "" nil note-path))
      (find-file note-path)))
  
  :config
  
  ;; Since there's a default Org that comes with emacs, adding this dummy check to ensure that
  ;; whenever I'm using a fresh emacs installation i have the correct package installed
  (if (not (and (boundp 'org-version) (equal org-version "9.1.2")))
      (error "WARNING :: Old Org-mode version is used (%s), check the configuration" org-version))
  
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

