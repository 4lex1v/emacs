(use-package org :demand t
  :load-path "modules/org/org-mode/lisp"
  :after flyspell
  
  :bind* ("C-'"  . ace-window)
  
  :general
  ;; Global Org-mode related keybindings
  (:prefix ""
   "C-#"   'helm-org-list-agenda-files)
  
  ;; Global Org-mode fucntionality
  ("o" '(:ignore t :which-key "Global Org")
   "oc" 'org-capture
   "ol" 'org-store-link
   "oa" 'org-agenda
   "oq" 'org-make-quick-note) 

  (:prefix "" :keymaps 'org-mode-map
   "C-j" 'org-next-visible-heading
   "C-k" 'org-previous-visible-heading

   "C-M-j" 'org-metadown
   "C-M-k" 'org-metaup)
  
  :init
  (setq org-log-done                   'note ;; When completing a task, prompt for a closing note...
        org-src-fontify-natively       t
        org-descriptive-links          t
        org-startup-with-inline-images t
        org-tags-column 0
        
        org-catch-invisible-edits      'error
        
        org-startup-indented           nil
        org-adapt-indentation          nil
        
        org-hide-leading-stars         nil
        org-line-spacing               5
        org-tags-column               0 ;; Have tags next to the title
        
        org-babel-load-languages      '((sql . t)
                                        (shell . t)
                                        (plantuml . t))
        
        ;; System dependant?
        org-quick-note-folder "~/Sandbox/Notes/quick_notes/"

        ;; Agenda Configuration
        org-agenda-start-on-weekday 6 ;; Saturday
        
        
        ;; Templates configuration
        org-capture-templates '(("t" "Task"    entry (file+headline "~/Sandbox/planning/inbox.org" "Tasks")    "* TODO %i%?")
                                ("i" "Ideas"   entry (file+headline "~/Sandbox/planning/inbox.org" "Ideas")    "* %i%?")
                                ("p" "Project" entry (file+headline "~/Sandbox/planning/inbox.org" "Projects") "* %i%?")
                                ("n" "Notes"   entry (file+headline "~/Sandbox/planning/inbox.org" "Notes")    "* %i%?"))
        
        ;; Refile configuration
        org-refile-targets '((org-agenda-files :maxlevel . 1))
        
        ;; Keywords
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")
                            (sequence "WAITING(w)" "SOMEDAY(s)" "|" "CANCELLED(c)"))
        
        org-todo-keyword-faces '(("SOMEDAY" . "yellow") ("WAITING" . "yellow")))

  (defun org-make-quick-note (name)
    (interactive "B")
    (let ((note-path (concat org-quick-note-folder name ".org")))
      (unless (not (file-exists-p note-path))
        (write-region "" nil note-path))
      (find-file note-path)))
  
  (defun helm-org-list-agenda-files ()
    (interactive)
    (helm :sources (helm-build-sync-source "Org-mode Agenda Files:"
                     :candidates 'org-agenda-files
                     :fuzzy-match t
                     :action 'find-file)))
  
  :config
  (evil-set-initial-state 'org-agenda-mode 'motion)
  
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

(use-package org-annotate-file :after org
  :init
  (setq org-annotate-file-storage-file "~/Sandbox/planning/annotations.el"))

