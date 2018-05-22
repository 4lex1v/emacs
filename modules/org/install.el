
;; #NOTE(4lex1v, 05/18/18) :: This is installed manually via package-list
(use-package org :ensure t :pin gnu
  :after flyspell
  
  :hooks
  (:org-mode-hook
   flyspell-mode
   yas-minor-mode)
  
  :general
  ;; Global Org-mode fucntionality
  ("o" '(org-control-panel/body :which-key "Org"))
  
  (:keymaps 'org-mode-map
   :states  'normal
   :prefix   nil

   "C-M-k" 'org-cut-subtree

   "C-'"   'ace-window
   "C-#"   'helm-org-list-agenda-files
   
   "C-j"   'org-next-visible-heading
   "C-k"   'org-previous-visible-heading)
  
  :init
  (setq org-log-done                   'note ;; When completing a task, prompt for a closing note...
        org-src-fontify-natively       t
        org-descriptive-links          t
        org-startup-with-inline-images t
        org-tags-column 0
        
        org-catch-invisible-edits      'error
        
        org-clock-persist              t
        
        org-hide-leading-stars         nil
        org-line-spacing               5
        org-tags-column                0 ;; Have tags next to the title
        
        org-babel-load-languages      '((sql . t)
                                        (shell . t)
                                        (plantuml . t))
        
        ;; System dependant?
        org-quick-note-folder "~/Sandbox/Notes/quick_notes/"
        
        ;; Templates configuration
        org-capture-templates '(("t" "Task"     entry (file+headline "~/Sandbox/planning/inbox.org" "Tasks")    "* TODO %i%?")
                                ("n" "Notes"    entry (file+headline "~/Sandbox/planning/inbox.org" "Notes")    "* %i%?"))
        
        ;; Keywords
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "ACTIVE" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)"))
        org-todo-keyword-faces '(("ACTIVE" . "yellow")))
  
  (defhydra org-control-panel (:color blue :hint nil)
    "
  General            Agenda         Brain
-----------------------------------------
  _o_: Org Mode    _a_: Weekly    _b_: Org Brain
 ----------------------------------------
"
    ("o" org-mode-control-panel/body)
    ("a" org-agenda-list)
    ("b" brain-control-panel/body)
    
    ("q" nil "cancel"))
  
  (defhydra org-mode-control-panel (:color blue :hint nil)
    "
-------
| Org |  Brain
----------------------------------------
^Capturing^        ^Planning^     ^Timing^
----------------------------------------
_c_: Capture       _a_: Agenda    _t_: Timings
_l_: Store Link    ^ ^            _i_: Clock-in
_n_: Quick Note    ^ ^            _o_: Clock-out
----------------------------------------
"
    ("c" org-capture)
    ("l" org-store-link)
    ("n" org-make-quick-note)
    
    ;; Planning
    ("a" org-agenda)
    
    ;; Timings
    ("t" org-clocks-and-timers/body)
    ("i" org-clock-in)
    ("o" org-clock-out)
    
    ("q" nil "cancel"))
  
  (defhydra org-clocks-and-timers (:color blue :hint nil)
    "
^Clock:^ ^In/out^     ^Edit^   ^Summary^     | ^Timers:^ ^Run^           ^Insert
-^-^-----^-^----------^-^------^-^-----------|--^-^------^-^-------------^------
^ ^      _i_n         _e_dit   _g_oto entry  |  ^ ^      _r_elative      ti_m_e
^ ^      _c_ontinue   _q_uit   _d_isplay     |  ^ ^      cou_n_tdown     i_t_em
^ ^      _o_ut        ^ ^      _r_eport      |  ^ ^      _p_ause toggle
^ ^      ^ ^          ^ ^      ^ ^           |  ^ ^      _s_top
"
    ("i" org-clock-in)
    ("c" org-clock-in-last)
    ("o" org-clock-out)
    
    ("e" org-clock-modify-effort-estimate)
    ("q" org-clock-cancel)

    ("g" org-clock-goto)
    ("d" org-clock-display)
    ("r" org-clock-report)
    ("x" (org-info "Clocking commands"))

    ("r" org-timer-start)
    ("n" org-timer-set-timer)
    ("p" org-timer-pause-or-continue)
    ("s" org-timer-stop)

    ("m" org-timer)
    ("t" org-timer-item)
    ("z" (org-info "Timers")))

  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'outline))
  
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
  ;; Since there's a default Org that comes with emacs, adding this dummy check to ensure that
  ;; whenever I'm using a fresh emacs installation i have the correct package installed
  (if (and (boundp 'org-version) (not (string= (substring org-version 0 1) "9")))
      (warn "WARNING :: Old Org-mode version is used (%s), check the configuration" org-version))
  
  ;; Since this config depends on the runtime value, this should be configured in this section
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  
  (add-to-list 'org-structure-template-alist '("scala" "#+BEGIN_SRC scala \n\t?\n#+END_SRC"))

  ;; Configure hooks for clock's persistance
  (org-clock-persistence-insinuate))

(use-package org-agenda :demand t
  :after org
  :init
  (setq org-agenda-files '("~/Sandbox/Planning/inbox.org"
                           "~/Sandbox/Planning/the_plan.org"
                           "~/Sandbox/Planning/projects.org")
        
        org-agenda-custom-commands
        '(("c" . "My Custom Agendas")
          ("cu"  "Unscheduled"
           ((todo ""
                  ((org-agenda-overriding-header "\nUnscheduled TODO")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
           nil
           nil))
        
        org-archive-location "./archives/%s_archive::"
        org-agenda-archives-mode t
        

        org-agenda-start-on-weekday 6 ;; Saturday
        org-agenda-include-diary nil
        org-agenda-span 'day
        org-agenda-skip-deadline-if-done t
        
        ;; Display agenda in full window
        org-agenda-window-setup 'current-window
        
        org-refile-targets '(("~/Sandbox/Planning/the_plan.org" :level . 1)
                             ("~/Sandbox/Planning/projects.org" :level . 1)))
  :config
  (add-hook 'org-agenda-finalize-hook
            (lambda () (remove-text-properties
                   (point-min) (point-max) '(mouse-face t))))

  (require 'evil-org-agenda)
  (add-hook #'org-agenda-mode-hook 'evil-org-agenda-set-keys))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  
  :config
  (add-hook #'evil-org-mode 'evil-org-set-key-theme))

(use-package org-ref :disabled t
  :after org
  
  :init
  (use-package helm-bibtex :ensure t :demand t :after helm)
  
  (setq reftex-default-bibliography '("~/Sandbox/Library/library.bib"))
  
  (setq bibtex-completion-bibliography "~/Sandbox/Library/library.org"
        bibtex-completion-library-path "~/Sandbox/Library"
        bibtex-completion-notes-path "~/Sandbox/Notes/")
  
  (setq org-ref-notes-directory "~/Sandbox/Notes"
        org-ref-bibliography-notes "~/Sandbox/Library/library.org"
        org-ref-default-bibliography '("~/Sandbox/Library/library.bib")))

(use-package org-brain :demand t
  :after org
  
  ;; :general
  ;; ("ob" '(org-brain-visualize :which-key "Brain"))
  
  :init
  (setq org-brain-path "~/Sandbox/Library/Notes"
        org-brain-visualize-default-choices 'all)
  
  (defhydra brain-control-panel (:color pink :hint nil)
    "
     ---------
 Org | Brain |
----------------------------------------
^Controls^  
----------------------------------------
_v_: Visualize       
----------------------------------------
"
    ("v" org-brain-visualize :color blue)
    ("q" nil "cancel"))
  
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize 'emacs))
  
  :config
  (push '("b" "Brain" plain #'org-brain-goto-end "* %i%?" :empty-lines 1) org-capture-templates))

(use-package org-noter
  :after org
  
  ;; :general
  ;; (:keymaps 'org-mode-map
  ;;  "on" 'org-noter)

  :init
  (setq org-noter-always-create-frame nil
        org-noter-notes-window-location 'vertical-split))

