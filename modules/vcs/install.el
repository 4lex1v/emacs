(use-package magit :load-path "modules/vcs/magit/lisp"
  :commands (magit-diff magit-clone)
  
  :init
  (setq-default
   magit-submodule-list-columns
   (quote
    (("Path" 50 magit-modulelist-column-path nil)
     ("Version" 35 magit-repolist-column-version nil)
     ("Branch" 20 magit-repolist-column-branch nil)
     ("L<U" 3 magit-repolist-column-unpulled-from-upstream
      ((:right-align t)))
     ("L>U" 3 magit-repolist-column-unpushed-to-upstream
      ((:right-align t)))
     ("L<P" 3 magit-repolist-column-unpulled-from-pushremote
      ((:right-align t)))
     ("L>P" 3 magit-repolist-column-unpushed-to-pushremote
      ((:right-align t))))))

  (setq magit-last-seen-setup-instructions "2.11.0"
        magit-status-show-hashes-in-headers t
        
        ;; Magit Diff configs
        magit-diff-options          '("--stat" "--no-ext-diff" "--word-diff")
        magit-diff-refine-hunk      'all
        magit-diff-paint-whitespace 'status)
  
  (defun magit-diff-visit-file-other-window (file)
    (interactive (list (--if-let (magit-file-at-point)
                           (expand-file-name it)
                         (user-error "No file at point"))))
    (magit-diff-visit-file file t))
  
  
  ;; This function was added to speed up my PR review workflow in a way that i can diff current branch
  ;; with master by a single keystroke...
  (defun magit-diff-branch-with-master ()
    (interactive)
    (let* ((args (magit-diff-arguments))
           (diff-cmd (format "master...%s" (magit-get-current-branch))))
      (magit-diff diff-cmd args)))
  
  :general 
  ("g" '(:ignore t :which-key "Magit")
   "gs"  'magit-status
   "gm"  'magit-dispatch-popup
   "gb"  'magit-blame
   "g'"  'magit-submodule-popup
   "gy"  'magit-show-refs-popup
   "ge"  'magit-ediff-popup
   "gp"  'magit-push-popup
   "gd"  'magit-diff-popup
   "gD"  'magit-diff-branch-with-master
   "gf"  'magit-pull-popup
   "gl" '(:ignore t :which-key "Logging")
   "gll" 'magit-log-all
   "glb" 'magit-log-buffer-file
   "glc" 'magit-log-current)
  
  (:prefix "" :keymaps 'magit-diff-mode-map
   "gf" 'magit-diff-visit-file-other-window)

  (:prefix "" :keymaps 'magit-submodule-list-mode-map
   "RET" 'magit-repolist-status)

  :config
  ;; (evil-set-initial-state 'magit-submodule-list-mode 'emacs)
  
  (add-to-list 'magit-log-arguments "--color")
  (add-to-list 'magit-diff-arguments "--ignore-space-change")

  (magit-define-popup-action 'magit-submodule-popup   
    ?l "List" 'magit-list-submodules)

  (define-key magit-file-section-map [remap magit-visit-thing] #'magit-diff-visit-file-other-window)

  (add-hook 'magit-submodule-list-mode-hook
            (lambda () (setq-local tabulated-list-sort-key (cons "L<U" t)))))

(use-package with-editor :ensure t :demand t
  :after magit
  :general
  (:keymaps 'with-editor-mode-map
    :prefix "" ;; don't use SPC prefix in this case
    "RET"    'with-editor-finish
    [escape] 'with-editor-cancel)
  :config
  (evil-set-initial-state 'with-editor-mode 'insert))

(use-package evil-magit :demand t
  :after magit-mode
  :config
  (evil-magit-init))

(use-package git-undo
  :general
  (:states '(normal visual)
   "g" '(:ignore t :which-key "Git")
   "gu" 'git-undo
   "gU" 'git-undo-browse))

(use-package ssh-agency :if IS_WINDOWS :ensure t
  :after magit
  :init
  (setq ssh-agency-keys (list (expand-file-name "~/.ssh/github_rsa")))
  
  :config
  (setenv "SSH_ASKPASS" "git-gui--askpass")
  (ssh-agency-ensure))

(use-package magithub :ensure t :disabled t
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package smerge-mode
  :general
  (:prefix ""
   :states nil
   :keymaps 'smerge-mode-map

   "C-M-j" 'smerge-next
   "C-M-k" 'smerge-prev))

