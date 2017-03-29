(use-package magit
  :load-path "modules/vcs/magit/lisp"
  :commands (magit-status magit-clone magit-submodule-popup)
  :init

  (setq magit-last-seen-setup-instructions "2.3.2"
        magit-status-show-hashes-in-headers t
        magit-diff-options '("--word-diff")
        magit-diff-refine-hunk 'all) ;; experimental

  (use-package with-editor :ensure t)

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

  :config
  (add-to-list 'magit-log-arguments "--color")
  (magit-define-popup-action 'magit-submodule-popup   
    ?l "List" 'magit-list-submodules)

  (evil-initial-state 'magit-submodule-list-mode 'emacs))

(use-package evil-magit
  :after magit-mode
  :init 
  (evil-leader/set-key
    "ms"  'magit-status
    "mm"  'magit-dispatch-popup
    "mb"  'magit-blame
    "mo"  'magit-submodule-popup
    "my"  'magit-show-refs-popup
    "me"  'magit-ediff-popup
    "mp"  'magit-push-popup
    "mf"  'magit-pull-popup
    "mll" 'magit-log-all
    "mlb" 'magit-log-buffer-file
    "mlc" 'magit-log-current)

  (which-key-declare-prefixes
    "<SPC> m"   "Magit"
    "<SPC> m l" "Logging") 

  (evil-define-key 'normal with-editor-mode-map
    (kbd "RET") 'with-editor-finish
    [escape]    'with-editor-cancel)

  (evil-define-key 'normal git-rebase-mode-map
    "l" 'git-rebase-show-commit)

  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  :config
  (evil-magit-init))

(use-package diff-hl
  :load-path "modules/vcs/diff-hl"
  :config
  (global-diff-hl-mode 1)

  (with-mode magit
    (add-hook 'magit-post-refresh-hook
              #'diff-hl-magit-post-refresh t))

  (use-package diff-hl-flydiff
    :config (diff-hl-flydiff-mode)))

(use-package magithub
  :load-path "modules/vcs/magithub"
  :after magit
  :config
  (magithub-feature-autoinject t))
