(use-package magit
  :load-path "modules/vcs/magit/lisp"
  :after eshell
  :commands (magit-status magit-clone magit-submodule-popup)
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

  (setq magit-last-seen-setup-instructions "2.3.2"
        magit-status-show-hashes-in-headers t
        magit-diff-options '("--word-diff")
        magit-diff-refine-hunk 'all) ;; experimental

  :config
  (add-to-list 'magit-log-arguments "--color")

  (magit-define-popup-action 'magit-submodule-popup   
    ?l "List" 'magit-list-submodules)

  (evil-initial-state 'magit-submodule-list-mode 'emacs))

(use-package with-editor
  :after magit
  :ensure t
  :defer)

(use-package evil-magit
  :after magit-mode
  :init 
  (general-define-key
    "m" '(:ignore t :which-key "Magit")
    "ms"  'magit-status
    "mm"  'magit-dispatch-popup
    "mb"  'magit-blame
    "mo"  'magit-submodule-popup
    "my"  'magit-show-refs-popup
    "me"  'magit-ediff-popup
    "mp"  'magit-push-popup
    "mf"  'magit-pull-popup
    "ml" '(:ignore t :which-key "Logging")
    "mll" 'magit-log-all
    "mlb" 'magit-log-buffer-file
    "mlc" 'magit-log-current)

  (general-define-key :keymaps 'with-editor-mode-map
    "RET" 'with-editor-finish
    [escape]    'with-editor-cancel)

  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  :config
  (evil-magit-init))

(use-package diff-hl
  :after magit
  :init
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh t)
  :config
  (global-diff-hl-mode 1))

(use-package diff-hl-flydiff
  :after diff-hl
  :config (diff-hl-flydiff-mode))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t))
