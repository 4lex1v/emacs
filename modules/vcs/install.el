(use-package magit
  :load-path "modules/vcs/magit/lisp"
  :defer
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
  
  :general 
  ("m" '(:ignore t :which-key "Magit")
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

  :config

  ;; diminish magit-auto-revert-mode
  
  (evil-set-initial-state 'magit-submodule-list-mode 'emacs)
  
  (add-to-list 'magit-log-arguments "--color")

  (magit-define-popup-action 'magit-submodule-popup   
    ?l "List" 'magit-list-submodules))

(use-package with-editor
  :after magit
  :ensure t
  :general
  (:keymaps 'with-editor-mode-map
    :prefix "" ;; don't use SPC prefix in this case
    "RET"    'with-editor-finish
    [escape] 'with-editor-cancel)
  :config
  (evil-set-initial-state 'with-editor-mode 'insert))

(use-package evil-magit
  :after magit-mode
  :config
  (evil-magit-init))

(use-package ssh-agency :if IS_WINDOWS :ensure t
  :after magit
  :init
  (setq ssh-agency-keys '("c:/Users/4lex1v/.ssh/id_rsa"))
  :config
  (setenv "SSH_ASKPASS" "git-gui--askpass")
  (ssh-agency-ensure))

;; (use-package diff-hl
;;   :after magit
;;   :config
;;   (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
;;   (global-diff-hl-mode 1))

; (use-package diff-hl-flydiff
;   :after diff-hl
;   :config 
;   (diff-hl-flydiff-mode)
;   )

;; (use-package magithub
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t))
