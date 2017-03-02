(use-package magit
  :load-path "modules/vcs/magit/lisp"
  :commands magit-clone
  :bind
  (("C-c m s" . magit-status)
   ("C-c m m" . magit-dispatch-popup)
   ("C-c m b" . magit-blame)
   ("C-c m o" . magit-submodule-popup)
   ("C-c m y" . magit-show-refs-popup)
   ("C-c m e" . magit-ediff-popup)
   
   ;; Git Logging
   ("C-c m l l" . magit-log-all)
   ("C-c m l b" . magit-log-buffer-file)
   ("C-c m l c" . magit-log-current)

   ("C-c m p" . magit-push-popup)
   ("C-c m f" . magit-pull-popup))

  :init
  (setq magit-last-seen-setup-instructions "2.3.2"
        magit-status-show-hashes-in-headers t)

  (unbind-key "C-c m")

  ;; TODO :: should `with-editor' be installed in the 'core' folder?
  (use-package with-editor :ensure t)

  (with-mode which-key 
    (which-key-declare-prefixes "C-c m" "magit")
    (which-key-declare-prefixes "C-c m l" "magit/log"))

  :config
  (add-to-list 'magit-log-arguments "--color"))

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