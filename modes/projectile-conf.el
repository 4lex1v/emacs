(use-package projectile
  :commands projectile-global-mode

  :defer 5

  :init 
  (projectile-global-mode)

  :bind-keymap ("C-c p" . projectile-command-map)

  :config
  (setq projectile-enable-caching              nil
        projectile-require-project-root        t)

  (use-package helm-config)
  (use-package ibuffer-projectile)
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))
