(use-package projectile

  :config
  ;; Turn on projectile in all buffers
  (projectile-global-mode)

  (setq projectile-enable-caching              nil
        projectile-completion-system          'helm
        projectile-require-project-root        t)

  ;; Dependencies
  (use-package ibuffer-projectile))
