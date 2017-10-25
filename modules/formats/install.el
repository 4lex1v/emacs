(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown"
        markdown-open-command "grip -b"))

(use-package yaml-mode :defer t :ensure t)
(use-package toml-mode :defer t :ensure t
  :mode ("/\\(Cargo.lock\\|\\.cargo/config\\)\\'" . toml-mode))

(use-package plantuml-mode :defer t :ensure t)
(use-package esxml :defer t :ensure t)
