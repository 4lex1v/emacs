(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown"
        markdown-open-command "grip -b"))

(use-package yaml-mode :ensure t
  :mode ("\\.yml\\|\\.yaml$" . yaml-mode))

(use-package plantuml-mode :defer t :ensure t)
(use-package esxml :defer t :ensure t)
