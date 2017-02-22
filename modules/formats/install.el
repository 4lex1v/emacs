(use-package yaml-mode
  :load-path "packages/yaml"
  :mode ("\\.yml$" . yaml-mode))

(use-package markdown-mode
  :load-path "packages/markdown-mode"
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
