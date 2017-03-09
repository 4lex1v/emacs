(use-package yaml-mode
  :load-path "modules/formats/yaml"
  :mode ("\\.yml\\|\\.yaml$" . yaml-mode))

(use-package markdown-mode
  :load-path "modules/formats/markdown-mode"
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; (use-package pandoc-mode
;;   :load-path "modules/formats/pandoc-mode"
;;   :after (org markdown-mode))
