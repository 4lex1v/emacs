(use-package yaml-mode
  :mode ("\\.yml\\|\\.yaml$" . yaml-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown"
        markdown-open-command "grip -b"))

(use-package plantuml-mode :defer)

;; (use-package pandoc-mode
;;   :load-path "modules/formats/pandoc-mode"
;;   :after (org markdown-mode))
