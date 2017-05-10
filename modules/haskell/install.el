(use-package haskell-mode-autoloads)

(use-package haskell-mode
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))

  :init
  (setq haskell-process-log t
        haskell-process-type 'stack-ghci))

(use-package haskell
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package haskell-doc-mode
  :config
  (add-hook 'haskell-mode-hook 'haskell-doc-mode))

(use-package haskell-decl-scan
  :config
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode))

