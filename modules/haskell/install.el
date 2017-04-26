(use-package haskell-mode
  :mode "\\.hs\\'"
  :defer t
  :commands haskell-mode
  
  :init
  (setq haskell-process-log t
        haskell-process-type (quote stack-ghci))
  
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode))
