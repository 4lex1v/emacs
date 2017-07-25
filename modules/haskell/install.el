(use-package haskell-mode-autoloads
  :defer)

(use-package haskell-mode
  :defer 
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))

  :init
  (setq haskell-process-log t
        haskell-process-type 'stack-ghci)
  
  (eval-after-load 'org-mode
    (lambda ()
      (add-to-list 'org-babel-load-languages '(haskell . t))))
  
  :config
  (evil-set-initial-state 'haskell-error-mode 'emacs))

(use-package haskell
  :defer
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package haskell-doc-mode
  :defer
  :config
  (add-hook 'haskell-mode-hook 'haskell-doc-mode))

(use-package haskell-decl-scan
  :defer
  :config
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode))

