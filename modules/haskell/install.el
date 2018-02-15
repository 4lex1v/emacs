(use-package haskell-mode
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))

  :hooks
  (interactive-haskell-mode
   haskell-doc-mode
   haskell-decl-scan-mode)
  
  :init
  (setq haskell-process-log t
        haskell-process-type 'stack-ghci)
  
  :config
  (evil-set-initial-state 'haskell-error-mode 'emacs)
  
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(haskell . t))))
