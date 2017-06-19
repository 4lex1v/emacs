(use-package racket-mode
  :mode ("\\.rkt[dl]?\\'" . racket-mode)
  :interpreter ("racket" . racket-mode)
  :init
  (setq racket-smart-open-bracket-enable t)

  :config
  (add-hook 'scala-mode-hook #'hideshowvis-enable)
  (add-hook 'scala-mode-hook #'hs-minor-mode)
  (add-hook 'racket-mode-hook #'smartparens-mode)

  (func insert-racket-lambda
    (insert "λ"))
  
  (bind-key "M-\\" 'insert-racket-lambda)
  
  (sp-local-pair 'racket-mode "'" nil :actions nil)
  (sp-local-pair 'racket-mode "`" nil :actions nil))
