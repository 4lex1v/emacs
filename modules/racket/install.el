(use-package racket-mode
  :mode ("\\.rkt[dl]?\\'" . racket-mode)
  :interpreter ("racket" . racket-mode)
  :init
  (setq racket-smart-open-bracket-enable t)
  
  (eval-after-load 'org-mode
    (lambda ()
      (add-to-list 'org-babel-load-languages '(racket . t)))) 

  :config
  (add-hook 'scala-mode-hook #'hideshowvis-enable)
  (add-hook 'scala-mode-hook #'hs-minor-mode)
  (add-hook 'racket-mode-hook #'smartparens-mode)

  (func insert-racket-lambda
    (insert "λ"))
  
  (bind-key "M-\\" 'insert-racket-lambda)
  
  (sp-local-pair 'racket-mode "'" nil :actions nil)
  (sp-local-pair 'racket-mode "`" nil :actions nil))

(use-package ob-racket
  :after racket-mode
  :init
  (setq org-babel-racket-command "/usr/local/bin/racket"))
