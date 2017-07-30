;; C# Language Support for Unity3D on mac
(use-package csharp-mode
  :defer t
  :mode ("\\.cs$" . csharp-mode)
  :hooks (hs-minor-mode
          hideshowvis-enable
          smartparens-mode
          yas-minor-mode
          company-mode

          omnisharp-mode))

(use-package omnisharp
  :after csharp-mode
  :init
  ;; Dirty hack for omnisharp compatibility
  (use-package shut-up  :ensure t)
  
  :config
  (add-to-list 'company-backends (company-add-mode-backends '(company-omnisharp))))
