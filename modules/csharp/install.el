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
  (use-package shut-up :after csharp-mode :ensure t)

  (setq omnisharp-company-do-template-completion t)
        ;omnisharp-server-executable-path "/usr/local/bin/omnisharp")

  :general
  (:keymaps 'omnisharp-mode-map :prefix ""
   
   "gd" 'omnisharp-go-to-definition
   "g." 'omnisharp-go-to-definition-other-window
   "gI" 'omnisharp-find-implementations
    "." 'omnisharp-add-dot-and-auto-complete)

  (:keymaps 'omnisharp-mode-map :prefix ","
   
   ;; Omnisharp Server
    "s" '(:ignore t :which-key "Server")
   "ss" 'omnisharp-start-omnisharp-server
   "sS" 'omnisharp-stop-server
   "sr" 'omnisharp-reload-solution

   ;; Help & Docs
    "h" '(:ignore t :which-key "Help&Docs")
   "ht" 'omnisharp-current-type-information)
  
  :config
  (add-hook 'omnisharp-mode-hook (company-add-mode-backends '(company-omnisharp))))
