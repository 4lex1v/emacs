(use-package cmake-mode)

(use-package cc-mode
  :defer t
  :hooks (hs-minor-mode
          hideshowvis-minor-mode
          smartparens-mode
          company-mode)
  :config
  (add-to-list 'company-backends 'company-clang))
  
(use-package irony
  :after cc-mode
  
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  
  (defun setup-c-clang-options ()
    (setq irony-additional-clang-options (quote ("-std=c11"))))

  (defun setup-cpp-clang-options ()
    (setq irony-additional-clang-options (quote ("-std=c++14" "-stdlib=libc++"))))
  
  :config
  (add-hook 'c++-mode-hook 'setup-cpp-clang-options)
  (add-hook 'c-mode-hook 'setup-c-clang-options))

(use-package company-irony
  :after (irony company)
  :config
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (add-to-list 'company-backends 'company-irony))

;; (use-package company-irony-c-headers
;;   :ensure t
;;   :after company
;;   :config (progn
;;             (setq company-irony-c-headers--compiler-executable (executable-find "clang++"))
;;             ;; group with company-irony but beforehand so we get first pick
;;             (add-to-list 'company-backends '(company-irony-c-headers company-irony))))

;; C# Language Support for Unity3D on mac
(use-package csharp-mode
  :defer t
  :mode ("\\.cs$" . csharp-mode)
  :hooks (hs-minor-mode hideshowvis-enable smartparens-mode))

(use-package omnisharp
  :after csharp-mode
  :init
  ;; Dirty hack for omnisharp compatibility
  (use-package shut-up  :ensure t))
