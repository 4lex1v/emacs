(use-package cc-mode
  :defer t
  :mode ("\\.h\\'" . c-mode)
  
  :general
  (:keymaps '(c-mode-map c++-mode-map objc-mode-map)
   "m"  '(:ignore t :which-key "Native")
   "ma" 'projectile-find-other-file
   "mA" 'projectile-find-other-file-other-window)
  
  ;; #TODO :: Check if this could be defined in the global configuration or it needs to be overriden for these modes?
  (:prefix "" :keymaps '(c-mode-map c++-mode-map objc-mode-map)  
   "C-S-j" #'next-error
   "C-S-k" #'previous-error)
  
  :init
  (setq org-babel-C-compiler "clang"
        org-babel-C++-compiler "clang++")
  
  :config
  (add-hook 'c-mode-hook 'hs-minor-mode)
  (add-hook 'c-mode-hook 'hideshowvis-minor-mode)
  (add-hook 'c-mode-hook 'smartparens-mode)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'yas-minor-mode)
  
  (configure-company-backends-for-mode c-mode
    '(company-dabbrev
      company-keywords
      company-clang
      company-yasnippet
      company-capf
      company-files))

  (c-toggle-auto-newline t)
  
  (with-eval-after-load "org"
    (add-to-list 'org-babel-load-languages '(C . t)))
  
  (sp-local-pair 'c++-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))

(use-package cmake-mode :ensure t :defer t
  :after cc-mode
  :hooks (company-mode)
  :config
  (configure-company-backends-for-mode cmake-mode
    '(company-dabbrev
      company-yasnippet
      company-cmake
      company-capf)))

(use-package disaster
  :after cc-mode
  :commands disaster
  :general (:prefix "," :keymaps '(c-mode-map c++-mode-map objc-mode-map) "d" 'disaster))

(use-package semantic
  :after cc-mode
  :load-path "modules/native/cedet/lisp/cedet"
  :init
  (setq semantic-default-submodes '(global-semanticdb-minor-mode
                                    global-semantic-idle-scheduler-mode
                                    global-semantic-idle-local-symbol-highlight-mode
                                    global-semantic-highlight-func-mode
                                    global-semantic-idle-completions-mode
                                    global-semantic-decoration-mode)))

(use-package company-semantic
  :after (semantic company)
  :config
  (configure-company-backends-for-mode semantic-mode
    (add-to-list 'company-backends 'company-semantic)))

;; Configure semantic
(add-hook 'change-major-mode-hook
          #'(lambda ()
              (if (derived-mode-p 'c-mode 'c++-mode)
                  (semantic-mode 1)
                (semantic-mode -1))))


