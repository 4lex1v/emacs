(use-package cc-mode
  :defer t
  :mode ("\\.h\\'" . c-mode)
  
  :general
  (:prefix "," :keymaps '(c-mode-map c++-mode-map)
   "a" 'projectile-find-other-file
   "A" 'projectile-find-other-file-other-window)
  
  :init
  (setq org-babel-C-compiler "clang"
        org-babel-C++-compiler "clang++")
  
  :config
  (add-hook 'c-mode-hook 'hs-minor-mode)
  (add-hook 'c-mode-hook 'hideshowvis-minor-mode)
  (add-hook 'c-mode-hook 'smartparens-mode)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'yas-minor-mode)
  (add-hook 'c-mode-hook (company-add-mode-backends 'company-clang))

  (c-toggle-auto-newline t)
  
  (with-eval-after-load "org"
    (add-to-list 'org-babel-load-languages '(C . t))))

(use-package cmake-mode :defer t :after cc-mode)

(use-package disaster
  :after cc-mode
  :commands disaster
  :general (:prefix "," :keymaps '(c-mode-map c++-mode-map) "d" 'disaster))

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
  ;(add-hook 'irony-mode-hook (company-add-mode-backends 'company-irony))
  )

;; (use-package company-irony-c-headers
;;   :ensure t
;;   :after company
;;   :config (progn
;;             (setq company-irony-c-headers--compiler-executable (executable-find "clang++"))
;;             ;; group with company-irony but beforehand so we get first pick
;;             (add-to-list 'company-backends '(company-irony-c-headers company-irony))))

(use-package semantic
  :after cc-mode
  :load-path "modules/native/cedet/lisp/cedet"
  :init
  (setq semantic-default-submodes '(global-semanticdb-minor-mode
                                    global-semantic-idle-scheduler-mode
                                    global-semantic-idle-local-symbol-highlight-mode
                                    global-semantic-highlight-func-mode
                                    global-semantic-idle-completions-mode
                                    global-semantic-decoration-mode))
  
  :config
  ;; Some dependencies
  (use-package company-semantic :after company :commands company-semantic)
  (add-hook 'c-mode-hook (company-add-mode-backends 'company-semantic)))

;; Configure semantic
(add-hook 'change-major-mode-hook
          #'(lambda ()
              (if (derived-mode-p 'c-mode 'c++-mode)
                  (semantic-mode 1)
                (semantic-mode -1))))


