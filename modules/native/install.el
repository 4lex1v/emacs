
;; Configuration for C / C++ / Objective-C / Objective-C++ languages
;;(use-package cc-mode
(use-package cc-mode
  :mode (("\\.h\\'"  . c++-mode)
         ("\\.mm\\'" . objc-mode))
  
  :hooks
  (:c-mode-common-hook
   smartparens-mode
   yas-minor-mode
   company-mode
   hs-minor-mode
   hideshowvis-minor-mode)
  
  :general
  (:keymaps '(c-mode-base-map)
   "m"  '(:ignore t :which-key "Native")
   "ma" 'projectile-find-other-file
   "mA" 'projectile-find-other-file-other-window)
  
  ;; #TODO :: Check if this could be defined in the global configuration or it needs to be overriden for these modes?
  (:prefix "" :keymaps '(c-mode-base-map)  
   "C-S-j" #'next-error
   "C-S-k" #'previous-error
   ",r"    #'recompile)
  
  :init
  (setq org-babel-C-compiler "clang"
        org-babel-C++-compiler "clang++")
  
  :config
  (c-toggle-auto-newline t)
  
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(C . t)))
  
  ;; Something helpful in Handmade Hero... Not sure if i'm going to use it in other projects...
  (font-lock-add-keywords 'c++-mode '(("\\<\\(assert\\|internal\\|global_var\\|local_persist\\)\\>" 1 font-lock-keyword-face)))
  (font-lock-add-keywords 'objc-mode '(("\\<\\(assert\\|internal\\|global_var\\|local_persist\\)\\>" 1 font-lock-keyword-face)))
  
  ;; #TODO :: Add company-xcode for Objective C
  (with-eval-after-load 'company
    (configure-company-backends-for-mode c-mode-common
      `(company-dabbrev
        company-capf
        company-keywords
        company-yasnippet
        company-files
        ,(if (and IS_UNIX (require 'company-clang nil t))
             (function company-clang)))))
  
  (with-eval-after-load 'smartparens
    (sp-local-pair 'c++-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))
  
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(C . t))))

(use-package cmake-mode :ensure t 
  :after cc-mode
  :hooks (company-mode)
  
  :init
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'cmake-mode))
  
  :config
  (with-eval-after-load 'company
    (configure-company-backends-for-mode cmake-mode
      '(company-cmake company-files company-dabbrev company-capf))))

(use-package disaster
  :after cc-mode
  :commands disaster
  :general
  (:prefix "m" :keymaps '(c-mode-base-map)
   "d" 'disaster))

;; (use-package semantic
;;   :after cc-mode
;;   :load-path "modules/native/cedet/lisp/cedet"
  
;;   :general
;;   (:prefix "" :keymaps '(c-mode-base-map)
;;    "g." 'semantic-ia-fast-jump)
  
;;   :init
;;   (setq semantic-default-submodes '(global-semanticdb-minor-mode
;;                                     global-semantic-idle-scheduler-mode
;;                                     global-semantic-idle-local-symbol-highlight-mode
;;                                     global-semantic-highlight-func-mode
;;                                     global-semantic-idle-completions-mode
;;                                     global-semantic-decoration-mode))

;;   ;; #TODO :: Decide what to do with this... For now the performance on Windows is terrible =(
;;   ;;(add-hook 'c-mode-common-hook 'semantic-mode)

;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (if IS_WINDOWS
;;                   (-each win32-system-include-paths 'semantic-add-system-include)))))

(use-package helm-semantic :after (semantic helm))

;; (use-package company-semantic
;;   :after (semantic company)
;;   :config
;;   (configure-company-backends-for-mode semantic-mode
;;     (add-to-list 'company-backends 'company-semantic)))

(use-package sourcetrail :ensure t
  :after cc-mode)

;; Configure semantic
(add-hook 'change-major-mode-hook
          #'(lambda ()
              (if (derived-mode-p 'c-mode 'c++-mode)
                  (semantic-mode 1)
                (semantic-mode -1))))


