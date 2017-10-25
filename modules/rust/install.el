
(use-package rust-mode :defer t
  :hooks (;; Rust-specific modes
          cargo-minor-mode
          racer-mode
          
          ;; General Modes
          hs-minor-mode
          hideshowvis-enable
          yas-minor-mode
          smartparens-mode
          company-mode)
  
  :init 
  (setq rust-indent-offset  2
        rust-format-on-save t
        rust-toolchain-path (run-shell-command "rustc --print sysroot"))
  
  :config
  (sp-with-modes 'rust-mode
    (sp-local-pair "(" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))))

(use-package smartparens-rust
  :after (rust-mode smartparens-mode)
  :config
  (add-hook 'rust-mode #'smartparens-rust))

(use-package cargo :defer t :ensure t
  :after rust-mode
  :general
  (:prefix "," :keymaps 'rust-mode-map
   "c" '(:ignore t :which-key "Cargo")
   "c." 'cargo-process-repeat
   "cC" 'cargo-process-clean
   "cX" 'cargo-process-run-example
   "cc" 'cargo-process-build
   "cd" 'cargo-process-doc
   "ce" 'cargo-process-bench
   "cf" 'cargo-process-current-test
   "cf" 'cargo-process-fmt
   "ci" 'cargo-process-init
   "cn" 'cargo-process-new
   "co" 'cargo-process-current-file-tests
   "cs" 'cargo-process-search
   "cu" 'cargo-process-update
   "cx" 'cargo-process-run
   "t" 'cargo-process-test)))

(use-package racer :defer t :ensure t
  :after rust-mode
  
  :general
  (:prefix "" :keymaps 'racer-mode-map
   "g." 'racer-find-definition)
  
  :init
  (setq racer-rust-src-path (concat rust-toolchain-path "/lib/rustlib/src/rust/src"))
  
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package company-racer :defer t :ensure t
  :after (racer company))

(use-package flycheck-rust :defer t :ensure t
  :after (rust-mode flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
