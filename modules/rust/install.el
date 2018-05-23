
(use-package rust-mode 
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
        rust-format-on-save nil
        rust-toolchain-path (run-shell-command "rustc --print sysroot"))
  
  :config
  (sp-with-modes 'rust-mode
    (sp-local-pair "(" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))

  ;; (configure-company-backends-for-mode rust-mode
  ;;   '(company-dabbrev
  ;;     company-keywords
  ;;     company-yasnippet
  ;;     company-capf
  ;;     company-files))
  )

(use-package smartparens-rust
  :after (rust-mode smartparens-mode)
  :config
  (add-hook 'rust-mode #'smartparens-rust))

(use-package cargo :ensure t
  :after rust-mode
  
  :general
  (:prefix ","
   :keymaps 'rust-mode-map
   
   "c" '(:ignore t :which-key "Cargo")
   "c." 'cargo-process-repeat
   "cC" 'cargo-process-clean
   "cX" 'cargo-process-run-example
   "cb" 'cargo-process-build
   "cc" 'cargo-process-check
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
   "t" 'cargo-process-test))

(use-package racer :ensure t
  :after rust-mode
  
  :general
  (:prefix "" :keymaps 'racer-mode-map
   "gd" 'racer-find-definition
   "g." 'racer-find-definition-other-window)
  
  :init
  (setq racer-rust-src-path (concat rust-toolchain-path "/lib/rustlib/src/rust/src"))
  
  :config
  (defun racer-find-definition-other-window ()
    "Run the racer find-definition command and process the results in other window."
    (interactive)
    (-if-let (match (--first (s-starts-with? "MATCH" it)
                             (racer--call-at-point "find-definition")))
        (-let [(_name line col file _matchtype _ctx)
               (s-split-up-to "," (s-chop-prefix "MATCH " match) 5)]
          (if (fboundp 'xref-push-marker-stack)
              (xref-push-marker-stack)
            (with-no-warnings
              (ring-insert find-tag-marker-ring (point-marker))))
          (switch-to-buffer-other-window file)
          (save-selected-window
            (racer--find-file file (string-to-number line) (string-to-number col))))
      (error "No definition found")))
  
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package company-racer :ensure t :demand t
  :after (racer company)
  
  :config
  (with-eval-after-load 'company
    (configure-company-backends-for-mode rust-mode
      '(company-dabbrev
        company-racer
        company-keywords))))

(use-package flycheck-rust :ensure t
  :after (rust-mode flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
