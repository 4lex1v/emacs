
(use-package rust-mode :defer t)

(use-package cargo :defer t :ensure t
  :after rust-mode
  :general
  (:prefix "," :keymaps 'rust-mode-map
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
  :after rust-mode)

(use-package flycheck-rust :defer t :ensure t
  :after (rust-mode flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
