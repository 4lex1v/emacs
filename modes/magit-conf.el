(use-package magit
  :defer t
  
  :diminish magit-auto-revert-mode

  :init
  (setq magit-last-seen-setup-instructions "1.4.0")

  :bind ("C-c s" . magit-status))
  
