(use-package evil-leader
  :load-path "platform/evil/evil-leader"
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

(use-package evil
  :load-path "platform/evil/evil-core"
  :after evil-leader ;; To enable evil-leader in initial buffers
  :init
  (setq evil-default-cursor t)
  :config
  (add-hook 'prog-mode-hook #'evil-normal-state)

  ;; Use `§' key to switch between emacs and normal state
  (evil-global-set-key 'normal "§" #'evil-emacs-state)
  (evil-global-set-key 'emacs  "§" #'evil-exit-emacs-state)
 
  (evil-ex-define-cmd "e[val]" #'eval-buffer)
  (evil-mode))
