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

(use-package evil-snipe :after evil)

(use-package evil-args
  :after evil
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))
