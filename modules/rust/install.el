
(use-package rust-mode
  :defer t
  :mode ("\\.rs\\'" . rust-mode))

(use-package racer
  :ensure t
  :after rust-mode)
