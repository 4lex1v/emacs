(use-package python
  :mode ("\\.py" . python-mode)
  :after hideshowvis
  :init
  (use-package anaconda-mode)
  (add-hook 'python-mode-hook #'hideshowvis-enable)
  (add-hook 'python-mode-hook #'hs-minor-mode))
