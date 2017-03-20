(use-package python
  :mode ("\\.py" . python-mode)
  :after hideshowvis
  :init
  (add-hook 'python-mode-hook #'hideshowvis-enable)
  (add-hook 'python-mode-hook #'hs-minor-mode))

;; Get from Melpa
(use-package pythonic :ensure t :after python)

(use-package anaconda-mode
  :after (python pythonic))
  
