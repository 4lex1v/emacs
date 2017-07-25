(use-package python
  :mode ("\\.py" . python-mode)
  :after hideshowvis
  
  :init
  (eval-after-load 'org-mode
    (lambda ()
      (add-to-list 'org-babel-load-languages '(python . t)))) 
  
  :config
  (add-hook 'python-mode-hook #'hideshowvis-enable)
  (add-hook 'python-mode-hook #'hs-minor-mode))

;; Get from Melpa
(use-package pythonic :ensure t :after python)

(use-package anaconda-mode
  :after (python pythonic))

