;; SBT mode configuration

(add-hook 'scala-mode-hook 
  '(lambda ()
		 (yas-minor-mode)
		 (hs-minor-mode)
		 (ensime-scala-mode-hook)))
															




						 

