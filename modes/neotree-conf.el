(use-package neotree
  :defer t
  
  :init
  (defun 4lex1v/neotree-projectile-toggle ()
    "Toogle neotree buffer for current projectile root"
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (neotree-projectile-action)))

  :bind ([f8] . 4lex1v/neotree-projectile-toggle))
