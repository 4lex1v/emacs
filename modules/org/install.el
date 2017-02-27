(use-package org
  :load-path "modules/org/org-mode"
  :after flyspell
  :bind
  (("C-c o l" . org-store-link)
   ("C-c o a" . org-agenda)
   ("C-c o c" . org-capture))
  :bind* ("C-'"  . ace-window)
  :init
  (setq org-log-done             t
        org-src-fontify-natively t
        org-descriptive-links    nil
        org-startup-with-inline-images t
        org-babel-load-languages '((emacs-lisp . t)
                                   (scala      . t)
                                   (haskell    . t))
        org-descriptive-links t) 

  (with-mode which-key
    (which-key-declare-prefixes
      "C-c o" "org"))
  
  (add-hook 'org-mode-hook #'flyspell-mode))
