(use-package org
  :load-path "modules/org/org-mode/lisp"
  :after flyspell
  :bind
  (("C-c o l" . org-store-link)
   ("C-c o a" . org-agenda)
   ("C-c o c" . org-capture))
  :bind* ("C-'"  . ace-window)

  :init
  (setq org-log-done             t
        org-src-fontify-natively t
        org-descriptive-links    t
        org-startup-with-inline-images t
        org-babel-load-languages '((emacs-lisp . t)
                                   (scala      . t)
                                   (haskell    . t)
                                   (shell      . t)
                                   (python     . t)
                                   (clojure    . t)
                                   (js         . t))
        org-ellipsis "⬎"
        org-hide-leading-stars nil
        org-startup-indented nil)

  :config
  (with-mode which-key
    (which-key-declare-prefixes
      "C-c o" "org"))

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  
  (add-hook 'org-mode-hook #'flyspell-mode))

(use-package ob :after org)

;; (use-package org-bullets
;;   :after org
;;   :init
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-beautify
  :after org
  :ensure org-beautify-theme
  :config
  (load-theme 'org-beautify t))

(use-package org-attach-screenshot :after org)

(setq notes-folder "~/Sandbox/Notes")
