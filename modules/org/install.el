(add-to-list 'load-path (expand-file-name "modules/org/org-mode/lisp" user-emacs-directory))

(setq org-log-done             t
      org-src-fontify-natively t
      org-descriptive-links    t
      org-startup-with-inline-images t
      org-babel-load-languages '((emacs-lisp . t)
                                 (scala      . t)
                                 (haskell    . t)
                                 (shell      . t)))

(with-mode which-key
  (which-key-declare-prefixes
    "C-c o" "org"))

(use-package org
  :after flyspell
  :bind
  (("C-c o l" . org-store-link)
   ("C-c o a" . org-agenda)
   ("C-c o c" . org-capture))
  :bind* ("C-'"  . ace-window)) ;; I believe doesn't make much sense with Evil?

(add-hook 'org-mode-hook #'flyspell-mode)

(setq org-ellipsis "⬎")

(setq org-hide-leading-stars nil)

(setq org-startup-indented nil)

(use-package org-attach-screenshot :after org)

(setq notes-folder "~/Sandbox/Notes")
