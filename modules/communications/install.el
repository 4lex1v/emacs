(use-package jabber
  :load-path "packages/emacs-jabber"
  :init
  (setq jabber-account-list
        `(("4lex1v@livecoding.tv"
           (:password . ,(exec-path-from-shell-getenv "LC_PWD"))))))
