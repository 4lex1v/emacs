(use-package jabber
  :defer
  :after exec-path-from-shell
  :init
  (setq jabber-account-list
        `(("4lex1v@livecoding.tv"
           (:password . ,(exec-path-from-shell-getenv "LC_PWD"))))))
