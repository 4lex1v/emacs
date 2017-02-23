(use-package jabber
  :load-path "modules/communications/jabber"
  :after exec-path-from-shell
  :init
  (setq jabber-account-list
        `(("4lex1v@livecoding.tv"
           (:password . ,(exec-path-from-shell-getenv "LC_PWD"))))))

;; TODO :: should this be here?
(use-package oauth2 :ensure t)

(use-package slack
  :after oauth2
  :load-path "modules/communications/slack")
