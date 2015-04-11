(use-package ace-jump-mode)
(use-package magit
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package scala-mode2
  :defer t
  :mode ("\\.scala\\'" . scala-mode))

(use-package sbt-mode           :defer t)
(use-package ensime             :defer t)
(use-package haskell-mode       :defer t)


(use-package yasnippet          :defer t)
(use-package smartparens-config :defer t)
(use-package hlinum             :defer t)
(use-package web-mode           :defer t)
(use-package company            :defer t)
