;; Config for plugins and modes
						
;; Basic workflow packages
(use-package projectile)
(use-package ibuffer-projectile)
(use-package helm-config)
(use-package helm-projectile)
(use-package helm-descbinds)
(use-package neotree)
(use-package ace-jump-mode)

(use-package org-install        :defer t)
(use-package yasnippet          :defer t)
(use-package scala-mode2        :defer t)
(use-package sbt-mode           :defer t)
(use-package ensime             :defer t)
(use-package haskell-mode       :defer t)
(use-package smartparens-config :defer t)
(use-package hlinum             :defer t)
(use-package web-mode           :defer t)
(use-package company            :defer t)

;; (mapc 'load custom-config-files)

(provide 'modes)
