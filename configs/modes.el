;; Config for plugins and modes

(defconst custom-config-files (directory-files "~/.emacs.d/configs/modes/" t ".el"))
						
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-install)
(require 'projectile)
(require 'helm-config)
(require 'helm-projectile)
(require 'helm-descbinds)
(require 'yasnippet)
(require 'scala-mode2)
(require 'sbt-mode)
(require 'ensime)
(require 'haskell-mode)
(require 'smartparens-config)
(require 'hlinum)
(require 'ace-jump-mode)
(require 'auto-complete-config)

(mapc 'load custom-config-files)

(provide 'modes)
