;; This file contains config for std modes and plugins like ido, dired, autocomplete, etc... 
;; Other pluging which requires more detailed configs (e.g scala, haskell, yasnippet, etc...) 
;; are stored in other separate files

;; DIRED
(require 'dired)
(setq delete-by-moving-to-trash t)

;; IDO
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-use-faces nil)
(setq ido-enable-flex-matching t)

;; PROJECTILE
(require 'projectile)
(projectile-global-mode)

;; SMEX
;; Extends std M-x functionality providing some history, better command
;; search capabilities + can show commands only for the current major mode
(require 'smex)
(smex-initialize)

;; ACE-JUMP-MODE
(require 'ace-jump-mode)

;; NEOTREE
(require 'neotree)

;; AUTO-COMPLETE
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20140726.303")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140726.303/ac-dict")
(ac-config-default)
