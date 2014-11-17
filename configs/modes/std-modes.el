;; This file contains config for std modes and plugins like ido, dired, autocomplete, etc... 
;; Other pluging which requires more detailed configs (e.g scala, haskell, yasnippet, etc...) 
;; are stored in other separate files

;; YASnippet
(setq yas-snippet-dirs '("~/.emacs.d/snippets")) ;; personal snippets
(yas-reload-all) ;; we need to reload yas after adding custom dir

;; DIRED
;; (setq delete-by-moving-to-trash t)

;; IDO
;; (ido-mode 0)
;; (ido-everywhere 0)
;; (setq ido-use-faces nil)
;; (setq ido-enable-flex-matching t)

;; SMEX
;; Extends std M-x functionality providing some history, better command
;; search capabilities + can show commands only for the current major mode
;; Still using for mode-private commands M-X
;; (smex-initialize)

;; AUTO-COMPLETE
;; (add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20140726.303")
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140726.303/ac-dict")
;; (ac-config-default)

;; SMARTPARENS
(setq sp-highlight-pair-overlay nil)
