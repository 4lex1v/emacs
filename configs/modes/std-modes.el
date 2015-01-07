;; This file contains config for std modes and plugins like ido, dired, autocomplete, etc... 
;; Other pluging which requires more detailed configs (e.g scala, haskell, yasnippet, etc...) 
;; are stored in other separate files

;; YASnippet
(setq yas-snippet-dirs '("~/.emacs.d/snippets")) ;; personal snippets
(yas-global-mode 1)
(yas-reload-all) ;; we need to reload yas after adding custom dir

;; SMARTPARENS
(setq sp-highlight-pair-overlay nil)

;; Turn on HS mode
(add-hook 'less-css-mode-hook 'hs-minor-mode)
