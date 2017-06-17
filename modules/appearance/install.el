;; Add themes to the scope
(fnd:attach "themes/zenburn-emacs"
            "themes/dracula"
            "themes/monokai-emacs"
            "themes/emacs-doom-theme"
            "themes/sirthias"
            "themes/solarized-emacs"
            "themes/spacemacs"
            "themes/tao-theme-emacs")

(setq solarized-contrast                   'high
      solarized-visibility                 'high
      solarized-termcolors                  256
      solarized-distinct-fringe-background  t
      solarized-use-variable-pitch          nil
      solarized-use-less-bold               nil
      solarized-use-more-italic             nil
      solarized-high-contrast-mode-line     t
      solarized-emphasize-indicators        t
      x-underline-at-descent-line           t)

(setq doom-enable-bold t
      doom-enable-italic t
      doom-one-brighter-modeline t
      doom-one-brighter-comments t)

;; (require 'doom-themes)
;; (load-theme 'doom-molokai t)

;; (require 'dracula-theme)
;; (load-theme 'dracula t)

(require 'spacemacs-light-theme)
(load-theme 'spacemacs-light t t)

(load "functions")
(load "fonts/pretty-pragmata")
(load "fonts")
(load "configuration")

(use-package spaceline-config)

(use-package spaceline
  :after spaceline-config
  
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        powerline-default-separator 'bar)
  
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  
  :init
  (setq beacon-color (face-attribute 'spaceline-evil-normal :background nil t))
  
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'term-mode)
  (beacon-mode +1))

(use-package hl-line
  :init
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)
  :config
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (global-hl-line-mode))

(use-package hide-mode-line
  )
