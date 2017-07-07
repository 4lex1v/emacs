(defconst theme-to-load 'dracula)

(load "functions")
(load "fonts/pretty-pragmata")
(load "fonts")
(load "configuration")

(use-package solarized-theme
  :if (and (display-graphic-p)
           (eq theme-to-load 'solarized))
  :load-path "modules/appearance/themes/solarized-emacs"
  :init
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
  :config
  (load-theme 'solarized-theme t))

(use-package doom-themes
  :if (and (display-graphic-p)
           (eq theme-to-load 'doom))
  :load-path "modules/appearance/themes/emacs-doom-theme"
  :init 
  (setq doom-enable-bold t
        doom-enable-italic t
        doom-one-brighter-modeline t
        doom-one-brighter-comments t)
  :config
  (load-theme 'doom-molokai t))

(use-package dracula-theme
  :if (and (display-graphic-p)
           (eq theme-to-load 'dracula))
  :load-path "modules/appearance/themes/dracula"
  :config
  (load-theme 'dracula t))

(use-package spacemacs-light-theme
  :if (and (display-graphic-p)
           (eq theme-to-load 'spacemacs))
  :load-path "modules/appearance/themes/spacemacs"
  :init
  :config
  (load-theme 'spacemacs-light t))

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
