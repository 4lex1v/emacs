;; TODO :: remove  
(add-to-list 'load-path (expand-file-name "."))

(load "fonts/pretty-pragmata")

;;(use-package zenburn-theme    :load-path "platform/appearance/themes/zenburn-emacs")
;;(use-package dracula-theme    :load-path "platform/appearance/themes/dracula")
;;(use-package sirthias-theme   :load-path "platform/appearance/themes/sirthias")

(use-package spacemacs-common 
  :load-path "platform/appearance/themes/spacemacs")

(use-package solarized-theme
  :load-path "platform/appearance/themes/solarized-emacs"
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
        x-underline-at-descent-line           t))

(load "fonts")

(setq font-name "PragmataPro")
;;(setq font-name "Ayuthaya")
;;(setq font-name "Hack")

(setq font-size 16)
(change-font-size 16)

(4lex1v:gui:frame :size         'maximized
                  :transparency '(100 . 100)
                  :theme        'spacemacs-light
                  :cursor       '(box . bar))
