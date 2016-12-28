(use-package spacemacs-common :load-path "themes/spacemacs")
;(use-package zenburn-theme    :load-path "themes/zenburn-emacs")
(use-package dracula-theme    :load-path "themes/dracula")

(use-package solarized-theme
  :load-path "themes/solarized-emacs"
  :init (setq solarized-contrast                   'high
              solarized-visibility                 'high
              solarized-termcolors                  256
              solarized-distinct-fringe-background  t
              solarized-use-variable-pitch          nil
              solarized-use-less-bold               nil
              solarized-use-more-italic             nil
              solarized-high-contrast-mode-line     t
              solarized-emphasize-indicators        t
              x-underline-at-descent-line           t))

(provide 'themes)
