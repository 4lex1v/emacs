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
  (setq spacemacs-theme-comment-italic t)

  :config
  (load-theme 'spacemacs-light t))

(use-package spaceline-config)
(use-package spaceline
  :after spaceline-config
  
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        powerline-image-apple-rgb t
        powerline-default-separator 'arrow)

  (spaceline-define-segment projectile-mode-segment
    "Pretty projectile segment rendering"
    (if (and (fboundp 'projectile-project-p)
             (projectile-project-p))
        (let ((project-name (projectile-project-name))
              (branch-name
               (if (fboundp 'magit-get-current-branch)
                   (magit-get-current-branch))))
          (propertize
           (if (or (eq branch-name nil)
                   (string-empty-p branch-name))
               project-name
             (format "[git: %s] | %s"
                     branch-name
                     project-name))))))
  
  (defun custom-spaceline-theme ()
    (spaceline-install
      ;; Left side
      `((hud :priority 0)
        (evil-state :face highlight-face :priority 0)
        projectile-mode-segment
        buffer-id
        ;(buffer-id :face spaceline-modified) 
        buffer-position) 
      
      ;; Right side
      '((minor-modes :when active)
        major-mode))
    
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))
  
  :config
  (custom-spaceline-theme))

(use-package beacon
  :ensure t
  :if (display-graphic-p)
  :diminish beacon-mode
  
  :init
  (setq beacon-color (face-attribute 'spaceline-evil-normal :background nil t))
  
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'term-mode)
  (beacon-mode +1))

(use-package hl-line
  :init
  (setq global-hl-line-sticky-flag nil)
  :config
  (global-hl-line-mode))

;; TODO :: need to check this one out
(use-package hide-mode-line)
