(setq-default
 mode-line-default-help-echo nil ; turn-off tooltips on cursor hover-over
 tab-width           2 ;; Though i'm not using tabs
 indent-tabs-mode    nil
 cursor-type        'box
 cursor-in-non-selected-windows 'bar
 frame-title-format " %@%b% -"
 linum-format       "%3d "  ;; Try dynamic?
 load-prefer-newer  t
 left-fringe-width  20)

(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode +1))

(add-to-list 'load-path (expand-file-name "platform/appearance/themes/zenburn-emacs" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "platform/appearance/themes/dracula" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "platform/appearance/themes/sirthias" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "platform/appearance/themes/solarized-emacs" user-emacs-directory))
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

(add-to-list 'load-path (expand-file-name "platform/appearance/themes/monokai-emacs" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "platform/appearance/themes/emacs-doom-theme" user-emacs-directory))

(setq doom-enable-bold t
      doom-enable-italic t
      doom-one-brighter-modeline t
      doom-one-brighter-comments t)

(use-package doom-nlinum)

(add-to-list 'load-path (expand-file-name "platform/appearance/themes/spacemacs" user-emacs-directory))

(require 'spacemacs-light-theme)
(load-theme 'spacemacs-light t t)

(defcustom default-font-name "PragmataPro"
  "Font name used by default"
  :group 'string 
  :options '("PragmataPro"
             "Hack"
             "Ayuthaya"))

(defcustom default-font-size 18
  "Default Font size used across all frames")

(defun change-font-size (value)
  (interactive
   (list
    (read-number (format "Font current font size [%d] to: " font-size))))
  (setq font-size value)
  (4lex1v:gui:font font-name :size value))

(load "fonts/pretty-pragmata")
(load "fonts")

(let ((frame-font (format "%s-%d"
                          default-font-name
                          default-font-size)))
  (set-frame-font frame-font)
  (add-to-list 'default-frame-alist (cons 'font frame-font)))

(4lex1v:gui:frame :size         'maximized
                 :transparency '(100 . 100)
                 :cursor       '(box . bar))

(use-package spaceline
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        powerline-default-separator 'bar)
  (use-package spaceline-config)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init
  (setq beacon-color (face-attribute 'spaceline-evil-normal :background nil t))
  :config
  (beacon-mode +1))

(use-package hl-line
  :init
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)
  (add-hook 'prog-mode-hook 'hl-line-mode)
  :config
  (global-hl-line-mode))
