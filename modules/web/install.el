(use-package web-mode
  :load-path "packages/web"
  :defer
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.html\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.jinja\\'" . web-mode)
   ("\\.php\\'" . web-mode))
  :init
  (setq web-mode-engines-alist '(("\\.jinja\\'"  . "django"))
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight nil)

  (with-mode which-key
    (which-key-declare-prefixes-for-mode 'web-mode
      "C-c C-t" "web/tag"
      "C-c C-b" "web/block"
      "C-c C-d" "web/dom"
      "C-c C-a" "web/attribute"
      "C-c C-e" "web/element")))

(use-package restclient
  :defer
  :init
  (use-package ob-restclient :after ob))
    
