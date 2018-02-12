(use-package web-mode :ensure t
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
        web-mode-enable-current-column-highlight nil))

;; Can't get it from the repos...
(use-package tide :ensure t :disabled t
  :init
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'tide)
    (add-to-list 'evil-collection-mode-list 'typescript-mode)))

(use-package restclient :defer t
  :commands restclient-mode
  :config
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(restclient . t))))

(use-package ob-restclient :ensure t
  :after (:both org restclient)
  :commands org-babel-execute:restclient)
    
