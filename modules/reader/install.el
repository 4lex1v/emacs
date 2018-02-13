
;; #NOTE :: This package requires a bunch of side things to be cooked, using the melpa's version
;; provides and install script that configures all that automatically.
(use-package pdf-tools :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (with-eval-after-load 'evil-collection
    (require 'evil-collection-pdf)
    (evil-collection-pdf-setup))
  
  (pdf-tools-install))

