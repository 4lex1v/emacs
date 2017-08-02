(require 'package)

(setq package-enable-at-startup nil
      package--init-file-ensured t
      package-archives '(("melpa"     . "https://melpa.org/packages/")
                  			 ("marmalade" . "http://marmalade-repo.org/packages/")
			                   ("gnu"       . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; `use-package' configuration
(setq use-package-verbose               t
      use-package-enable-imenu-support  t
      use-package-check-before-init     t
      use-package-minimum-reported-time 0.1)

(require 'use-package)

;; Vendor packages
(use-package diminish :ensure t)

;; Use-Package Extensions
(use-package upe-hooks)

(unbind-key "C-x b")

(load "functions")
(load "macros")
(load "keys")
(load "configuration")
(load "async")

(add-hook 'after-init-hook #'(lambda () (server-start)))
