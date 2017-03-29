(add-to-list 'load-path (expand-file-name "modules/essentials/use-package" user-emacs-directory))

;; `use-package' configuration
(setq use-package-verbose               t
      use-package-enable-imenu-support  t
      use-package-check-before-init     t
      use-package-minimum-reported-time 0.01)

(require 'use-package)

(require 'package)
(setq package-enable-at-startup nil
      package--init-file-ensured t)
(add-to-list 'package-archives '(("melpa"     . "https://melpa.org/packages/")
                                 ("marmalade" . "http://marmalade-repo.org/packages/")
                                 ("gnu"       . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Vendor packages
(use-package dash     :ensure t)
(use-package s        :ensure t)
(use-package f        :ensure t)
(use-package diminish :ensure t)

(unbind-key "C-x b")

(load "functions")
(load "macros")
(load "keys")
(load "configuration")

