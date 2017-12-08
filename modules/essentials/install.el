(require 'package)

(setq package-enable-at-startup nil
      package--init-file-ensured t
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; `use-package' configuration
(setq use-package-verbose               t
      use-package-enable-imenu-support  t
      use-package-check-before-init     t
      use-package-minimum-reported-time 0.1)

(require 'use-package)

;; Vendor packages
(use-package diminish :ensure t)
(use-package async :ensure t :pin "melpa")

;; Use-Package Extensions
(use-package upe-hooks)

(use-package general
  :init
  (setq general-default-states  'normal
        general-default-prefix  "<SPC>"))

(unbind-key "C-x b")

(load "functions")
(load "macros")
(load "configuration")


(use-package mode-local)

(add-hook 'after-init-hook #'(lambda () (server-start)))
