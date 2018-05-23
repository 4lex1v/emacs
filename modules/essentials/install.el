(require 'package)

(setq package-enable-at-startup nil
      package--init-file-ensured t
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(eval-when-compile 
  ;; `use-package' configuration
  (setq use-package-verbose               t
        use-package-always-defer          t
        use-package-enable-imenu-support  t
        use-package-check-before-init     t
        use-package-minimum-reported-time 0.1)
  
  ;; Only when the config is stable
  (setq use-package-expand-minimally t)

  (require 'use-package-core)
  (require 'use-package)

  (use-package general :demand t
    :init
    (setq general-default-states  'normal
          general-default-prefix  "<SPC>")
    :config
    (general-evil-setup t))

  ;; Use-Package Extensions
  (use-package upe-hooks :demand t))

;; Vendor packages
(use-package diminish :ensure t)
(use-package async :ensure t :pin "melpa")

(unbind-key "C-x b")

(load "functions")
(load "macros")
(load "configuration")

(use-package mode-local :demand t)

(use-package helpful :ensure t :after elisp-refs)

(add-hook 'after-init-hook #'(lambda () (server-start)))
