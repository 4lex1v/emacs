(require 'package)

;;;;;;;;; EMACS SETTINGS ;;;;;;;;;;

;; emacs settings
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)

;; repos
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; load path
(add-to-list 'load-path "~/.emacs.d/ergoemacs-mode")
(add-to-list 'load-path "~/.emacs.d/elpa")

;;;;;;;; PLUGINS ;;;;;;;;;;


(require 'linum+)
(setq linum-format "%d ")
(global-linum-mode t)

(add-to-list 'load-path "~/.emacs.d/elpa/scala-mode2")
(require 'scala-mode2)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'bs)
(global-set-key (kbd "<f2>") 'bs-show)

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
