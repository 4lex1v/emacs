;;;;;;;;; EMACS SETTINGS ;;;;;;;;;;

;; Custom configurations folder
(defconst configs-dir   (expand-file-name "configs" user-emacs-directory))
(defconst themes-dir    (expand-file-name "themes"  user-emacs-directory))

;; General settings
(defconst custom-themes (directory-files themes-dir nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
(defconst config-files  (directory-files "~/.emacs.d/configs/modes/" t ".el"))

;; Load configs
(add-to-list 'load-path configs-dir)
(add-to-list 'load-path "~/.emacs.d/modes.el")

(require 'packages)         ;; init package system
(require 'benchmark-init)   ;; benchmark emacs loading time
(benchmark-init/activate)   ;; start benchmarking

(require 'use-package)

(use-package ui)            ;; Load UI settings
(use-package general)       ;; general emacs configs
(use-package modes)         ;; configs for installed modes & plugins, but doesn't contains key bindings

(benchmark-init/deactivate) ;; stop benchmarking
