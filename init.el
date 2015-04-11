;; Folder with main configuration files for Emacs
(defconst configs-dir   (expand-file-name "configs" user-emacs-directory))

;; Contains configuration files for all used packages
(defconst modes-dir     (expand-file-name "modes"   user-emacs-directory))
(defconst mode-files    (directory-files modes-dir t ".el"))

;; Add both folder to the load path
(add-to-list 'load-path configs-dir)
(add-to-list 'load-path modes-dir)

(require 'packages)         ;; init package system
(require 'benchmark-init)   ;; benchmark emacs loading time
(benchmark-init/activate)   ;; start benchmarking

;; Macro that's used to do the configuration
(require 'use-package)

;; Emacs configuration
(use-package ui)
(use-package general)


(mapc 'load mode-files)

(benchmark-init/deactivate) ;; stop benchmarking
