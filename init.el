;;;;;;;;; EMACS SETTINGS ;;;;;;;;;;

;; Load configuration files  
(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))

(require 'packages)  ;; init package system
(require 'general)   ;; general emacs configs
(require 'ui)        ;; load custom UI configs
(require 'modes)     ;; configs for installed modes & plugins, but doesn't contains key bindings
(require 'functions) ;; loads all custom defined functions 
(require 'keys)      ;; general keybindings & different mode specific
