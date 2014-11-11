;;;;;;;;; EMACS SETTINGS ;;;;;;;;;;

(defun 4lex1v/load-custom-configs () 
	(add-to-list 'load-path "~/.emacs.d/configs"))

(defun 4lex1v/configure-package-system ()
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/")))
  
;;;;;;;;;;;;;
(4lex1v/load-custom-configs)
(4lex1v/configure-package-system)

(load "functions.el")		;; loads all custom defined functions 
(load "general.el")			;; general emacs configs
(load "ui.el")					;; load custom UI configs
(load "modes.el")				;; configs for installed modes & plugins, but doesn't contains key bindings
(load "keys.el")				;; general keybindings & different mode specific
