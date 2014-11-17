;; UI Configs

(defun 4lex1v/load-custom-themes ()
	(defun custom-load-theme (name) 
		(add-to-list 
		 'custom-theme-load-path
		 (concat "~/.emacs.d/themes/" name)))
  (custom-load-theme "monokai"))

(defun 4lex1v/configure-window ()
  (tool-bar-mode   -1)
  (scroll-bar-mode -1))

;; Maximize emacs window
;; change 'maximized to 'fullboth to make fullscreen
(defun 4lex1v/set-fullscreen ()
  (set-frame-parameter nil 'fullscreen 'maximized))

(defun 4lex1v/monaco-font ()
  (set-frame-font "Monaco for Powerline-18"))

(defun 4lex1v/config-paren-mode ()
  (show-paren-mode t)
  (setq show-paren-delay 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;
(4lex1v/load-custom-themes)
(4lex1v/configure-window)
(4lex1v/set-fullscreen)
(4lex1v/monaco-font)
(4lex1v/config-paren-mode)

(load-theme 'dichromacy t)

(setq-default tab-width 2)
(global-linum-mode t)
(global-hl-line-mode 1)
(set-default 'cursor-type 'bar)

(provide 'ui)
