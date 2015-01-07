;; UI Configs

;; THEMES CONFIGURATION
;; Defines a folder for all custom themes
(defconst custom-themes-folder (concat user-emacs-directory "themes/"))
(defconst custom-themes (directory-files custom-themes-folder nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))

;; Loads custom theme by name
(defun 4lex1v/load-theme (theme-name)
  (add-to-list 'custom-theme-load-path (concat custom-themes-folder theme-name "/")))

(defun 4lex1v/configure-theme (current-theme)
  (load-theme current-theme t))

(defun 4lex1v/configure-font (name size)
  (let ((frame-font (format "%s-%d" name size)))
    
    ;; Set font for current frame 
    (set-frame-font frame-font) 

    ;; Set default font for new frames
    (add-to-list 'default-frame-alist
                 (cons 'font (format "%s-%d" name size)))))
  
;; BOOTSTRAP UI CONFIG
(defun 4lex1v/prepare-ui-configuration ()
  (mapc '4lex1v/load-theme custom-themes))

;; UI CONFIGURATION
(4lex1v/prepare-ui-configuration)
(tool-bar-mode          -1)
(scroll-bar-mode        -1)
(show-paren-mode        t)
(set-frame-parameter    nil 'fullscreen 'maximized)
(setq show-paren-delay  0.0)
(global-hl-line-mode    t)
(global-linum-mode      t)
(setq-default           tab-width 2
                        cursor-type 'bar)
(setq linum-format "%3d ") 
(hlinum-activate)
(4lex1v/configure-font  "Monaco for Powerline" 14)
(4lex1v/configure-theme 'sirthias)

(provide 'ui)
