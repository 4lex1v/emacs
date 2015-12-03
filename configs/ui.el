(defconst themes-dir    (expand-file-name "themes"  user-emacs-directory))
(defconst custom-themes (directory-files themes-dir nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))

;; Loads custom theme by name
(defun 4lex1v/load-theme (theme-name)
  (add-to-list 'custom-theme-load-path (concat themes-dir "/" theme-name "/")))

(defun 4lex1v/configure-theme (current-theme)
  (if (equal (window-system) 'mac)
      (load-theme current-theme t)))

(defun 4lex1v/configure-font (name size)
  (let ((frame-font (format "%s-%d" name size)))
    
    ;; Set font for current frame 
    (set-frame-font frame-font) 

    ;; Set default font for new frames
    (add-to-list 'default-frame-alist   
                 (cons 'font (format "%s-%d" name size)))))

(defun 4lex1v/configure-frame-size (size-param)
  
  ;; Set frame size for current frame
  (set-frame-parameter nil 'fullscreen size-param)
  
  ;; Set Default frame size param
  (add-to-list 'default-frame-alist
               (cons 'fullscreen size-param)))

;; BOOTSTRAP UI CONFIG
(defun 4lex1v/prepare-ui-configuration ()
  (mapc '4lex1v/load-theme custom-themes))

(defun 4lex1v/transparent-ui (v1 v2)
	(set-frame-parameter (selected-frame) 'alpha (cons v1 v2))
	(add-to-list 'default-frame-alist (cons 'alpha (cons v1 v2))))

;; UI CONFIGURATION
(4lex1v/prepare-ui-configuration)
(tool-bar-mode          -1)
(scroll-bar-mode        -1)

;; Highlight parethesis
(setq show-paren-delay  0.0)
(show-paren-mode        t)

(global-hl-line-mode    t)
(global-linum-mode      nil)
(column-number-mode     t)

(setq-default           tab-width 2
                        cursor-type 'bar
                        frame-title-format " %@%b% -"
                        linum-format "%3i ")

(4lex1v/configure-font       "Monaco for Powerline" 20)

(4lex1v/configure-theme      'sirthias)

(setq solarized-contrast     'high
      solarized-visibility   'high
      solarized-termcolors   256) 

(4lex1v/transparent-ui 95 95)
(set-face-attribute 'mode-line nil  :height 180)

(if (not (display-graphic-p))
    (menu-bar-mode -1))

;;(hlinum-activate)


(provide 'ui)
