
(use-package appearance :demand t
  :init
  (setq-default truncate-lines nil
                line-spacing 2)
  
  (setq default-font-name  "PragmataPro"
        default-font-size  (if (string= default-font-name "PragmataPro") 18 16)
        theme-to-load      (if (not IS_WINDOWS) 'sirthias 'the_boring_one)
        sirthias-easy-mode nil)
  :config
  (reload-view))

(use-package spacemacs-light-theme :demand t
  :if (and (display-graphic-p)
           (eq theme-to-load 'spacemacs))
  :load-path "modules/appearance/themes/spacemacs"
  :init
  (setq spacemacs-theme-comment-italic t)

  :config
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold)
  (load-theme 'spacemacs-light t))

(use-package sirthias-theme :demand t
  :if (and (display-graphic-p)
           (eq theme-to-load 'sirthias))
  :load-path "modules/appearance/themes/sirthias"
  :config
  (load-theme 'sirthias t))

(use-package polygon-theme :demand t
  :if (and (display-graphic-p)
           (eq theme-to-load 'polygon))
  :load-path "modules/appearance/themes/polygon"
  :config
  (load-theme 'polygon t))

(use-package the_boring_one-theme :demand t
  :if (eq theme-to-load 'the_boring_one)
  :load-path "modules/appearance/themes/the_boring_one"
  :config
  (load-theme 'the_boring_one t))

(use-package spaceline-config :if IS_MAC :demand t)
(use-package spaceline :disabled t :if IS_MAC
  :after spaceline-config
  
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        powerline-image-apple-rgb (if IS_MAC t)
        powerline-default-separator 'bar)
  
  ;; TODO :: the problem with this functiona is that
  ;; it's rather often that magit is not loaded
  (spaceline-define-segment projectile-mode-segment
    "Pretty projectile segment rendering"
    (if (and (fboundp 'projectile-project-p)
             (projectile-project-p))
        (let ((project-name (projectile-project-name))
              (branch-name
               (if (fboundp 'magit-get-current-branch)
                   (magit-get-current-branch))))
          (propertize
           (if (or (eq branch-name nil)
                   (string-empty-p branch-name))
               project-name
             (format "[git: %s] | %s"
                     branch-name
                     project-name))))))
  
  (spaceline-define-segment buffer-positioning
    "Basic buffer positioning info"
    "%l:%c %p")
  
  (defun custom-spaceline-theme ()
    (spaceline-install
      ;; Left side
      `((hud :face highlight :priority 0)
        (evil-state :face mode-line-highlight :priority 0)
        projectile-mode-segment
        buffer-id
        ;(buffer-id :face spaceline-modified) 
        buffer-positioning) 
      
      ;; Right side
      '((minor-modes :when active)
        major-mode))
    
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))
  
  :config
  
  ;; For sirthias theme need to change colors
  (if (eq theme-to-load 'sirthias)
      (progn
         ;(set-face-attribute 'font-lock-constant-face nil :weight 'bold))
        ))
  
  (custom-spaceline-theme))

(use-package beacon
  :ensure t
  :if (display-graphic-p)
  :diminish beacon-mode
  
  :init
  (setq beacon-color
        (face-attribute
         (if IS_MAC 'spaceline-highlight-face 'default)
         :background nil t))
  
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'term-mode)
  (beacon-mode +1))

(use-package rainbow-mode :ensure t)

(use-package hl-line
  :init
  (setq global-hl-line-sticky-flag nil)
  :config
  (global-hl-line-mode))

;; TODO :: need to check this one out
(use-package hide-mode-line)



