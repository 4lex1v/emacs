
(use-package appearance
  :init
  (setq default-font-name "PragmataPro"
        theme-to-load     'sirthias)
  :config
  (reload-view))

(use-package spaceline-config :if IS_MAC)
(use-package spaceline :if IS_MAC
  :disabled t
  :after spaceline-config
  
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        powerline-image-apple-rgb t
        powerline-default-separator 'arrow)
  
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



