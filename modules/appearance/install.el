(load "functions")

;; #TODO(4lex1v) :: Should this check for the pragmata fonts?
(if (not IS_WINDOWS)
    (load "fonts/pretty-pragmata"))

(load "configuration")

;; TODO :: add line / column number 

;(defconst theme-to-load 'dracula)
;; (defconst theme-to-load 'spacemacs)
(defconst theme-to-load 'sirthias)
;; (defconst theme-to-load 'the_boring_one)
;; (defconst theme-to-load 'default)

(use-package dracula-theme
  :if (and (display-graphic-p)
           (eq theme-to-load 'dracula))
  :load-path "modules/appearance/themes/dracula"
  :config
  (load-theme 'dracula t))

(use-package spacemacs-light-theme
  :if (and (display-graphic-p)
           (eq theme-to-load 'spacemacs))
  :load-path "modules/appearance/themes/spacemacs"
  :init
  (setq spacemacs-theme-comment-italic t)

  :config
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold)
  (load-theme 'spacemacs-light t))

(use-package sirthias-theme
  :if (and (display-graphic-p)
           (eq theme-to-load 'sirthias))
  :load-path "modules/appearance/themes/sirthias"
  :config
  (load-theme 'sirthias t))

(use-package the_boring_one-theme
  :if (eq theme-to-load 'the_boring_one)
  :load-path "modules/appearance/themes/the_boring_one"
  :config
  (load-theme 'the_boring_one t))

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

;; Set of custom hack of the default theme to make it a bit prettier
(if (eq theme-to-load 'default)
    (progn 
      (set-face-attribute 'fringe nil :background nil)
      (with-eval-after-load "eshell"
        (lambda ()
          (set-face-attribute 'eshell-prompt nil :foreground "#000080")))))

