
(use-package appearance :demand t
  :init
  (setq-default truncate-lines nil
                line-spacing 2)
  
  (setq default-font-name  "Iosevka"
        default-font-size  16
        theme-to-load      (if (not IS_WINDOWS) 'sirthias 'spacemacs)
        sirthias-easy-mode nil)

  ;; #TODO :: search fails if the buffer is opened and not at the beginning of the buffer
  (defun edit-face-at-point ()
    "Editor face of the active theme at the point."
    (interactive)
    (-if-let* ((face-name (face-at-point))
               (theme-file-buffer (find-library (concat (symbol-name theme-to-load) "-theme"))))
        (with-current-buffer theme-file-buffer
          (search-forward (symbol-name face-name)))))
  
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

(use-package the_boring_one-theme :demand t
  :if (eq theme-to-load 'the_boring_one)
  :load-path "modules/appearance/themes/the_boring_one"
  :config
  (load-theme 'the_boring_one t))

(use-package powerline :if IS_MAC :demand t
  :init

  ;; #TODO :: Add some colouring to the branch name?
  ;;            - Red - dirty branch
  ;;            - Green - no changes?
  (defpowerline project-segment 
    (if (and (fboundp 'projectile-project-p)
             (projectile-project-p))
        (let ((project-name (projectile-project-name))
              (branch-name
               (if (fboundp 'magit-get-current-branch)
                   (magit-get-current-branch))))
          (propertize
           (if (or (eq branch-name nil)
                   (string-empty-p branch-name))
               (format "[%s]" project-name)
             (format "[%s @ %s]"
                     project-name
                     branch-name))))))
  
  :config
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (face0 (if active 'powerline-active0 'powerline-inactive0))
             (separator-left (intern (format "powerline-%s-%s"
                                             (powerline-current-separator)
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              (powerline-current-separator)
                                              (cdr powerline-default-separator-dir))))
             (lhs (list (powerline-raw evil-mode-line-tag face0)
                        (project-segment face0)
                        (powerline-buffer-id face0)))
             (rhs (list (powerline-raw "[" face0 'l)
                        (powerline-minor-modes face0)
                        (powerline-raw "]" face0 'r)
                        (powerline-major-mode face0 'r))))
        (concat (powerline-render lhs)
                (powerline-fill face0 (powerline-width rhs))
                (powerline-render rhs)))))))

(use-package beacon
  :ensure t
  :if (display-graphic-p)
  :diminish beacon-mode
  
  ;; :init
  ;; (setq beacon-color
  ;;       (face-attribute
  ;;        (if IS_MAC 'spaceline-highlight-face 'default)
  ;;        :background nil t))
  
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'term-mode)
  (beacon-mode +1))

(use-package rainbow-mode :ensure t)

(use-package hl-line
  :init
  (setq global-hl-line-sticky-flag nil)
  :config
  (global-hl-line-mode))





