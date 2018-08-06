
(use-package appearance :demand t
  :init
  (setq-default truncate-lines nil
                line-spacing 2)
  
  (setq
   ;; default-font-ref    "-*-Iosevka-light-normal-normal-mono-18-*-*-*-m-0-iso10646-1"
   default-font-ref       "-*-PragmataPro Mono-normal-normal-normal-18-*-*-*-*-m-0-iso10646-1"
   
   default-font-size     20 
   theme-to-load         'sirthias
   sirthias-easy-mode    t
   sirthias-cold-mode    nil)
  
  ;; #TODO :: search fails if the buffer is opened and not at the beginning of the buffer
  (defun edit-face-at-point ()
    "Editor face of the active theme at the point."
    (interactive)
    (-if-let* ((face-name (face-at-point))
               (theme-file-buffer (find-library (concat (symbol-name theme-to-load) "-theme"))))
        (with-current-buffer theme-file-buffer
          (search-forward (symbol-name face-name)))))
  
  :config
  (reload-view)
  
  (if (not IS_WINDOWS)
      (load "fonts/pretty-pragmata"))
  
  ;; Set of custom hack of the default theme to make it a bit prettier
  (if (eq theme-to-load 'default)
      (progn 
        (set-face-attribute 'fringe nil :background nil)
        (with-eval-after-load "eshell"
          (lambda ()
            (set-face-attribute 'eshell-prompt nil :foreground "#000080"))))))

(use-package sirthias-theme :demand t
  :if (and (display-graphic-p)
           (eq theme-to-load 'sirthias))
  :load-path "modules/appearance/themes/sirthias"
  :init (setq sirthias-easy-mode t)
  :config (load-theme 'sirthias t))

(use-package powerline :demand t
  :config
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
                        (powerline-buffer-id face0 'l)
                        (powerline-raw ":%l" face0)))
             
             (rhs (list (powerline-raw "[" face0 'l)
                        (powerline-minor-modes face0)
                        (powerline-raw "]" face0 'r)
                        (powerline-major-mode face0 'r))))
        
        (concat (powerline-render lhs)
                (powerline-fill face0 (powerline-width rhs))
                (powerline-render rhs)))))))

(use-package rainbow-mode :ensure t)






