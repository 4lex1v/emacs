
(defun 4lex1v:gui:frame (&rest configs)
  "Helper function for simpler frame configuration"
  (pcase-let* ((`(,active . ,inactive) (plist-get configs :transparency))
               (`(,active-cursor . ,inactive-cursor) (plist-get configs :cursor)))
    
    (setq-default cursor-type active-cursor
                  cursor-in-non-selected-windows inactive-cursor)

    (set-frame-parameter (selected-frame) 'alpha (cons active inactive))
    (add-to-list 'default-frame-alist (cons 'alpha (cons active inactive)))))

;; #TODO :: search fails if the buffer is opened and not at the beginning of the buffer
(defun edit-face-at-point ()
  "Editor face of the active theme at the point."
  (interactive)
  (-if-let* ((face-name (face-at-point))
             (theme-file-buffer (find-library (concat (symbol-name theme-to-load) "-theme"))))
      (with-current-buffer theme-file-buffer
        (search-forward (symbol-name face-name)))))

;; #NOTE(4lex1v, 08/28/18) :: Removed on Windows configuration
(defun reload-view ()
  (set-frame-font default-font-ref)
  (add-to-list 'default-frame-alist (cons 'font default-font-ref))
  (set-face-attribute 'default nil :height (* default-font-size 10)))

(setq-default
 mode-line-default-help-echo nil ; turn-off tooltips on cursor hover-over
 tab-width           2 ;; Though i'm not using tabs
 indent-tabs-mode    nil
 cursor-type        'box
 cursor-in-non-selected-windows 'bar
 frame-title-format "%f"
 linum-format       "%3d "  ;; Try dynamic?
 load-prefer-newer  t
 left-fringe-width  20
 word-wrap t
 
 ;; #TODO(4lex1v, 08/28/18) :: Is this the one that should be used of the switcher?
 truncate-lines nil
 line-spacing 2)

(setq
 default-font-name      "PragmataPro"
 default-font-size      16
 theme-to-load         'sirthias)

(toggle-truncate-lines  nil)
(show-paren-mode        t)
(delete-selection-mode  t)
(tooltip-mode          -1)
(tool-bar-mode         -1)
(menu-bar-mode         -1)
(scroll-bar-mode       -1)

(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode +1))

(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins (get-buffer-window (current-buffer) nil) 5 3)))

(use-package sirthias-theme :demand t
  :load-path "modules/appearance/themes/sirthias"
  :if (and (display-graphic-p) (eq theme-to-load 'sirthias))
  
  :init
  (setq
   sirthias-easy-mode    t
   sirthias-cold-mode    nil)
  
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

;; #NOTE(4lex1v, 08/24/17) :: Ref: `font-lock-keywords`
;; #NOTE(4lex1v) :: Wonder if i can simple use `prog-mode` to enable the highliting everywhere?
;; #TODO(4lex1v) :: Need to add some support in combination with projectile to see all entries in the project
;; #TODO(4lex1v) :: Need to update the visual repr of these things
(mapc
 (lambda (mode)
   (font-lock-add-keywords ;;`font-lock-keywords`
    mode
    '(("#\\<\\(TODO\\)\\>" 1 '(error :underline t) t)
      ("#\\<\\(NOTE\\)\\>" 1 '(warning :underline t) t))))
   '(emacs-lisp-mode scala-mode c-mode objc-mode c++-mode rust-mode))

(reload-view)
  
;; #TODO(4lex1v, 08/28/18) :: Should this also double check that we are using the pragmata font as well?
;; #TODO(4lex1v, 08/28/18) :: Double check if it really slows down the work on windows
(if (not IS_WINDOWS)
    (load "fonts/pretty-pragmata"))

;; Set of custom hack of the default theme to make it a bit prettier
(if (eq theme-to-load 'default)
    (progn 
      (set-face-attribute 'fringe nil :background nil)
      (with-eval-after-load "eshell"
        (lambda ()
          (set-face-attribute 'eshell-prompt nil :foreground "#000080")))))






