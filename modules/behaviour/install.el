
;; Goes before others to correctly load which-key-declare-prefixes
(use-package which-key :demand t
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.2
        which-key-sort-order 'which-key-prefix-then-key-order-reverse
        which-key-show-operator-state-maps t ;; Hack to make this work with Evil
        which-key-prefix-prefix ""
        which-key-side-window-max-width 0.5

        which-key-popup-type           'side-window 
        which-key-side-window-location 'bottom) 
  
  :config
  (which-key-mode)
  
  ;; #NOTE :: For some reason it doesn't work as a `use-pacakge' directive
  (general-define-key
   :keymaps 'which-key-C-h-map
   :prefix ""
   :states nil
    "l" 'which-key-show-next-page-cycle
    "j" 'which-key-show-previous-page-cycle)

  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'while-key)))

;; #NOTE(4lex1v, 08/24/17) :: Some movement keybinds are defined in Editor/Smartparens
(use-package evil :demand t
  :after general ;; To enable evil-leader in initial buffers
  
  :general
  (:prefix   ""
   :keymaps 'evil-motion-state-map
   :states   nil
   
   "j"   'evil-next-visual-line
   "k"   'evil-previous-visual-line)

  (:prefix ""
   
   "$"   'evil-end-of-visual-line
   "C-j" 'evil-forward-paragraph
   "C-k" 'evil-backward-paragraph
   "g,"  'evil-jump-backward
   "g."  'find-function-at-point
   "C-q" '4lex1v/close-buffer

   ;; Navigation keys
   "C-S-o" #'evil-jump-forward
   "C-M-j" #'next-error
   "C-M-k" #'previous-error)

  (:states 'normal
   
   "f"   '(:ignore t :which-key "Files")
   "fi"  'init.el
   "fe"  'eshell
   
   "e"   '(:ignore t :which-key "Emacs")
   "eq"  'save-buffers-kill-emacs
   "er"  'revert-buffer)
  
  :init
  (setq evil-default-cursor             t
        evil-ex-substitute-global       t
        evil-ex-search-vim-style-regexp t
        evil-want-C-u-scroll            t
        evil-ex-interactive-search-highlight 'selected-window
        evil-want-integration           nil)
  
  :config
  (evil-set-initial-state 'prog-mode   'normal)
  (evil-set-initial-state 'comint-mode 'normal)
  
  (evil-set-initial-state 'package-menu-mode 'motion)

  ;; Use `§' key to switch between emacs and normal state
  (evil-global-set-key 'normal "§" #'evil-emacs-state)
  (evil-global-set-key 'emacs  "§" #'evil-exit-emacs-state)

  (evil-ex-define-cmd "e[val]" #'eval-buffer)
  (evil-ex-define-cmd "we" #'(lambda () (interactive) (save-buffer) (eval-buffer)))
  
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode))

(use-package evil-collection :demand t
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t
        evil-collection-mode-list `(bookmark
                                    (buff-menu "buff-menu")
                                    calendar
                                    comint
                                    compile
                                    debbugs
                                    debug
                                    diff-mode
                                    dired
                                    doc-view
                                    edebug
                                    eval-sexp-fu
                                    etags-select
                                    flycheck
                                    help
                                    ibuffer
                                    info
                                    log-view
                                    man
                                    simple
                                    ,@(when evil-collection-setup-minibuffer '(minibuffer))
                                    (occur ,(if (<= emacs-major-version 25) "replace" 'replace))
                                    (package-menu package)
                                    rtags
                                    (term term ansi-term multi-term)))
  :config
  (add-hook 'after-init-hook
            (lambda () (evil-collection-init))))

(use-package hydra :demand t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")
    ("r" (text-scale-set 0) "reset")
    ("0" (text-scale-set 0) :bind nil :exit t)
    ("1" (text-scale-set 0) nil :bind nil :exit t))

  (defhydra hydra-error (evil-normal-state-map "C-e")
    "error"
    ("j" next-error "next")
    ("k" previous-error "previous")))

(use-package projectile
  :commands projectile-project-root

  :general
  (:prefix "" "M-4" 'projectile-switch-project)      
  
  ;;  Projectile-only bindings
  ("p" '(:ignore t :which-key "Projectile")
   "pk" 'projectile-kill-buffers
   "pr" 'projectile-replace
   "pi" 'projectile-invalidate-cache)
  
  :init
  (setq projectile-enable-caching       nil
        projectile-completion-system   'helm
        projectile-require-project-root t
        projectile-use-git-grep         nil
        projectile-mode-line            '(:eval (format " {%s}" (projectile-project-name))))

  :config
  (projectile-global-mode))

(use-package ranger
  :general
  ("fr" 'ranger)
  
  :init
  (setq ranger-override-dired 'ranger
        ranger-show-literal    nil ;; Turn on highlighting in ranger mode
        ranger-cleanup-eagerly t
        ranger-show-dotfiles   t
        ranger-ignored-extensions '())

  (bind-key "M-2" #'(lambda (&optional arg)
                      (interactive "P")
                      (if (not arg)
                          (ranger arg)
                        (ranger
                         (or (projectile-project-root)
                             default-directory)))))

  (cl-defun drill-folder-down (&optional (entry-point (dired-get-filename nil t)))
    (interactive)
    (if (file-directory-p entry-point)
        (let* ((subentries (f-entries entry-point)))
          ;; If `entry-point' contains a single folder navigate into it
          (if (and (eq (length subentries) 1)
                   (file-directory-p (car subentries)))
              (drill-folder-down (car subentries))
            (ranger-find-file entry-point)))))

  (defun drill-folder-up (&optional initial)
    (interactive)
    (let* ((entry  (or initial default-directory))
           (parent-folder (f-parent entry))
           (parent-content (f-entries parent-folder))
           (nr-of-entryies (length parent-content)))
      (if (eq nr-of-entryies 1)
          (drill-folder-up parent-folder)
        (ranger-find-file parent-folder))))           
  
  :config
  (ranger-override-dired-mode t)
  
  (add-to-list 'ranger-excluded-extensions "meta")
  (add-to-list 'ranger-excluded-extensions "cs.meta")

  (bind-key "l" #'drill-folder-down ranger-mode-map)
  (bind-key "h" #'drill-folder-up   ranger-mode-map))

(use-package avy :ensure t
  :bind
  (("C-c SPC" . avy-goto-char)
   ("C-c j c" . avy-goto-char)
   ("C-c j w" . avy-goto-word-1)
   ("C-c j l" . avy-goto-line))
  
  :init
  (general-define-key "j" #'avy-goto-char))

(use-package ace-window :ensure t
  :bind
  (("C-'"  . ace-window))
  :general
  ("wj" 'ace-window))

(use-package helm :demand t
  :general
  (:prefix ""
   :states '(normal)
   
   "ga" 'helm-apropos)

  (:prefix ""
   :states nil
   
   "C-c h"   'helm-command-prefix
   "M-y"     'helm-show-kill-ring
   "C-x b"   'helm-mini
   "C-x C-f" 'helm-find-files         
   "M-x"     'helm-M-x
   "M-:"     'helm-eval-expression-with-eldoc
   "M-i"     'helm-occur

   ;; Number keys
   "M-3"      'helm-mini
   "M-6"      'helm-bookmarks)

  (:prefix ""
   :keymaps 'helm-map
   :states nil
   
   "<tab>" 'helm-execute-persistent-action
   "C-i"   'helm-execute-persistent-action
   "C-z"   'helm-select-action)

  (:prefix ""
   :keymaps 'helm-find-files-map
   :states nil
   
   "C-h"   'helm-find-files-up-one-level)
  
  :init
  (setq helm-idle-delay                        0.0
        helm-input-idle-delay                  0.01
        helm-quick-update                      t
        helm-split-window-in-side-p            t
        helm-buffers-fuzzy-matching            t
        helm-ff-fuzzy-matching                 t
        helm-move-to-line-cycle-in-source      t
        helm-scroll-amount                     8
        helm-ff-search-library-in-sexp         t
        helm-ff-file-name-history-use-recentf  t
        helm-follow-mode-persistent            t

        helm-display-function                 'helm-display-buffer-in-own-frame
        helm-display-buffer-reuse-frame        t
        helm-use-undecorated-frame-option      t)
  
  (use-package helm-config :demand t)
  
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'helm))
  
  :config 
  (use-package helm-mode :demand t)
  
  (helm-autoresize-mode)
                                        ;(spaceline-helm-mode)
  (which-key-declare-prefixes "<SPC> f" "Files")

  (func init.el (find-file (concat user-emacs-directory "/" "init.el")))
  (general-define-key "ff" 'helm-find-files)
  
  (substitute-key-definition 'find-tag 'helm-etags-select global-map))

(use-package foundation-helm
  :after helm
  :general
  ("fm" '(fnd:helm-list-modules :which-key "Modules")))

(use-package helm-descbinds :ensure t
  :commands helm-descbinds
  
  :general
  (:prefix ""
   :keymaps 'helm-command-map
   :states nil
   
   "b" 'helm-descbinds)
  
  ("eb" '(helm-descbinds :which-key "Bindings"))
  
  :init
  (setq helm-descbinds-window-style 'split-window)
  
  :config
  (unbind-key "\C-h b")
  (unbind-key "<f1> b")
  (unbind-key "<help> b")
  (unbind-key "C-c h b")
  (unbind-key "C-x c b")

  (fset 'describe-bindings 'helm-descbinds))

;; #NOTE :: This package doesn't rely on Projectile cause my workflow starts with helm-projectile-switch-project
;; So this package bootstrap the projectile loading
(use-package helm-projectile :ensure t
  :after helm
  
  :general
  ("pp"  'helm-projectile-switch-project)
  
  (:prefix ""
   "C-M-3" 'helm-projectile-switch-to-buffer
   "M-1"   'helm-projectile-find-file)
  
  :config (helm-projectile-on))

(use-package helm-ag :ensure t
  :after helm-projectile
  :general
  ("ps"  '(:ignore t :which-key "Search [Ag]")
   "pss" 'helm-projectile-ag
   "psr" 'helm-ag-project-root
   "psa" 'helm-do-ag)

  :init
  (setq helm-ag-insert-at-point 'symbol
        helm-ag-fuzzy-match     t)
  
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'ag)))

(use-package helm-dash :ensure t
  :commands (helm-dash helm-dash-at-point))

(use-package helm-gtags :ensure t
  :after helm
  
  :general
  (:prefix ""
   "C-]"   'helm-gtags-dwim
   "C-M-]" 'helm-gtags-select)
  
  :init
  (setq helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-ignore-case t))

;; Need to organize this to avoid disambiguity and not to forget
(delete-selection-mode t)
(global-auto-revert-mode t)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(fset 'yes-or-no-p   'y-or-n-p)
