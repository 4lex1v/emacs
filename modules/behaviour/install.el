
(defun 4lex1v/multi-window-p ()
  (> (count-windows) 1))

(defun 4lex1v/close-buffer (&optional arg)
  "Close active buffer if a single window environment or close buffer with corresponding window
in multi-window environment. In order to leave the window opened provided an optional arg `leave-window'"
  (interactive "P")
  (kill-buffer (current-buffer))
  (if (and (not (equal arg 'nil))
           (4lex1v/multi-window-p))
      (delete-window)))

(defun 4lex1v:w/close-other-window ()
  "In a multi window environment close other (i.e not active) window. If there're more
then two windows around, provide an index number which window to close"
  (interactive)
  (if (4lex1v/multi-window-p)
      (progn
        (other-window 1)
        (kill-buffer (current-buffer))
        (delete-window))))     

;; #NOTE(4lex1v, 08/24/17) :: Default to an empty string that should be introduced manually
(setq comment-note-comment-prefix "")

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
   :prefix   nil
   :states   nil
    
    "l" 'which-key-show-next-page-cycle
    "j" 'which-key-show-previous-page-cycle)

  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'while-key)))

(use-package hydra :demand t
  :general
  (:prefix nil
   
   "<f2>" 'hydra-zoom/body)

  (:prefix nil
   :states 'normal

   "C-e"  'hydra-error/body)
  
  :config
  (defhydra hydra-zoom (:color pink :hint nil)
    "
^Zoom^
----------
_g_: In
_l_: Out
_r_: Reset
----------
"
    ("g" text-scale-increase)
    ("l" text-scale-decrease)
    ("r" (text-scale-set 0))
    ("q" nil "quit"))

  (defhydra hydra-error (:color pink :hint nil)
    "
^Errors^       ^Level (cur: %`compilation-skip-threshold)^
------------------------------
_j_: Next      _0_: All
_k_: Previous  _1_: Warnings
_h_: First     _2_: Errors
_l_: Last            
------------------------------
"
    ("j" next-error)
    ("k" previous-error)
    ("h" first-error)
    ("l" (condition-case err
             (while t
               (next-error))
           (user-error nil)))
    
    ;; Messages level support
    ("0" (compilation-set-skip-threshold 0))
    ("1" (compilation-set-skip-threshold 1))
    ("2" (compilation-set-skip-threshold 2))
    
    ("q" nil "quit")))

;; #TODO(4lex1v, 07/20/18) :: Would demanding projectile at the startup slowdown the process?
(use-package projectile :demand t
  ;; #NOTE :: this is configured using Spaceline, no need to duplicated in minor mode section
  :diminish projectile-mode
  
  :commands projectile-project-root

  :general
  (:prefix nil
   
   "M-4" 'projectile-switch-project
   "M-!" 'projectile-run-shell-command-in-root)      
  
  ;;  Projectile-only bindings
  ("p" '(:ignore t :which-key "Projectile")
   "pk" 'projectile-kill-buffers
   "pr" 'projectile-replace
   "pi" 'projectile-invalidate-cache
   "pe" 'projectile-run-eshell
   "p&" 'projectile-run-async-shell-command-in-root
   "pS" 'projectile-save-project-buffers)
  
  :init
  (setq projectile-enable-caching       t
        projectile-completion-system   'helm
        projectile-require-project-root t
        projectile-use-git-grep         nil
        projectile-mode-line            '(:eval (format " {%s}" (projectile-project-name))))

  :config
  (projectile-global-mode))

(use-package helm :demand t
  :general
  (:prefix nil
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
  
  (:prefix  nil
   :states '(normal)
   
   "ga" 'helm-apropos)

  (:prefix   nil
   :keymaps 'helm-map
   :states   nil
   
   "<tab>" 'helm-execute-persistent-action
   "C-i"   'helm-execute-persistent-action
   "C-z"   'helm-select-action
   "C-o"   'helm-next-source
   "C-j"   'helm-next-line
   "C-k"   'helm-previous-line)

  ;; (:prefix   nil
  ;;  :keymaps 'helm-find-files-map
  ;;  :states   nil
  
  ;;  "C-h"   'helm-find-files-up-one-level)
  
  (:prefix   nil
   :keymaps 'comint-mode-map
   :states  '(normal insert)

   "M-r" 'helm-comint-input-ring)
  
  :init
  (setq helm-idle-delay                        0.0
        helm-input-idle-delay                  0.01
        helm-quick-update                      t
        helm-split-window-inside-p             t
        helm-buffers-fuzzy-matching            t
        helm-ff-fuzzy-matching                 t
        helm-move-to-line-cycle-in-source      t
        helm-scroll-amount                     8
        helm-ff-search-library-in-sexp         t
        helm-ff-file-name-history-use-recentf  t
        helm-follow-mode-persistent            t)
  
  (use-package helm-config :demand t)
  
  :config 
  (use-package helm-mode :demand t)
  
  (helm-autoresize-mode)
                                        ;(spaceline-helm-mode)
  (which-key-declare-prefixes "<SPC> f" "Files")

  (func init.el (find-file (concat user-emacs-directory "/" "init.el")))
  (general-define-key "ff" 'helm-find-files)
  
  (substitute-key-definition 'find-tag 'helm-etags-select global-map)

  (use-package foundation-helm
    :after helm
    :general
    ("fm" '(fnd:helm-list-modules :which-key "Modules")))

  ;; #TODO(4lex1v, 07/20/18) :: Marked for removal
  (use-package helm-descbinds :ensure t :disabled t
    :commands helm-descbinds
    
    :general
    (:prefix   nil
     :keymaps 'helm-command-map
     :states   nil
     
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
    :general
    ("pp"  'helm-projectile-switch-project)
    
    (:prefix ""
     "C-M-3" 'helm-projectile-switch-to-buffer
     "M-1"   'helm-projectile-find-file)
    
    :config (helm-projectile-on))
  

  ;; #TODO(4lex1v, 07/20/18) :: Marked for removal, though might be helpful if rg is not avail.
  (use-package helm-ag :ensure t :disabled t
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
    :diminish (helm-gtags-mode . "GT")
    :after helm
    
    :hook (prog-mode . helm-gtags-mode)
    
    :general
    (:prefix nil

     "C-]"   'helm-gtags-dwim
     "C-M-]" 'helm-gtags-select)

    ("t"     '(:ignore t :which-key "Tags")
     "tt"    'helm-gtags-dwim
     "tf"    'helm-gtags-find-tag-other-window)
    
    :init
    (setq helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-ignore-case t)))

(use-package rg :demand t
  :general
  ("ps"  '(:ignore t :which-key "Search")
   "pss" 'rg-dwim-project-dir
   "psr" 'rg-project
   "psl" 'rg-literal))

;; Need to organize this to avoid disambiguity and not to forget
;; #NOTE :: DOESN'T REQUIRE Prefix
(general-evil-define-key 'normal 'global-map :prefix nil
  
  ;; Window Management
  "M-q"    '4lex1v:w/close-other-window ;; #TODO :: Doesn't look i'm using this at all
  
  ;; Buffer Management
  "C-q"    '4lex1v/close-buffer) ;; #TODO :: Evil has C-w c which seems to be very convenient

(defmacro open-hff-in-folder (folder)
  "fast way to access the the folder with helm"
  `(lambda ()
     (interactive)
     (let ((default-directory ,(format "~/%s/" folder)))
       (helm-find-files nil))))

(general-evil-define-key '(normal insert) 'global-map
  :prefix nil

  "C-w i" '(clone-indirect-buffer-other-window :which-key "Indirect Buffer"))

;; #NOTE :: REQUIRES Prefix
(general-evil-define-key 'normal 'global-map
  ;; Toggles
  "et"   '(:ignore t :which-key "Toggles")
  "etl"  'toggle-truncate-lines

  ;; Files
  "f"  '(:ignore t :which-key "Files")
  "fs" `(,(open-hff-in-folder "Sandbox") :which-key "Sandbox")
  "fd" `(,(open-hff-in-folder "Dropbox") :which-key "Dropbox")
  "fw" `(,(open-hff-in-folder "Sandbox/Work") :which-key "Work")
  "fl" '(find-library :which-key "Find Library")

  ;; Services
  "s" '(:ignore t :which-key "Services"))

