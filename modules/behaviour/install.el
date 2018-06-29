
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

(use-package projectile
  ;; #NOTE :: this is configured using Spaceline, no need to duplicated in minor mode section
  :diminish projectile-mode
  
  :commands projectile-project-root

  :general
  (:prefix ""
   
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
        helm-gtags-ignore-case t))

;; Need to organize this to avoid disambiguity and not to forget
(delete-selection-mode t)
(global-auto-revert-mode t)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(fset 'yes-or-no-p   'y-or-n-p)

;; #NOTE :: DOESN'T REQUIRE Prefix
(general-evil-define-key 'normal 'global-map :prefix ""
  
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
