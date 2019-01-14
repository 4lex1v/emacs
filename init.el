(setq gc-cons-threshold 10000000)

(defconst IS-MAC               (eq system-type 'darwin))
(defconst IS-WINDOWS           (eq system-type 'windows-nt))
(defconst IS-UNIX              (not IS-WINDOWS))
(defconst USER-INIT-FILE       (or load-file-name (buffer-file-name)))
(defconst USER-EMACS-DIRECTORY (file-name-directory USER-INIT-FILE))

(defmacro func (name &rest body)
  "Shortcut for basic interactive no-arg functions"
  `(defun ,name ()
     (interactive)
     ,@body))

(defmacro -func (body)
  `(lambda ()
     (interactive)
     ,@body))

(defmacro with-package (pkg-name &rest body)
  (declare (indent 1))
  `(if (fboundp ',pkg-name) (progn ,@body)))

;; #TODO :: search fails if the buffer is opened and not at the beginning of the buffer
(defun edit-face-at-point ()
  "Editor face of the active theme at the point."
  (interactive)
  (-if-let* ((face-name (face-at-point))
             (theme-file-buffer (find-library (concat (symbol-name theme-to-load) "-theme"))))
      (with-current-buffer theme-file-buffer
        (search-forward (symbol-name face-name)))))

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

(defun 4lex1v:gui:frame (&rest configs)
  "Helper function for simpler frame configuration"
  (pcase-let* ((`(,active . ,inactive) (plist-get configs :transparency))
               (`(,active-cursor . ,inactive-cursor) (plist-get configs :cursor)))
    
    (setq-default cursor-type active-cursor
                  cursor-in-non-selected-windows inactive-cursor)

    (set-frame-parameter (selected-frame) 'alpha (cons active inactive))
    (add-to-list 'default-frame-alist (cons 'alpha (cons active inactive)))))

;; #NOTE(4lex1v, 08/24/17) :: Default to an empty string that should be introduced manually
(setq comment-note-comment-prefix "")

(setq-default
 truncate-lines t
 initial-major-mode (quote fundamental-mode)
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
 line-spacing 2)

(setq
 show-paren-delay            0.0
 ring-bell-function         'ignore
 tramp-default-method       "ssh"
 make-backup-files              nil
 auto-save-default              nil
 inhibit-startup-message        t
 initial-scratch-message        nil
 kill-do-not-save-duplicates    t
 ad-redefinition-action        'accept
 next-line-add-newlines         t
 desktop-save-mode              nil
 desktop-save                   nil
 user-ref-name                 "4lex1v"
 mouse-wheel-scroll-amount     '(1)
 mouse-wheel-progressive-speed  nil
 inhibit-compacting-font-caches t
 
 default-font-name           "PragmataPro"
 default-font-size           14
 default-font-setting        (format "%s %d" default-font-name default-font-size)
 
 default-directory           "~/Sandbox"
 
 theme-to-load               'sirthias
 search-upper-case            nil)

(show-paren-mode        t)
(delete-selection-mode  t)
(tooltip-mode          -1)
(tool-bar-mode         -1)
(menu-bar-mode         -1)
(scroll-bar-mode       -1)

(set-frame-font default-font-setting)
(add-to-list 'default-frame-alist (cons 'font default-font-setting))
(set-face-attribute 'default nil :font default-font-setting)

(delete-selection-mode t)
(global-auto-revert-mode t)

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(fset 'yes-or-no-p   'y-or-n-p)

(require 'mode-local)
(require 'package)

(setq package-enable-at-startup nil
      package--init-file-ensured t
      package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("marmalade"    . "https://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))

;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(defun package--save-selected-packages (&rest opt) nil)

(package-initialize)

;; Use-Package
(setq use-package-verbose               t
      use-package-always-defer          t
      use-package-enable-imenu-support  t
      use-package-check-before-init     t
      use-package-minimum-reported-time 0.1)

;; Only when the config is stable
(setq use-package-expand-minimally t)

(add-to-list 'load-path (expand-file-name "modules/use-package" USER-EMACS-DIRECTORY))

(require 'bind-key)
(require 'use-package)
(use-package upe-hooks :demand t :load-path "modules/upe-hooks")
(use-package diminish :ensure t :demand t
  :config
  (diminish 'auto-revert-mode))

(use-package s    :ensure t :demand t) ;; Strings manipulation library
(use-package f    :ensure t :demand t) ;; Files manipulation library
(use-package dash :ensure t :demand t) ;; List manipulation library

(use-package sirthias-theme :load-path "themes/sirthias" :demand t
  :if (eq theme-to-load 'sirthias)
  
  :init
  (setq
   sirthias-easy-mode t
   sirthias-cold-mode nil)
  
  (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'sirthias t)))
  
  :config
  (load-theme 'sirthias t))

;; #TODO(4lex1v, 08/28/18) :: Should this also double check that we are using the pragmata font as well?
;; #TODO(4lex1v, 08/28/18) :: Double check if it really slows down the work on windows
;; (if (not IS-WINDOWS)
;;     (load "fonts/pretty-pragmata"))

;; Set of custom hack of the default theme to make it a bit prettier
(if (eq theme-to-load 'default)
    (progn 
      (set-face-attribute 'fringe nil :background nil)
      (with-eval-after-load "eshell"
        (lambda ()
          (set-face-attribute 'eshell-prompt nil :foreground "#000080")))))

(use-package general :demand t :load-path "modules/general"
  :init
  (setq general-default-states  'normal
        general-default-prefix  "<SPC>")
  
  :config
  (with-eval-after-load 'evil
    (general-evil-setup t)))

(use-package evil :load-path "modules/evil" :demand t
  :after general ;; To enable evil-leader in initial buffers
  
  :functions (prog-mode-hook)
  
  :hooks hs-minor-mode
  
  :preface
  (setq 
   evil-want-integration     nil
   evil-collection-company-use-tng nil)
  
  :general
  (:prefix   nil
   :keymaps 'evil-motion-state-map
   :states   nil
   
   "j"   'evil-next-visual-line
   "k"   'evil-previous-visual-line)

  (:prefix nil
   
   "$"   'evil-end-of-visual-line
   "C-j" 'evil-forward-paragraph
   "C-k" 'evil-backward-paragraph
   "g,"  'evil-jump-backward
   "g."  'find-function-at-point
   "C-q" '4lex1v/close-buffer
   "zl"  'hs-hide-level

   ;; Navigation keys
   "C-S-o" #'evil-jump-forward)

  (:states 'normal
   
   "f"   '(:ignore t :which-key "Files")
   "fi"  'init.el
   "fe"  'eshell
   
   "e"   '(:ignore t :which-key "Emacs")
   "eq"  'save-buffers-kill-emacs
   "er"  'revert-buffer

   "ee"  '(:ignore t :which-key "Evil")
   "een" '(evil-ex-nohighlight :which-key "No Highlighting"))
  
  :init
  (setq evil-default-cursor             t
        evil-ex-substitute-global       t
        evil-ex-search-vim-style-regexp t
        evil-want-C-u-scroll            t
        evil-ex-interactive-search-highlight 'selected-window
        evil-want-keybinding            nil)
  
  ;; #NOTE :: This makes things like `just_an_example' selectable as a single word
  (defun fix-word-def () (modify-syntax-entry ?_ "w"))
  (add-hook #'prog-mode-hook 'fix-word-def)
  
  :config
  (evil-set-initial-state 'prog-mode   'normal)
  (evil-set-initial-state 'comint-mode 'normal)
  
  (evil-set-initial-state 'package-menu-mode 'motion)

  ;; Use `§' key to switch between emacs and normal state
  ;; (evil-global-set-key 'normal "§" #'evil-emacs-state)
  ;; (evil-global-set-key 'emacs  "§" #'evil-exit-emacs-state)

  (evil-ex-define-cmd "e[val]" #'eval-buffer)
  (evil-ex-define-cmd "we" #'(lambda () (interactive) (save-buffer) (eval-buffer)))
  
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode)

  (use-package evil-collection :ensure t :demand t
    :after evil
    :commands evil-collection-init
    :init
    (setq evil-collection-setup-minibuffer nil
          evil-collection-mode-list `(arc-mode
                                      bookmark
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

  (use-package evil-surround :ensure t
    :after evil
    :commands global-evil-surround-mode
    :config
    (global-evil-surround-mode 1))

  (use-package evil-args :ensure t :demand t
    :after evil
    :config
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args))

  (general-evil-define-key 'normal 'global-map
    "et"   '(:ignore t :which-key "Toggles")
    "etl"  'toggle-truncate-lines
    "f"    '(:ignore t :which-key "Files")
    "fs"   `(,(open-hff-in-folder "Sandbox") :which-key "Sandbox")
    "fd"   `(,(open-hff-in-folder "Dropbox") :which-key "Dropbox")
    "fw"   `(,(open-hff-in-folder "Sandbox/Work") :which-key "Work")
    "fl"   '(find-library :which-key "Find Library")
    "s"    '(:ignore t :which-key "Services"))
  
  (general-evil-define-key 'normal 'global-map :prefix nil
    "M-q"    '4lex1v:w/close-other-window
    "C-q"    '4lex1v/close-buffer)
  
  (general-evil-define-key '(normal insert) 'global-map :prefix nil
    "C-;"    'toggle-comment-on-line
    "C-x \\" 'align-regexp
    "C-c r"  'revert-buffer
    "M-j"    'join-line
    "M-o"    '4lex1v/insert-line-and-jump
    "C-S-d"  '4lex1v/duplicate-line
    "C-w i"  '(clone-indirect-buffer-other-window :which-key "Indirect Buffer")))

;; Goes before others to correctly load which-key-declare-prefixes
(use-package which-key :demand t
  :load-path "modules/which-key"
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

(use-package async :ensure t)
(use-package helm :load-path "modules/helm" :demand t
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
   
   ;; #NOTE(4lex1v, 01/09/19) :: Bad idea, searches from the first line, doesn't make an auto-jump to the first occurence.
   ;;                            much better on its own.
   ;; "/"  'helm-occur
   "ga" 'helm-apropos)
  
  (:prefix   nil
   :keymaps 'helm-find-files-map
   :states   nil
   
   "C-<backspace>"   'backward-kill-word)

  (:prefix   nil
   :keymaps 'helm-map
   :states   nil
   
   "<tab>" 'helm-execute-persistent-action
   "C-i"   'helm-execute-persistent-action
   "C-z"   'helm-select-action
   "C-o"   'helm-next-source
   "C-j"   'helm-next-line
   "C-k"   'helm-previous-line)

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

  (func init.el (find-file (concat USER-EMACS-DIRECTORY "/" "init.el")))
  (general-define-key "ff" 'helm-find-files)
  
  (substitute-key-definition 'find-tag 'helm-etags-select global-map)

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
    :commands helm-projectile-on
    :general
    ("pp"  'helm-projectile-switch-project)
    
    (:prefix nil

     "C-M-3" 'helm-projectile-switch-to-buffer
     "M-1"   'helm-projectile-find-file)
    
    :config (helm-projectile-on))

  ;; #TODO(4lex1v, 07/20/18>) :: Marked for removal, though might be helpful if rg is not avail.
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
  
  ;; #TODO(4lex1v, 09/03/18) :: Install on Windows?
  (use-package helm-gtags :ensure t
    :if (not IS-WINDOWS)
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

;; #TODO(4lex1v, 07/20/18) :: Would demanding projectile at the startup slowdown the process?
(use-package projectile :load-path "modules/projectile" :demand t   
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
  (projectile-mode))

(use-package magit :defer 2
  :load-path "modules/magit/lisp"
  :commands (magit magit-status magit-diff-range magit-clone)
  
  :general 
  ("g" '(:ignore t :which-key "Magit")
   "gs"  'magit-status
   "gm"  'magit-dispatch-popup
   "gb"  'magit-addition-blame
   "g'"  'magit-submodule-popup
   "gy"  'magit-show-refs-popup
   "ge"  'magit-ediff-popup
   "gp"  'magit-push-popup
   "gd"  'magit-diff-popup
   "gD"  'magit-diff-branch-with-master
   "gf"  'magit-pull-popup
   "gl" '(:ignore t :which-key "Logging")
   "gll" 'magit-log-all
   "glb" 'magit-log-buffer-file
   "glc" 'magit-log-current)
  
  (:keymaps 'magit-status-mode-map
            :prefix   nil

            "j" 'magit-next-line
            "k" 'magit-previous-line)
  
  (:keymaps 'magit-diff-mode-map
            :prefix   nil
            
            "gf" 'magit-diff-visit-file-other-window)

  (:keymaps 'magit-submodule-list-mode-map
            :prefix   nil
            
            "RET" 'magit-repolist-status)
  
  :init
  (setq-default
   magit-submodule-list-columns
   (quote
    (("Path" 50 magit-modulelist-column-path nil)
     ("Version" 35 magit-repolist-column-version nil)
     ("Branch" 20 magit-repolist-column-branch nil)
     ("L<U" 3 magit-repolist-column-unpulled-from-upstream
      ((:right-align t)))
     ("L>U" 3 magit-repolist-column-unpushed-to-upstream
      ((:right-align t)))
     ("L<P" 3 magit-repolist-column-unpulled-from-pushremote
      ((:right-align t)))
     ("L>P" 3 magit-repolist-column-unpushed-to-pushremote
      ((:right-align t))))))

  (setq magit-last-seen-setup-instructions "2.11.0"
        magit-status-show-hashes-in-headers t
        
        ;; Magit Diff configs
        magit-diff-options          '("--stat" "--no-ext-diff" "--word-diff")
        magit-diff-refine-hunk      'all
        magit-diff-paint-whitespace 'status)
  
  (defun magit-diff-visit-file-other-window (file)
    (interactive (list (--if-let (magit-file-at-point)
                           (expand-file-name it)
                         (user-error "No file at point"))))
    (magit-diff-visit-file file t))
  
  
  ;; This function was added to speed up my PR review workflow in a way that i can diff current branch
  ;; with master by a single keystroke...
  (defun magit-diff-branch-with-master ()
    (interactive)
    (let* ((args (magit-diff-arguments))
           (diff-cmd (format "master...%s" (magit-get-current-branch))))
      (magit-diff-range diff-cmd args)))
  
  :config
  (add-to-list 'magit-log-arguments "--color")
  (add-to-list 'magit-diff-arguments "--ignore-space-change")

  (magit-define-popup-action 'magit-submodule-popup   
                             ?l "List" 'magit-list-submodules)
  
  (magit-define-popup-switch 'magit-log-popup
                             ?f "First Parent" "--first-parent")

  (define-key magit-file-section-map [remap magit-visit-thing] #'magit-diff-visit-file-other-window)

  (add-hook 'magit-submodule-list-mode-hook
            (lambda () (setq-local tabulated-list-sort-key (cons "L<U" t))))

  (use-package magit-popup :ensure t :after magit)
  (use-package ghub :ensure t :after magit)

  (use-package with-editor :ensure t 
    :after magit
    :general
    (:keymaps 'with-editor-mode-map
              :prefix "" ;; don't use SPC prefix in this case
              "RET"    'with-editor-finish
              [escape] 'with-editor-cancel)
    :config
    (with-eval-after-load 'evil
     (evil-set-initial-state 'with-editor-mode 'insert)))

  (use-package evil-magit :ensure t :demand t
    :after (evil magit-mode)
    :commands evil-magit-init
    :config
    (evil-magit-init)))

(use-package exec-path-from-shell :ensure t :demand t
  :commands (exec-path-from-shell-getenv
             exec-path-from-shell-setenv)
  :init
  ;; Under certain conditions this can be nicely used withing Windows environment as well...
  (defun run-shell-command (&rest cmd)
    (replace-regexp-in-string "\r?\n\\'" ""
                              (shell-command-to-string
                               (mapconcat 'identity cmd " ")))) 
  
  ;; TODO :: Check if it works on Windows
  (defun register-path-folders (&rest paths)
    (declare (indent 1))
    (let ((path (-reduce-r-from
                 (lambda (value acc) (format "%s:%s" acc value))
                 (exec-path-from-shell-getenv "PATH")
                 paths)))
      
      (exec-path-from-shell-setenv "PATH" path))))

(use-package osx :if IS-MAC :demand t
  :after exec-path-from-shell
  :defines
  (mac-command-modifier
   mac-option-modifier
   mac-control-modifier
   ns-function-modifier
   ns-use-native-fullscreen)
  
  :init
  (setq browse-url-browser-function 'browse-url-default-macosx-browser
        delete-by-moving-to-trash    t
        mac-command-modifier        'meta
        mac-option-modifier         'super
        mac-control-modifier        'control
        ns-function-modifier        'hyper
        ns-use-native-fullscreen     t
        frame-resize-pixelwise       t
        shell-file-name              "/bin/sh")

  :general
  (:prefix nil
   :states '(normal insert)
   
   "M-`" 'ns-next-frame)

  :config
  (message "[CONFIGURATION] Loading MacOS system configuration")

  (exec-path-from-shell-setenv "HOMEBREW_PREFIX" "/usr/local")
  (exec-path-from-shell-setenv "HOMEBREW_CELLAR" "/usr/local/Cellar")
  (exec-path-from-shell-setenv "GTAGSCONF" "/usr/local/share/gtags/gtags.conf")
  (exec-path-from-shell-setenv "GTAGSLABEL" "ctags")
  (register-path-folders "/usr/local/opt/llvm/bin" "/usr/local/homebrew/bin" "/usr/local/bin")

  (use-package em-alias
    :config
    (eshell/alias "bubu" "brew update && brew upgrade")
    (eshell/alias "sshs" "ssh-add ~/.ssh/github_rsa")))

(use-package windows :if IS-WINDOWS
  :init
  (message "[CONFIGURATION] Loading Windows OS system configuration")
  
  ;; #NOTE(4lex1v) :: Not sure if these paths should be defined here or in Native modules configuration
  )

;; (use-package powerline :ensure t :demand t :disabled t
;;   :config
;;   (defpowerline project-segment 
;;     (if (and (fboundp 'projectile-project-p)
;;              (projectile-project-p))
;;         (let ((project-name (projectile-project-name))
;;               (branch-name
;;                (if (and (not IS-WINDOWS)
;;                         (fboundp 'magit-get-current-branch))
;;                    (magit-get-current-branch))))
;;           (propertize
;;            (if (or (eq branch-name nil)
;;                    (string-empty-p branch-name))
;;                (format "[%s]" project-name)
;;              (format "[%s @ %s]"
;;                      project-name
;;                      branch-name))))))
  
;;   (setq-default
;;    mode-line-format
;;    '("%e"
;;      (:eval
;;       (let* ((active (powerline-selected-window-active))
;;              (face0 (if active 'powerline-active0 'powerline-inactive0))
;;              (separator-left (intern (format "powerline-%s-%s"
;;                                              (powerline-current-separator)
;;                                              (car powerline-default-separator-dir))))
;;              (separator-right (intern (format "powerline-%s-%s"
;;                                               (powerline-current-separator)
;;                                               (cdr powerline-default-separator-dir))))
             
;;              (lhs (list (powerline-raw evil-mode-line-tag face0)
;;                         (project-segment face0)
;;                         (powerline-buffer-id face0 'l)
;;                         (powerline-raw ":%l" face0)))
             
;;              (rhs (list (powerline-raw "[" face0 'l)
;;                         (powerline-minor-modes face0)
;;                         (powerline-raw "]" face0 'r)
;;                         (powerline-major-mode face0 'r))))
        
;;         (concat (powerline-render lhs)
;;                 (powerline-fill face0 (powerline-width rhs))
;;                 (powerline-render rhs)))))))

;; #TODO(4lex1v, 01/11/19):
;;   - Need to work more on keybindings, hitting SPC W for an access is too slow for this, though maybe with time this would become easeier / faster
;;   - Would be nice to add a function that would allow me to open a file from Helm's FF in a new workspace
(use-package eyebrowse :ensure t
  :after hydra 
  
  :commands
  (eyebrowse-create-window-config
   eyebrowse-close-window-config
   eyebrowse-prev-window-config
   eyebrowse-next-window-config
   eyebrowse-switch-to-window-config-0
   eyebrowse-switch-to-window-config-1
   eyebrowse-switch-to-window-config-2
   eyebrowse-switch-to-window-config-3)
  
  :general
  ("w"  'hydra-eyebrowse/body)
  
  :config
  (defhydra hydra-eyebrowse (:color pink :hint nil)
    "
   ^Configs^      ^Navigation^
-----------------------------
 _1_: Config 1    _c_: Create
 _2_: Config 2    _k_: Close Current 
 _3_: Config 3    _j_: Previous
 _4_: Config 4    _l_: Next
-----------------------------
"
    ("c" eyebrowse-create-window-config :color blue)
    ("k" eyebrowse-close-window-config :color blue)
    ("j" eyebrowse-prev-window-config)
    ("l" eyebrowse-next-window-config)
    
    ("1" eyebrowse-switch-to-window-config-0)
    ("2" eyebrowse-switch-to-window-config-1)
    ("3" eyebrowse-switch-to-window-config-2)
    ("4" eyebrowse-switch-to-window-config-3)
    
    ("q" nil "quit"))

  (eyebrowse-mode))

(use-package hydra :load-path "modules/hydra" :demand t 
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

(use-package rg :ensure t
  :general
  ("ps"  '(:ignore t :which-key "Search")
   "pss" 'rg-dwim-project-dir
   "psr" 'rg-project
   "psl" 'rg-literal))

(use-package smartparens :load-path "modules/smartparens"
  :commands
  (sp-forward-slurp-sexp
   sp-backward-slurp-sexp
   sp-forward-barf-sexp
   sp-backward-barf-sexp
   sp-pair
   sp-local-pair
   smartparens-mode
   sp-with-modes
   sp-local-pairs)
  
  :hook
  ((conf-mode text-mode) . smartparens-mode)
  
  :general
  (:keymaps 'smartparens-mode-map
            :prefix   nil
            :states  '(normal insert)
            
            "M-t"   'sp-transpose-sexp
            
            "C-M-k" 'sp-kill-sexp
            "C-M-w" 'sp-copy-sexp
            
            "C-s" 'hydra-smartparens/body)
  
  :init
  (setq sp-base-key-bindings nil
        sp-autoinsert-if-followed-by-word t
        sp-autoskip-closing-pair 'always-end
        sp-hybrid-kill-entire-symbol nil)
  
  (defhydra hydra-smartparens (:color pink :hint nil)
    
    "
^Slurp^         ^Barfs^
--------------------
_l_: f-slurp    _L_: f-barf
_h_: b-slurp    _H_: b-barf
--------------------
"
    
    ("l" sp-forward-slurp-sexp)
    ("h" sp-backward-slurp-sexp)
    ("L" sp-forward-barf-sexp)
    ("H" sp-backward-barf-sexp)
    ("q" nil "cancel"))
  
  :config
  (use-package smartparens-config :demand t)
  
  (sp-pair "(" ")"   :wrap "C-(")
  (sp-pair "[" "]"   :wrap "s-[")
  (sp-pair "\"" "\"" :wrap "C-\"")
  (sp-pair "<" ">"   :wrap "C-<")
  
  (sp-pair "{" "}"   :wrap "C-{")
  (sp-local-pair '(c++-mode) "{" "};"))

(use-package yasnippet :load-path "modules/yasnippet"
  :diminish (yas-minor-mode . " Y")

  :commands
  (yas-minor-mode
   yas-load-directory
   yas-activate-extra-mode
   yas-insert-snippet
   yas-visit-snippet-file
   yas-new-snippet
   yas-tryout-snippet
   yas-describe-tables
   yas-reload-all)
  
  :mode ("\\.yasnippet" . snippet-mode)
  
  :general
  ("es" '(hydra-yasnippet/body :which-key "Snippets"))
  
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")
        yas-wrap-around-region t
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  
  (add-hook 'after-save-hook
            (lambda ()
              (when (eq major-mode 'snippet-mode)
                (yas-reload-all))))
  
  (defhydra hydra-yasnippet (:color blue :hint nil)
    "
^Modes^    ^Load/Visit^    ^Actions^
--------------------------------------------
_m_inor   _d_irectory      _i_nsert
_e_xtra   _f_ile           _t_ryout
^ ^       _l_ist           _n_ew
^ ^       _a_ll
"
    ("d" yas-load-directory)
    ("e" yas-activate-extra-mode)
    ("i" yas-insert-snippet)
    ("f" yas-visit-snippet-file)
    ("n" yas-new-snippet)
    ("t" yas-tryout-snippet)
    ("l" yas-describe-tables)
    ("m" yas-minor-mode)
    ("a" yas-reload-all))
  
  :config
  (yas-reload-all))

(use-package company :load-path "modules/company"
  :commands company-mode
  
  :functions (company-clang)
  
  :general
  (:prefix  nil
            :states '(insert)
            
            "C-SPC" 'company-complete)
  
  (:prefix   nil
             :keymaps 'company-active-map
             :states   nil
             
             "C-j" 'company-select-next-or-abort
             "C-k" 'company-select-previous-or-abort
             "C-o" 'company-other-backend
             "C-l" 'company-other-backend
             "C-d" 'company-show-doc-buffer)

  :hook ((text-mode) . company-mode)
  
  :init
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        
        company-idle-delay 0.3
        company-minimum-prefix-length 3
        
        company-selection-wrap-around t
        company-tooltip-align-annotations t

        company-transformers '(company-sort-by-occurrence)
        company-backends '())
  
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'company))

  (cl-defmacro configure-company-backends-for-mode (mode backends)
    (declare (indent 1))
    `(add-hook
      ',(intern (concat (symbol-name mode) "-hook"))
      (lambda ()
        (make-local-variable 'company-backends)
        (setq company-backends (list (remove nil ,backends)))))))

(use-package flycheck :ensure t :if IS-UNIX
  :general
  ("ef"  '(:ignore t :which-key "Flycheck")
   "efl" 'flycheck-list-errors))

(use-package flyspell :ensure t :if IS-UNIX
  :bind
  (("C-c i b" . flyspell-buffer)
   ("C-c i f" . flyspell-mode))
  
  :init
  (use-package ispell
    :bind
    (("C-c i c" . ispell-comments-and-strings)
     ("C-c i d" . ispell-change-dictionary)
     ("C-c i k" . ispell-kill-ispell)
     ("C-c i m" . ispell-message)
     ("C-c i r" . ispell-region)))
  
  :config
  (unbind-key "C-." flyspell-mode-map))

(use-package undo-tree :ensure t
  :diminish undo-tree-mode
  :commands global-undo-tree-mode
  :general
  (:prefix nil
           :states 'normal

           "M-/" 'undo-tree-visualize)
  
  (:prefix   nil
             :keymaps 'undo-tree-visualizer-mode-map
             :states  '(motion)

             "j" 'undo-tree-visualize-redo
             "k" 'undo-tree-visualize-undo
             "l" 'undo-tree-visualize-switch-branch-right
             "h" 'undo-tree-visualize-switch-branch-left)
  
  :config (global-undo-tree-mode))

(use-package string-inflection :ensure t
  :general
  (:prefix  nil
            :states '(normal)

            "gu" 'string-inflection-all-cycle))

(use-package elisp-mode
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode        (("\\.el$" . emacs-lisp-mode)
                ("Cask"   . emacs-lisp-mode))
  
  :functions (emacs-lisp-mode-hook)
  
  :hooks
  (:emacs-lisp-mode-hook
   yas-minor-mode
   company-mode
   smartparens-mode
   hs-minor-mode)
  
  :general
  (:prefix ""
           "M-."     'find-function-at-point
           "M-,"     'find-variable-at-point
           "C-c e r" 'eval-region)
  
  (:keymaps 'emacs-lisp-mode-map
            "e"  '(:ignore t :which-key "Emacs")
            "ev" '(:ignore t :which-key "Describe Variable")
            "ed" '(:ignore t :which-key "Docs & Help")
            "eda" #'helm-apropos)
  
  :init
  (setq-mode-local emacs-lisp-mode comment-note-comment-prefix ";;")
  
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'elisp-mode))

  :config
  (with-eval-after-load 'company
    (configure-company-backends-for-mode emacs-lisp-mode
      '(company-elisp company-capf company-files company-yasnippet)))

  (with-eval-after-load 'smartparens
    (sp-with-modes 'emacs-lisp-mode
      (sp-local-pair "'" nil :actions nil)))

  (use-package macrostep :ensure t
    :after elisp-mode
    :commands macrostep-expand
    :mode ("\\*.el\\'" . emacs-lisp-mode)
    
    :general
    ;; Support Macrostep in Evil mode
    (:keymaps 'macrostep-keymap :prefix ""
              "q" #'macrostep-collapse-all
              "e" #'macrostep-expand)

    (general-define-key :keymaps 'emacs-lisp-mode-map
                        "em" #'macrostep-expand)
    
    :init
    (with-eval-after-load 'evil-collection
      (add-to-list 'evil-collection-mode-list 'macrostep)))

  (use-package deferred :ensure t :disabled t
    :after elisp-mode)

  (use-package request :ensure t :disabled t
    :after (elisp-mode deferred)
    :init
    (use-package request-deferred :after deferred)
    (setq request-log-level 'debug
          request-message-level 'warn))

  (use-package elisp-refs :ensure t :disabled t
    :init
    (with-eval-after-load 'evil-collection
      (add-to-list 'evil-collection-mode-list 'elisp-refs)))
  
  (add-hook 'after-save-hook 
            (lambda ()
              (if (string= (buffer-file-name)
                           USER-INIT-FILE)
                  (byte-recompile-file USER-INIT-FILE)))))

(use-package scala-mode :disabled t
  :load-path "modules/scala-mode"
  :mode        ("\\.\\(scala\\|sbt\\|sc\\)\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  
  :hooks
  (4lex1v/fix-scala-fonts
   smartparens-mode
   yas-minor-mode
   company-mode
   hs-minor-mode)
  
  :general
  (:keymaps 'scala-mode-map
            "s" '(:ignore t :which-key "Scala"))
  
  (:keymaps 'scala-mode-map
            :states  '(normal insert)
            :prefix   nil
            
            "<C-return>"     #'newline-or-comment)

  (:keymaps 'scala-mode-map
            :states  'normal
            :prefix   nil
            
            "J" #'scala-join-lines)

  :init
  (setq scala-indent:use-javadoc-style t
        scala-mode:debug-messages nil)

  (setq-mode-local scala-mode comment-note-comment-prefix "//")
  
  :config
  (load "scala-defs")

  (with-eval-after-load 'company
    (configure-company-backends-for-mode scala-mode
      '(company-dabbrev
        company-keywords
        company-capf
        company-yasnippet
        company-files)))
  
  (use-package smartparens-scala
    :after (:all smartparens scala-mode)
    :config
    (message "Smartparens for Scala has been configured"))

  (use-package sbt-mode
    :load-path "modules/sbt-mode"
    :after scala-mode
    :general
    (:keymaps 'scala-mode-map :prefix "<SPC> sb"
              ""  '(:ignore t :which-key "SBT")
              "b" '(4lex1v:open-sbt-build-file :which-key "build.sbt")
              "s" 'sbt-start
              "r" 'sbt-command
              "c" '(4lex1v:sbt-compile-command :which-key "compile"))
    
    (:keymaps 'scala-mode-map :prefix ","
              "c" '(4lex1v:sbt-compile-command :which-key "compile")
              "r" 'sbt-run-previous-command
              "i" '4lex1v/open-in-intellij)

    :init
    (setq sbt:program-name "sbt shell -mem 2048 -v"
          sbt:prompt-regexp  "^\\(\\(scala\\|\\[[^\]]*\\] \\)?[>$]\\|[ ]+|\\)[ ]*")
    
    :config
    (load "sbt-defuns")
    (setq-default truncate-lines t)
    
    (with-eval-after-load 'evil
      (evil-set-initial-state 'sbt-mode 'insert))))

(use-package cc-mode
  :mode (("\\.h\\'"  . c++-mode)
         ("\\.mm\\'" . objc-mode))
  
  :commands c-toggle-auto-newline
  :defines (c-mode-common-hook)
  
  :hooks
  (:c-mode-common-hook
   smartparens-mode
   yas-minor-mode
   company-mode
   hs-minor-mode)
  
  :general
  (:keymaps 'c-mode-base-map
            
            "m"  '(:ignore t :which-key "Native")
            "ma" 'projectile-find-other-file
            "mA" 'projectile-find-other-file-other-window)
  
  ;; #TODO :: Check if this could be defined in the global configuration or it needs to be overriden for these modes?
  (:prefix nil :keymaps 'c-mode-base-map  
             "C-S-j" #'next-error
             "C-S-k" #'previous-error
             ",r"    #'recompile)
  
  :init
  (setq 
   c-default-style "stroustrup"
   c-basic-offset 2
   org-babel-C-compiler "clang"
   org-babel-C++-compiler "clang++")
  
  (if IS-WINDOWS
      (setq
       win32-system-include-paths '("c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/shared"
                                    "c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/ucrt"
                                    "c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/um"
                                    "c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/winrt")))
  
  :config
  (c-toggle-auto-newline t)
  
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(C . t)))
  
  ;; Something helpful in Handmade Hero... Not sure if i'm going to use it in other projects...
  (font-lock-add-keywords 'c++-mode '(("\\<\\(assert\\|internal\\|global_var\\|local_persist\\)\\>" 1 font-lock-keyword-face)))
  (font-lock-add-keywords 'objc-mode '(("\\<\\(assert\\|internal\\|global_var\\|local_persist\\)\\>" 1 font-lock-keyword-face)))
  
  ;; #TODO :: Add company-xcode for Objective C
  (with-eval-after-load 'company
    (configure-company-backends-for-mode c-mode-common
      `(company-capf
        company-dabbrev
        company-keywords
        company-yasnippet
        company-files
        ,(if (and IS-UNIX (require 'company-clang nil t))
             (function company-clang)))))
  
  (with-eval-after-load 'smartparens
    (sp-local-pair 'c++-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))
  
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(C . t)))

  (use-package cmake-mode :ensure t :disabled t
    :after cc-mode
    :hooks (company-mode)
    
    :init
    (with-eval-after-load 'evil-collection
      (add-to-list 'evil-collection-mode-list 'cmake-mode))
    
    :config
    (with-eval-after-load 'company
      (configure-company-backends-for-mode cmake-mode
        '(company-cmake company-files company-dabbrev company-capf))))

  (use-package disaster :disabled t
    :after cc-mode
    :commands disaster
    :general
    (:prefix "m"
             :keymaps 'c-mode-base-map

             "d" 'disaster))

  (use-package semantic :load-path "modules/cedet/lisp/cedet" :disabled t
    :after cc-mode
    
    :general
    (:prefix nil :keymaps 'c-mode-base-map
               "g." 'semantic-ia-fast-jump)
    
    ;; :init
    ;; (setq semantic-default-submodes '(global-semanticdb-minor-mode
    ;;                                   global-semantic-idle-scheduler-mode
    ;;                                   global-semantic-idle-local-symbol-highlight-mode
    ;;                                   global-semantic-highlight-func-mode
    ;;                                   global-semantic-idle-completions-mode
    ;;                                   global-semantic-decoration-mode))

    ;; #TODO :: Decide what to do with this... For now the performance on Windows is terrible =(
    ;;(add-hook 'c-mode-common-hook 'semantic-mode)

    :config
    (add-hook 'c-mode-common-hook
              (lambda ()
                (if IS-WINDOWS
                    (-each win32-system-include-paths 'semantic-add-system-include))))

    ;; Configure semantic
    (add-hook 'change-major-mode-hook
              #'(lambda ()
                  (if (derived-mode-p 'c-mode 'c++-mode)
                      (semantic-mode 1)
                    (semantic-mode -1)))))

  (use-package helm-semantic :after (semantic helm))

  ;; (use-package company-semantic
  ;;   :after (semantic company)
  ;;   :config
  ;;   (configure-company-backends-for-mode semantic-mode
  ;;     (add-to-list 'company-backends 'company-semantic)))

  (use-package sourcetrail :ensure t :disabled t
    :after cc-mode)

  )

(use-package rust-mode :disabled t
  :hooks (;; Rust-specific modes
          cargo-minor-mode
          ;; racer-mode
          hs-minor-mode
          yas-minor-mode
          smartparens-mode
          company-mode)
  
  :init 
  (setq rust-indent-offset  2
        rust-format-on-save nil
        rust-toolchain-path (run-shell-command "rustc --print sysroot"))
  
  :config
  (sp-with-modes 'rust-mode
                 (sp-local-pair "(" nil :post-handlers '(("||\n[i]" "RET")))
                 (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))

  ;; (configure-company-backends-for-mode rust-mode
  ;;   '(company-dabbrev
  ;;     company-keywords
  ;;     company-yasnippet
  ;;     company-capf
  ;;     company-files))
  
  (use-package smartparens-rust
    :after (rust-mode smartparens-mode)
    :config
    (add-hook 'rust-mode #'smartparens-rust))

  (use-package cargo :ensure t
    :after rust-mode
    
    :general
    (:prefix ","
             :keymaps 'rust-mode-map
             
             "c" '(:ignore t :which-key "Cargo")
             "c." 'cargo-process-repeat
             "cC" 'cargo-process-clean
             "cX" 'cargo-process-run-example
             "cb" 'cargo-process-build
             "cc" 'cargo-process-check
             "cd" 'cargo-process-doc
             "ce" 'cargo-process-bench
             "cf" 'cargo-process-current-test
             "cf" 'cargo-process-fmt
             "ci" 'cargo-process-init
             "cn" 'cargo-process-new
             "co" 'cargo-process-current-file-tests
             "cs" 'cargo-process-search
             "cu" 'cargo-process-update
             "cx" 'cargo-process-run
             "t" 'cargo-process-test)

    :init
    (setq cargo-process--enable-rust-backtrace t))

  (use-package racer :ensure t
    :after rust-mode
    
    :general
    (:prefix "" :keymaps 'racer-mode-map
             "gd" 'racer-find-definition
             "g." 'racer-find-definition-other-window)
    
    :init
    (setq racer-rust-src-path (concat rust-toolchain-path "/lib/rustlib/src/rust/src"))
    
    :config
    (defun racer-find-definition-other-window ()
      "Run the racer find-definition command and process the results in other window."
      (interactive)
      (-if-let (match (--first (s-starts-with? "MATCH" it)
                               (racer--call-at-point "find-definition")))
          (-let [(_name line col file _matchtype _ctx)
                 (s-split-up-to "," (s-chop-prefix "MATCH " match) 5)]
            (if (fboundp 'xref-push-marker-stack)
                (xref-push-marker-stack)
              (with-no-warnings
                (ring-insert find-tag-marker-ring (point-marker))))
            (switch-to-buffer-other-window file)
            (save-selected-window
              (racer--find-file file (string-to-number line) (string-to-number col))))
        (error "No definition found")))
    
    (add-hook 'racer-mode-hook #'eldoc-mode))

  (use-package company-racer :ensure t :demand t
    :after (racer company)
    
    :config
    (with-eval-after-load 'company
      (configure-company-backends-for-mode rust-mode
        '(company-dabbrev
          company-racer
          company-keywords))))

  (use-package flycheck-rust :ensure t
    :after (rust-mode flycheck-mode)
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

  (use-package toml-mode :ensure t
    :mode ("/\\(Cargo.lock\\|\\.cargo/config\\)\\'" . toml-mode)))

(use-package lua-mode :ensure t :defer t :disabled t
  :mode "\\.lua\\'"
  :init
  (setq lua-indent-level 2))

(use-package web-mode :ensure t :disabled t
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.html\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.jinja\\'" . web-mode)
   ("\\.php\\'" . web-mode))
  
  :init
  (setq web-mode-engines-alist '(("\\.jinja\\'"  . "django"))
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight nil))

(use-package tide :ensure t :disabled t
  :init
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'tide)
    (add-to-list 'evil-collection-mode-list 'typescript-mode)))

(use-package restclient :ensure t :disabled t
  :commands restclient-mode
  :config
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(restclient . t)))

  (use-package ob-restclient :ensure t :after (:both org restclient) :commands org-babel-execute:restclient))

(use-package ssh-agency :if IS-WINDOWS :ensure t
  :after magit
  :commands ssh-agency-ensure
  :init
  (let ((sysroot (getenv "SystemRoot")))
    (setq ssh-agency-keys (list
                           (expand-file-name "~/.ssh/github_rsa")
                           (expand-file-name "~/.ssh/bamtech"))
          
          ssh-agency-add-executable (expand-file-name
                                     (format "%s/%s" sysroot "System32\\OpenSSH\\ssh-add.exe"))
          
          ssh-agency-agent-executable (expand-file-name
                                       (format "%s/%s" sysroot "System32\\OpenSSH\\ssh-agent.exe"))))
  
  :config
  (setenv "SSH_ASKPASS" "git-gui--askpass")
  (ssh-agency-ensure))

(use-package elfeed
  :load-path "modules/elfeed"
  :commands elfeed-update
  :general
  ("se" 'elfeed)

  (:keymaps 'elfeed-search-mode-map
            :states  'normal
            :prefix   nil

            "SPC" 'elfeed-control-panel/body)
  
  :init 
  (setq elfeed-search-filter "@1-month-ago +unread" ;; Default filter
        elfeed-db-directory "~/Dropbox/Приложения/elfeeddb"
        elfeed-feeds '(("https://bartoszmilewski.com/feed" FP)

                       ;; Financing
                       ("http://feeds.feedburner.com/budgetsaresexy?format=xml" Financing)
                       ("http://feeds.feedburner.com/FrugalRules?format=xml" Financing)
                       ("https://www.moneyunder30.com/feed" Financing)
                       
                       ;; Engineering
                       ("https://fgiesen.wordpress.com/feed/" Engineering)
                       
                       ;; Emacs
                       ("http://irreal.org/blog/?feed=rss2" Emacs)
                       ("http://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-and-content.xml" Emacs)

                       ;; C++
                       ("https://www.fluentcpp.com/feed/" C++)
                       ("https://arne-mertz.de/feed/" C++)
                       ("http://www.modernescpp.com/index.php/component/jaggyblog/format=feed&type=rss" C++)
                       ("http://feeds.feedburner.com/codeandgraphics" C++)
                       ("https://herbsutter.com/feed/" C++)
                       ("https://akrzemi1.wordpress.com/feed/" C++)
                       ("https://blogs.msdn.microsoft.com/vcblog/feed/" C++)
                       ("http://blog.regehr.org/feed" C++)
                       
                       ;; Rust
                       ("https://this-week-in-rust.org/rss.xml" Rust)
                       ("feed://smallcultfollowing.com/babysteps/atom.xml" Rust)

                       ;; GameDev
                       ("https://80.lv/feed/" GameDev)
                       ("https://www.unrealengine.com/rss" GameDev UnrealEngine)
                       ("http://labs.domipheus.com/blog/feed/" GameDev)
                       ("http://msinilo.pl/blog/?feed=rss2" GameDev)
                       ("https://0fps.net/feed/" GameDev)
                       ("http://www.sebastiansylvan.com/index.xml" GameDev)
                       ("http://accidentallyquadratic.tumblr.com/rss" GameDev)
                       ("http://www.alexandre-pestana.com/feed/" GameDev)
                       ("http://algassert.com/feed.xml" GameDev)
                       ("http://deplinenoise.wordpress.com/feed/" GameDev)
                       ("http://akrzemi1.wordpress.com/feed/" GameDev)
                       ("http://c0de517e.blogspot.com/feeds/posts/default" GameDev)
                       ("https://anteru.net/rss.xml" GameDev)
                       ("http://aras-p.info/atom.xml" GameDev)
                       ("https://erkaman.github.io/rss.xml" GameDev)
                       ("http://voxelium.wordpress.com/feed/" GameDev)
                       ("http://blog.tobias-franke.eu/rss" GameDev)
                       ("http://www.enkisoftware.com/rss" GameDev)
                       ("http://bartwronski.com/feed/" GameDev)
                       ("http://bitbashing.io/feed.xml" GameDev)
                       ("http://bitsquid.blogspot.com/feeds/posts/default" GameDev)
                       ("http://www.binomial.info/blog?format=RSS" GameDev)
                       ("http://stephaniehurlburt.com/blog?format=RSS" GameDev)
                       ("http://graphicrants.blogspot.com/feeds/posts/default" GameDev)
                       ("http://briansharpe.wordpress.com/feed/" GameDev)
                       ("http://randomascii.wordpress.com/feed/" GameDev)
                       ("http://cppsecrets.blogspot.com/feeds/posts/default" GameDev)
                       ("http://mollyrocket.com/casey/stream_atom.rss" GameDev)
                       ("http://blog.xyzw.us/feeds/posts/default?alt=rss" GameDev)
                       ("http://casual-effects.blogspot.com/feeds/posts/default" GameDev)
                       ("http://cbloomrants.blogspot.com/feeds/posts/default" GameDev)
                       ("http://realtimecollisiondetection.net/blog/?feed=rss2" GameDev)
                       "http://www.thetenthplanet.de/feed"
                       "http://codecapsule.com/feed/"
                       "http://code4k.blogspot.com/feeds/posts/default"
                       "http://blog.dimitridiakopoulos.com/rss/"
                       "http://www.rorydriscoll.com/feed/"
                       "http://www.codersnotes.com/rss/"
                       "http://feeds.feedburner.com/codinghorror/"
                       "https://computingandrecording.wordpress.com/feed/"
                       "http://copypastepixel.blogspot.com/feeds/posts/default"
                       "http://blog.icare3d.org/feeds/posts/default"
                       "http://danluu.com/atom.xml"
                       "https://blog.forrestthewoods.com/feed"
                       "https://flashypixels.wordpress.com/feed/"
                       "http://blog.duangle.com/feeds/posts/default"
                       "https://emilypriceisright.com/feed/"
                       "http://www.epicshaders.com/feed/"
                       "http://fgiesen.wordpress.com/feed/"
                       "http://fabiensanglard.net/rss.xml"
                       "http://filmicworlds.com/feed.xml"
                       "http://eev.ee/feeds/atom.xml"
                       "https://fuzzyreflection.com/feed/"
                       "http://new.gafferongames.com/index.xml"
                       "http://www.petecollier.com/?feed=rss2"
                       "http://donw.io/index.xml"
                       "https://gpfault.net/rss.xml"
                       "http://www.decarpentier.nl/feed"
                       "http://ginsweater.com/blog/feed/"
                       "http://www.glowybits.com/index.xml"
                       "http://gpudissector.blogspot.com/feeds/posts/default"
                       "http://gpupro.blogspot.com/feeds/posts/default"
                       "http://gpuzen.blogspot.com/feeds/posts/default?alt=rss"
                       "http://imdoingitwrong.wordpress.com/feed/"
                       "http://castano.ludicon.com/blog/feed/"
                       "https://imgtec.com/feed/"
                       "http://industrialarithmetic.blogspot.com/feeds/posts/default"
                       "http://ir-ltd.net/feed/"
                       "http://jamesdolan.blogspot.com/feeds/posts/default"
                       "http://playtechs.blogspot.com/feeds/posts/default"
                       "https://jendrikillner.bitbucket.io/index.xml"
                       "http://www.forceflow.be/feed/"
                       "http://joeduffyblog.com/feed.xml"
                       "http://repi.blogspot.com/feeds/posts/default"
                       "http://john-ahlgren.blogspot.com/feeds/posts/default"
                       "http://www.johncalsbeek.com/feed.xml"
                       "http://johnwhite3d.blogspot.com/feeds/posts/default"
                       "http://olickspulpit.blogspot.com/feeds/posts/default"
                       "http://www.jonolick.com/2/feed"
                       "http://joostdevblog.blogspot.com/feeds/posts/default"
                       "http://www.iryoku.com/feed"
                       "http://daugaard.org/blog/?feed=rss2"
                       "https://kate.io/feed.xml"
                       "http://kosmokleaner.wordpress.com/feed/"
                       "http://kosmonautblog.wordpress.com/feed/"
                       "http://interplayoflight.wordpress.com/feed/"
                       "http://kristerw.blogspot.com/feeds/posts/default"
                       "http://knarkowicz.wordpress.com/feed/"
                       "http://leighalexander.net/feed/"
                       "http://feeds.lia-sae.net/main.rss.xml"
                       "http://lousodrome.net/blog/light/feed/"
                       "http://themaister.net/blog/feed/"
                       "http://marc-b-reynolds.github.io/feed.xml"
                       "http://blogs.msdn.com/marcelolr/rss.xml"
                       "http://blog.marmakoide.org/?feed=rss2"
                       "http://mynameismjp.wordpress.com/feed/"
                       "http://directtovideo.wordpress.com/feed/"
                       "http://deanoc.com/rss.xml"
                       "https://tuxedolabs.blogspot.com/feeds/posts/default?alt=rss"
                       "http://www.dimension3.sk/feed/"
                       "http://blog.mmacklin.com/feed/"
                       "http://allenchou.net/feed/"
                       "http://molecularmusings.wordpress.com/feed/"
                       "http://mmikkelsen3d.blogspot.com/feeds/posts/default"
                       "http://reedbeta.com/feed/"
                       "https://nlguillemot.wordpress.com/feed/"
                       "http://www.openglsuperbible.com/feed/"
                       "http://ourmachinery.com/index.xml"
                       "http://outerra.blogspot.com/feeds/posts/default"
                       "http://pzurita.wordpress.com/feed/"
                       "http://psgraphics.blogspot.com/feeds/posts/default"
                       "http://petersikachev.blogspot.com/feeds/posts/default?alt=rss"
                       "http://pixeljetstream.blogspot.com/feeds/posts/default"
                       "http://pointersgonewild.com/feed/"
                       "http://preshing.com/feed"
                       "http://prog21.dadgum.com/atom.xml"
                       "http://ventspace.wordpress.com/feed/"
                       "http://blog.shivoa.net/feeds/posts/default"
                       "http://chainedchaos31.tumblr.com/rss"
                       "http://www.randygaul.net/feed/"
                       "http://raytracey.blogspot.com/feeds/posts/default"
                       "http://www.realtimerendering.com/blog/feed/"
                       "http://realtimevoxels.blogspot.com/feeds/posts/default"
                       "http://richg42.blogspot.com/feeds/posts/default"
                       "http://msm.grumpybumpers.com/?feed=rss2"
                       "http://seblagarde.wordpress.com/feed/"
                       "http://www.palgorithm.co.uk/feed/"
                       "https://sandervanrossen.blogspot.com/feeds/posts/default"
                       "http://vec3.ca/feed/"
                       "http://simonschreibt.blogspot.com/feeds/posts/default"
                       "http://sjbrown.co.uk/index.xml"
                       "http://www.sophiehoulden.com/feed/"
                       "http://blog.selfshadow.com/feed/"
                       "http://blog.stevemcauley.com/feed/"
                       "http://steve-yegge.blogspot.com/feeds/posts/default"
                       "https://medium.com/feed/@Aprilw"
                       "https://medium.com/feed/@bgolus"
                       "https://medium.com/feed/@Esquiring"
                       "https://medium.com/feed/@steve.yegge"
                       "http://blog.hvidtfeldts.net/index.php/feed/"
                       "http://evincarofautumn.blogspot.com/feeds/posts/default"
                       "http://blog.demofox.org/feed/"
                       "http://kylehalladay.com/atom.xml"
                       "http://www.joshbarczak.com/blog/?feed=rss2"
                       "http://david.fancyfishgames.com/feeds/posts/default"
                       "http://blogs.msdn.com/oldnewthing/rss.xml"
                       "http://blogs.msdn.com/b/visualstudio/rss.aspx"
                       "http://the-witness.net/news/feed/"
                       "https://timothylottes.github.io/index.rss"
                       "http://grantland.com/contributors/tom-bissell/feed/"
                       "http://tomforsyth1000.github.io/blog.wiki.xml"
                       "http://tomhammersley.blogspot.com/feeds/posts/default"
                       "http://seven-degrees-of-freedom.blogspot.com/feeds/posts/default"
                       "http://sonnati.wordpress.com/feed/"
                       "https://blogs.msdn.microsoft.com/wsl/feed/"
                       "http://diaryofagraphicsprogrammer.blogspot.com/feeds/posts/default"
                       "http://xoofx.com/feed.xml"
                       "http://kayru.org/feed.xml"
                       "http://zrusin.blogspot.com/feeds/posts/default"
                       "http://zeuxcg.org/feed/"
                       "https://colinbarrebrisebois.com/feed/")
        
        elfeed-mac-connections 10
        url-queue-timeout 30)

  (defhydra elfeed-control-panel ()
    "elfeed"
    ("u" elfeed-update "update" :color blue)
    ("q" nil "quit" :color blue))
  
  :config
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'elfeed)
    (evil-collection-init)))

(use-package org :ensure t :pin gnu :disabled t
  :after flyspell
  
  :hooks
  (:org-mode-hook yas-minor-mode)
  
  :general
  ;; Global Org-mode fucntionality
  ("o" '(org-control-panel/body :which-key "Org"))
  
  (:keymaps 'org-mode-map
            :states  'normal
            :prefix   nil

            "C-M-k" 'org-cut-subtree

            "C-'"   'ace-window
            "C-#"   'helm-org-list-agenda-files
            
            "C-j"   'org-next-visible-heading
            "C-k"   'org-previous-visible-heading)
  
  :init
  (setq org-log-done                   'time ;; When completing a task, prompt for a closing note...
        org-log-reschedule             'time
        org-src-fontify-natively       t
        org-descriptive-links          t
        org-startup-with-inline-images t
        
        org-list-description-max-indent 0
        
        org-enforce-todo-dependencies  t
        org-enforce-todo-checkbox-dependencies t
        
        org-catch-invisible-edits      'error
        
        org-clock-persist              t
        
        org-hide-leading-stars         nil
        org-line-spacing               5
        org-tags-column                0 ;; Have tags next to the title
        
        org-babel-load-languages      '((sql . t)
                                        (shell . t)
                                        (plantuml . t))
        
        ;; Templates configuration
        org-capture-templates '(("t" "Task"  entry (file "~/Sandbox/planning/inbox.org") "* TODO %i%?"))
        
        ;; Keywords
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "ACTIVE" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)"))
        org-todo-keyword-faces '(("ACTIVE" . "yellow"))

        org-refile-use-outline-path 'file
        org-refile-targets '((org-agenda-files :maxlevel . 1))

        ;; NEW EXPERIMENTAL SETTINGS
        org-adapt-indentation          nil)
  
  (setq-mode-local org evil-auto-indent nil)
  
  (defhydra org-control-panel (:color blue :hint nil)
    "
  General            Agenda         Brain
-----------------------------------------
  _o_: Org Mode    _a_: Weekly    _b_: Org Brain
  _c_: Capture
 ----------------------------------------
"
    ("o" org-mode-control-panel/body)
    ("c" org-capture)
    ("a" org-agenda-list)
    ("b" brain-control-panel/body)
    
    ("q" nil "cancel"))
  
  (defhydra org-mode-control-panel (:color blue :hint nil)
    "
-------
| Org |  Brain
----------------------------------------
^Capturing^        ^Planning^     ^Timing^
----------------------------------------
_c_: Capture       _a_: Agenda    _t_: Timings
_l_: Store Link    ^ ^            _i_: Clock-in
_n_: Quick Note    ^ ^            _o_: Clock-out
----------------------------------------
"
    ("c" org-capture)
    ("l" org-store-link)
    ("n" org-make-quick-note)
    
    ;; Planning
    ("a" org-agenda)
    
    ;; Timings
    ("t" org-clocks-and-timers/body)
    ("i" org-clock-in)
    ("o" org-clock-out)
    
    ("q" nil "cancel"))
  
  (defhydra org-clocks-and-timers (:color blue :hint nil)
    "
^Clock:^ ^In/out^     ^Edit^   ^Summary^     | ^Timers:^ ^Run^           ^Insert
-^-^-----^-^----------^-^------^-^-----------|--^-^------^-^-------------^------
^ ^      _i_n         _e_dit   _g_oto entry  |  ^ ^      _r_elative      ti_m_e
^ ^      _c_ontinue   _q_uit   _d_isplay     |  ^ ^      cou_n_tdown     i_t_em
^ ^      _o_ut        ^ ^      _r_eport      |  ^ ^      _p_ause toggle
^ ^      ^ ^          ^ ^      ^ ^           |  ^ ^      _s_top
"
    ("i" org-clock-in)
    ("c" org-clock-in-last)
    ("o" org-clock-out)
    
    ("e" org-clock-modify-effort-estimate)
    ("q" org-clock-cancel)

    ("g" org-clock-goto)
    ("d" org-clock-display)
    ("r" org-clock-report)
    ("x" (org-info "Clocking commands"))

    ("r" org-timer-start)
    ("n" org-timer-set-timer)
    ("p" org-timer-pause-or-continue)
    ("s" org-timer-stop)

    ("m" org-timer)
    ("t" org-timer-item)
    ("z" (org-info "Timers")))

  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'outline))
  
  (defun org-make-quick-note (name)
    (interactive "B")
    (let ((note-path (concat org-quick-note-folder name ".org")))
      (unless (not (file-exists-p note-path))
        (write-region "" nil note-path))
      (find-file note-path)))
  
  (defun helm-org-list-agenda-files ()
    (interactive)
    (helm :sources (helm-build-sync-source "Org-mode Agenda Files:"
                     :candidates 'org-agenda-files
                     :fuzzy-match t
                     :action 'find-file)))
  
  :config
  ;; Since there's a default Org that comes with emacs, adding this dummy check to ensure that
  ;; whenever I'm using a fresh emacs installation i have the correct package installed
  (if (and (boundp 'org-version) (not (string= (substring org-version 0 1) "9")))
      (warn "WARNING :: Old Org-mode version is used (%s), check the configuration" org-version))
  
  (org-indent-mode -1)
  
  ;; Since this config depends on the runtime value, this should be configured in this section
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  
  (add-to-list 'org-structure-template-alist '("scala" "#+BEGIN_SRC scala \n\t?\n#+END_SRC"))

  ;; Configure hooks for clock's persistance
  (org-clock-persistence-insinuate)

  (use-package org-agenda :demand t
    :after org
    :init
    (setq
     org-agenda-files
     (f-files "~/Sandbox/Planning"
              (lambda (path)
                (not (s-starts-with-p "_" (f-filename path)))))
     
     org-agenda-custom-commands
     '(("c" . "My Custom Agendas")
       ("cu"  "Unscheduled"
        ((todo ""
               ((org-agenda-overriding-header "\nUnscheduled TODO")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))))
        nil
        nil))
     
     org-archive-location "./archives/%s_archive::"
     org-agenda-archives-mode t

     org-agenda-start-on-weekday 6 ;; Saturday
     org-agenda-include-diary nil
     org-agenda-span 'day
     org-agenda-skip-deadline-if-done t
     
     ;; Display agenda in full window
     org-agenda-window-setup 'current-window)
    
    :config
    (add-hook 'org-agenda-finalize-hook
              (lambda () (remove-text-properties
                          (point-min) (point-max) '(mouse-face t)))))

  (use-package evil-org
    :after (evil org)
    :hook (org-mode . evil-org-mode)
    
    :config
    (require 'evil-org-agenda)
    (add-hook #'org-agenda-mode-hook 'evil-org-agenda-set-keys) 
    (add-hook #'evil-org-mode 'evil-org-set-key-theme))

  (use-package org-ref :disabled t
    :after org
    
    :init
    (use-package helm-bibtex :ensure t :demand t :after helm)
    
    (setq reftex-default-bibliography '("~/Sandbox/Library/library.bib"))
    
    (setq bibtex-completion-bibliography "~/Sandbox/Library/library.org"
          bibtex-completion-library-path "~/Sandbox/Library"
          bibtex-completion-notes-path "~/Sandbox/Notes/")
    
    (setq org-ref-notes-directory "~/Sandbox/Notes"
          org-ref-bibliography-notes "~/Sandbox/Library/library.org"
          org-ref-default-bibliography '("~/Sandbox/Library/library.bib")))

  (use-package org-brain :disabled t
    :after org
    
    ;; :general
    ;; ("ob" '(org-brain-visualize :which-key "Brain"))
    
    :init
    (setq org-brain-path "~/Sandbox/Library/Notes"
          org-brain-visualize-default-choices 'all)
    
    (defhydra brain-control-panel (:color pink :hint nil)
      "
     ---------
 Org | Brain |
----------------------------------------
^Controls^  
----------------------------------------
_v_: Visualize       
----------------------------------------
"
      ("v" org-brain-visualize :color blue)
      ("q" nil "cancel"))
    
    (with-eval-after-load 'evil
      (evil-set-initial-state 'org-brain-visualize 'emacs))
    
    :config
    (push '("b" "Brain" plain #'org-brain-goto-end "* %i%?" :empty-lines 1) org-capture-templates))

  (use-package org-noter :disabled t
    :after org
    
    ;; :general
    ;; (:keymaps 'org-mode-map
    ;;  "on" 'org-noter)

    :init
    (setq org-noter-always-create-frame nil
          org-noter-notes-window-location 'vertical-split))
  
  )

(use-package markdown-mode :ensure t :disabled t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown"
        markdown-open-command "grip -b")

  :hooks (flyspell-mode))

(use-package yaml-mode :ensure t :disabled t)

(use-package plantuml-mode :ensure t :disabled t)

(use-package eshell
  :defines (eshell-visual-commands eshell-mode-hook)
  :functions (eshell/alias eshell/pwd eshell-cmpl-initialize)
  
  :init
  (defun 4lex1v:helm-eshell-history ()
    (eshell-cmpl-initialize)
    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
  
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'eshell))

  (defun git-prompt-branch-name ()
    "Get current git branch name"
    (let ((args '("symbolic-ref" "HEAD" "--short")))
      (with-temp-buffer
        (apply #'process-file "git" nil (list t nil) nil args)
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position))))))
  
  (defun 4lex1v:eshell-prompt ()
    (let ((branch-name (git-prompt-branch-name)))
      (concat
       "\n# " (user-login-name) " in " (abbreviate-file-name (eshell/pwd)) "\n"
       (if branch-name (format "git:(%s) >> " branch-name) ">> ")
       )))
  
  (setq eshell-prompt-function #'4lex1v:eshell-prompt
        eshell-prompt-regexp ".*>>+ ")
  
  :hooks
  (:eshell-mode-hook
   4lex1v:helm-eshell-history
   company-mode
   ansi-color-for-comint-mode-on)
  
  :config
  (with-eval-after-load 'em-term
    (add-to-list 'eshell-visual-commands "htop"))

  (with-eval-after-load 'company
    (add-hook 'eshell-mode-hook
              (lambda nil
                (make-local-variable 'company-backends)
                (setq company-backends
                      (remove nil
                              '(company-files company-dabbrev)))))))

(mapc
 (lambda (mode)
   (font-lock-add-keywords ;;`font-lock-keywords`
    mode
    '(("#\\<\\(TODO\\)\\>" 1 '(error :underline t) t)
      ("#\\<\\(NOTE\\)\\>" 1 '(warning :underline t) t))))
 '(emacs-lisp-mode lua-mode scala-mode c-mode objc-mode c++-mode rust-mode))

(setq gc-cons-threshold 1000000)

