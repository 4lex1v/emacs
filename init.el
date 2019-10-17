(setq gc-cons-threshold 10000000)

(defconst THEME_TO_LOAD 'default)

(defconst IS-MAC               (eq system-type 'darwin))
(defconst IS-WINDOWS           (eq system-type 'windows-nt))
(defconst IS-UNIX              (not IS-WINDOWS))
(defconst USER-EMACS-DIRECTORY (file-name-directory (or load-file-name (buffer-file-name))))
(defconst USER-INIT-FILE       (concat USER-EMACS-DIRECTORY "init.el"))

;; #TODO :: search fails if the buffer is opened and not at the beginning of the buffer
(defun 4l/edit-face-at-point ()
  "Editor face of the active theme at the point."
  (interactive)
  (-if-let* ((face-name (face-at-point))
             (theme-file-buffer (find-library (concat (symbol-name THEME_TO_LOAD) "-theme"))))
      (with-current-buffer theme-file-buffer
        (search-forward (symbol-name face-name)))))

(defun 4l/adjust-frame-size ()
  "Resets current frame width to 120 columns"
  (interactive)
  (if (not window-system)
      (error "Can only adjust a frame-based Emacs instance")
    (let ((desirable-size 120))
      (cl-loop
       for window in (window-list (selected-frame) nil)
       do (adjust-window-trailing-edge window (- desirable-size (window-width)) t)))))

(defun 4l/insert-block (end-with-semicolon-p)
  (interactive "P")
  (insert "{")
  (newline-and-indent)
  (newline)
  (insert (if end-with-semicolon-p "};" "}"))
  (beginning-of-line)
  (indent-according-to-mode)
  (previous-line)
  (indent-according-to-mode))

(setq-default
 auto-window-vscroll nil
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
 show-paren-delay               0.0
 ring-bell-function            'ignore
 tramp-default-method          "ssh"
 make-backup-files              nil
 auto-save-default              nil
 inhibit-startup-message        t
 initial-scratch-message        nil
 kill-do-not-save-duplicates    t
 ad-redefinition-action        'accept
 next-line-add-newlines         nil
 desktop-save-mode              nil
 desktop-save                   nil
 user-ref-name                 "4lex1v"
 mouse-wheel-scroll-amount     '(1)
 mouse-wheel-progressive-speed  nil

 ;; Performance related settings
 inhibit-compacting-font-caches t
 jit-lock-defer-time 0
 fast-but-imprecise-scrolling t

 default-directory              "~/Sandbox"
 search-upper-case              nil
 safe-local-variable-values (quote ((user-ref-name . aivanov))))

(if IS-MAC
    (progn
      (setq
       browse-url-browser-function 'browse-url-default-macosx-browser
       delete-by-moving-to-trash    t
       mac-command-modifier        'meta
       mac-option-modifier         'super
       mac-control-modifier        'control
       ns-function-modifier        'hyper
       ns-use-native-fullscreen     t
       frame-resize-pixelwise       t
       shell-file-name              "/bin/sh")
      (global-set-key (kbd "M-`") #'other-frame)
      ))

(if IS-WINDOWS
    (setq
     shell-file-name "c:/Users/Aleksandr/scoop/apps/pwsh/current/pwsh.exe"))

(if (display-graphic-p)
    (progn
      (tooltip-mode          -1)
      (tool-bar-mode         -1)
      (scroll-bar-mode       -1)

      (eval-and-compile
        (add-to-list 'load-path "/Users/aleksandrivanov/.emacs.d/themes/sirthias")
        (add-to-list 'load-path "/Users/aleksandrivanov/.emacs.d/themes/paladin"))
      
      (let ((font-setting "PragmataPro-16:antialias=subpixel"))
        (add-to-list 'default-frame-alist (cons 'font font-setting))
        (set-frame-font font-setting))

      (cond ((eq THEME_TO_LOAD 'sirthias)
             (when (locate-library "sirthias-theme")
               (setq sirthias-easy-mode t sirthias-cold-mode nil)
               (add-hook 'after-make-frame-functions
                         (lambda (frame)
                           (select-frame frame)
                           (load-theme 'sirthias t)))
               (require 'sirthias-theme nil nil)
               (load-theme 'sirthias t)))
            ((eq THEME_TO_LOAD 'paladin)
             (when (locate-library "paladin-theme")
               (setq paladin-easy-mode t paladin-cold-mode nil)
               (add-hook 'after-make-frame-functions
                         (lambda (frame)
                           (select-frame frame)
                           (load-theme 'paladin t)))
               (require 'paladin-theme nil nil)
               (load-theme 'paladin t)))
            ((eq THEME_TO_LOAD 'default)
             (progn 
               (set-face-attribute 'fringe nil :background nil)
               (with-eval-after-load "eshell"
                 (lambda ()
                   (set-face-attribute 'eshell-prompt nil :foreground "#000080"))))))))

(blink-cursor-mode     -1)
(show-paren-mode       -1)
(menu-bar-mode         -1)
(delete-selection-mode t)
(global-auto-revert-mode t)

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(fset 'yes-or-no-p   'y-or-n-p)

(require 'abbrev)

(when (and (require 'ls-lisp) (require 'dired))
  (setq
   ls-lisp-dirs-first t
   ls-lisp-use-insert-directory-program nil))

(require 'mode-local)
(setq-mode-local scala-mode comment-note-comment-prefix "//")
(setq-mode-local org evil-auto-indent nil)
(setq-mode-local emacs-lisp-mode comment-note-comment-prefix ";;")

 (setq
 package-enable-at-startup nil
 package--init-file-ensured t
 package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")))

;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(defun package--save-selected-packages (&rest opt) nil)
(require 'package) 
(package-initialize)

;; Use-Package
(add-to-list 'load-path (expand-file-name "modules/use-package" USER-EMACS-DIRECTORY))

(setq
 use-package-verbose               t
 use-package-always-defer          t
 use-package-enable-imenu-support  t
 use-package-check-before-init     t
 use-package-minimum-reported-time 0.1

 ;; Only when the config is stable
 use-package-expand-minimally t)

(require 'bind-key)
(require 'use-package)

(use-package upe-hooks :demand t :load-path "modules/upe-hooks")
(use-package diminish :ensure t :demand t)
(use-package s    :ensure t :demand t) ;; Strings manipulation library
(use-package f    :ensure t :demand t) ;; Files manipulation library
(use-package dash :ensure t :demand t) ;; List manipulation library

(when (require 'hideshow)
  (add-hook 'text-mode-hook #'hs-minor-mode)
  (add-hook 'prog-mode-hook #'hs-minor-mode))

(use-package exec-path-from-shell :ensure t :demand t
  :commands (exec-path-from-shell-getenv
             exec-path-from-shell-setenv)
  :init
  ;; Under certain conditions this can be nicely used withing Windows environment as well...
  (defun run-shell-command (&rest cmd)
    (replace-regexp-in-string "\r?\n\\'" ""
                              (shell-command-to-string
                               (mapconcat 'identity cmd " ")))) 
  
  (defun register-path-folders (&rest paths)
    (declare (indent 1))
    (let ((path (-reduce-r-from
                 (lambda (value acc) (format "%s:%s" value acc))
                 (exec-path-from-shell-getenv "PATH")
                 paths)))
      (exec-path-from-shell-setenv "PATH" path)))

  :config
  (if IS-MAC
      (progn
        (exec-path-from-shell-setenv "HOMEBREW_PREFIX" "/usr/local")
        (exec-path-from-shell-setenv "HOMEBREW_CELLAR" "/usr/local/Cellar")
        (exec-path-from-shell-setenv "GTAGSCONF" "/usr/local/share/gtags/gtags.conf")
        (exec-path-from-shell-setenv "GTAGSLABEL" "ctags")
        (register-path-folders "/usr/local/opt/llvm/bin" "/usr/local/homebrew/bin" "/usr/local/bin")))

  (if IS-WINDOWS
      (progn
        (exec-path-from-shell-setenv "SHELL" "c:/Users/Aleksandr/scoop/apps/pwsh/current/pwsh.exe"))))

(use-package general :demand t :load-path "modules/general"
  :init
  (setq general-default-states  'normal
        general-default-prefix  "<SPC>")
  
  :config
  (with-eval-after-load 'evil
    (general-evil-setup t)))

(use-package evil :load-path "modules/evil" :demand t
  :after general ;; To enable evil-leader in initial buffers
  
  :defines (evil-hook)
  :functions (prog-mode-hook)
  
  :preface
  (setq 
   evil-want-integration           nil
   evil-collection-company-use-tng nil)
  
  :general
  (:prefix nil :states nil :keymaps 'evil-motion-state-map
   "j"   'next-line
   "k"   'previous-line)

  (:prefix nil
   "$"   'evil-end-of-visual-line
   "C-j" 'evil-forward-paragraph
   "C-k" 'evil-backward-paragraph
   "g,"  'evil-jump-backward
   "g."  'find-function-at-point
   "zl"  'hs-hide-level

   "<M-wheel-up>" 'text-scale-increase
   "<M-wheel-down>" 'text-scale-decrease

   ;; Navigation keys
   "C-S-o" #'evil-jump-forward)

  (:states  'normal
   :keymaps 'global-map

   "e"   '(:ignore t :wk "Emacs")
   "eq"  'save-buffers-kill-emacs
   "er"  'revert-buffer
   "eb"  'bookmark-bmenu-list

   "ee"  '(:ignore t :wk "Evil")
   "een" '(evil-ex-nohighlight :wk "No Highlighting")
   "et"  '(:ignore t :wk "Toggles")
   "etl" 'toggle-truncate-lines

   "f"   '(:ignore t :wk "Files")
   "fe"  'eshell
   "fi"  `((lambda () (interactive) (find-file USER-INIT-FILE)) :wk "init.el")
   "ff"  '(helm-find-files :wk "Files")
   "fd"  `((lambda () (interactive) (helm-find-files-1 "~/Dropbox/")) :wk "Dropbox")
   "fs"  `((lambda () (interactive) (helm-find-files-1 "~/Sandbox/")) :wk "Sandbox")
   "fp"  `((lambda () (interactive) (helm-find-files-1 "~/Sandbox/Projects/")) :wk "Projects")
   "fw"  `((lambda () (interactive) (helm-find-files-1 "~/Sandbox/Work/")) :wk "Work")

   "fl"  '(find-library :wk "Find Library")

   "s"   '(:ignore t :wk "Services"))

  (:prefix   nil
   :states  'normal
   :keymaps 'global-map 

   "C-q" #'((lambda (&optional arg)
              (interactive "P")
              (kill-buffer (current-buffer))
              (if (and (not (equal arg 'nil))
                       (> (count-windows) 1))
                  (delete-window)))
            :wk "close-buffer")

   "M-q" #'((lambda ()
              (interactive)
              (if (> (count-windows) 1) 
                  (progn
                    (other-window 1)
                    (kill-buffer (current-buffer))
                    (delete-window))))
            :wk "close-other-window"))

  (:states  '(normal insert)
   :keymaps 'global-map
   :prefix   nil

   "C-;"    'comment-line
   "C-x \\" 'align-regexp
   "C-c r"  'revert-buffer
   "M-j"    'join-line
   "M-o"    '(lambda (arg)
               (interactive "p")
               (end-of-line)
               (open-line arg)
               (next-line 1)
               (indent-according-to-mode))
   "M-["    #'(lambda () (interactive) (4l/insert-block nil))
   "M-{"    #'(lambda () (interactive) (4l/insert-block t))
   "C-S-d"  '4l/duplicate-line
   "C-w i"  '(clone-indirect-buffer-other-window :wk "Indirect Buffer"))

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
                                      help
                                      info
                                      log-view
                                      man
                                      simple
                                      ,@(when evil-collection-setup-minibuffer '(minibuffer))
                                      (package-menu package)
                                      (term term ansi-term multi-term)))

    :config
    (add-hook 'after-init-hook
              (lambda () (evil-collection-init))))

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
    (define-key evil-normal-state-map "K" 'evil-jump-out-args)))

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

(use-package smartparens :ensure t :pin melpa-stable
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
  ((conf-mode text-mode prog-mode) . smartparens-mode)
  
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
  
  :config
  (use-package smartparens-config :demand t)
  (sp-use-smartparens-bindings)
  
  (sp-pair "(" ")"   :wrap "C-(")
  (sp-pair "[" "]"   :wrap "s-[") ;; This one doesn't work as expected
  (sp-pair "\"" "\"" :wrap "C-\"")
  
  (sp-pair "{" "}"   :wrap "C-{"))

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

  :hook
  ((prog-mode) . yas-minor-mode)
  
  :general
  ("es" '(hydra-yasnippet/body :wk "Snippets"))
  
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")
        yas-wrap-around-region t
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  
  (add-hook 'after-save-hook
            (lambda ()
              (when (eq major-mode 'snippet-mode)
                (yas-reload-all))))
  
  (with-eval-after-load 'hydra
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
    ("a" yas-reload-all)))
  
  :config
  (yas-reload-all))

(use-package helm :demand t :ensure t :pin melpa-stable
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
   :keymaps 'helm-find-files-map
   :states   nil

   "C-<backspace>"   'backward-kill-word
   "C-d"             '(lambda ()
                        (interactive)
                        (helm-exit-and-execute-action
                         'helm-point-file-in-dired)))

  (:prefix   nil
   :keymaps 'helm-map
   :states   nil
   
   "<tab>" 'helm-execute-persistent-action
   "C-i"   'helm-execute-persistent-action
   "C-z"   'helm-select-action
   "C-o"   'helm-next-source
   "C-j"   'helm-next-line
   "C-k"   'helm-previous-line
   "C-f"   'helm-toggle-full-frame)

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
        helm-follow-mode-persistent            t
        helm-show-completion-display-function  nil
        helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  
  (use-package async :ensure t)
  (use-package helm-config :demand t)
  
  :config 
  (use-package helm-mode :demand t)
  
  (helm-autoresize-mode)

  (substitute-key-definition 'find-tag 'helm-etags-select global-map))

(use-package projectile :demand t :ensure t :pin melpa-stable 
  :diminish projectile-mode
  :commands projectile-project-root

  :general
  (:prefix nil
   "M-1" 'projectile-find-file
   "M-!" 'projectile-run-shell-command-in-root)      
  
  ;;  Projectile-only bindings
  ("p" '(:ignore t :wk "Projectile")
   "pp" 'projectile-switch-project
   "pk" 'projectile-kill-buffers
   "pr" 'projectile-replace
   "pi" 'projectile-invalidate-cache
   "pe" 'projectile-run-eshell
   "p&" 'projectile-run-async-shell-command-in-root
   "pS" 'projectile-save-project-buffers
   "ps" '((lambda (arg)
            (interactive "P")
            (helm-grep-ag (projectile-project-root) arg))
          :wk "Search"))
  
  :init
  (setq projectile-enable-caching       t
        projectile-completion-system   'helm
        projectile-require-project-root t
        projectile-use-git-grep         nil
        projectile-mode-line            '(:eval (format " {%s}" (projectile-project-name))))

  :config
  (projectile-mode))

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

(use-package avy :ensure t
  :general
  (:keymaps 'global :states 'normal
   "jj" 'avy-goto-char
   "jl" 'avy-goto-line))

(use-package magit :ensure t :pin melpa-stable
  :commands (magit magit-status magit-diff-range magit-clone)
  
  :general 
  ("g" '(:ignore t :wk "Magit")
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
   "gl" '(:ignore t :wk "Logging")
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
  (use-package ghub :ensure t :disabled t :after magit)
  (use-package with-editor :ensure t :after magit
    :general
    (:keymaps 'with-editor-mode-map
              :prefix "" ;; don't use SPC prefix in this case
              "RET"    'with-editor-finish
              [escape] 'with-editor-cancel)
    :config
    (with-eval-after-load 'evil
      (evil-set-initial-state 'with-editor-mode 'insert)))
  (use-package magit-popup :ensure t :after magit)

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



  (use-package evil-magit :ensure t :demand t
    :after (evil magit-mode)
    :commands evil-magit-init
    :config
    (evil-magit-init)))

(use-package hydra :load-path "modules/hydra" :demand t :disabled t
  :general
  (:prefix nil
           
   "<f2>" 'hydra-zoom/body)

  (:prefix nil
   :states 'normal

   "C-e"  'hydra-error/body)
  
  :config
  (let ((hydras-file-path "~/.emacs.d/hydras.el"))
    (if (f-exists-p hydras-file-path)
        (load hydras-file-path)))

  (with-eval-after-load 'hydra
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
      
      ("q" nil "quit"))))

(use-package company :load-path "modules/company" :disabled t
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

  :hook ((text-mode prog-mode) . company-mode)
  
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

(use-package elisp-mode
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode        (("\\.el$" . emacs-lisp-mode)
                ("Cask"   . emacs-lisp-mode))
  
  :defines (emacs-lisp-mode-hook)
  
  :general
  (:prefix nil
   "M-."     'find-function-at-point
   "M-,"     'find-variable-at-point
   "C-c e r" 'eval-region)
  
  (:keymaps 'emacs-lisp-mode-map
   "e"  '(:ignore t :wk "Emacs")
   "ev" '(:ignore t :wk "Describe Variable")
   "ed" '(:ignore t :wk "Docs & Help")
   "eda" #'helm-apropos)
  
  :config
  

  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'elisp-mode))

  (with-eval-after-load 'company
    (configure-company-backends-for-mode emacs-lisp-mode
      '(company-elisp company-capf company-files company-yasnippet)))

  (with-eval-after-load 'smartparens
    (sp-with-modes 'emacs-lisp-mode
      (sp-local-pair "'" nil :actions nil)))

  (use-package macrostep :ensure t
    :commands macrostep-expand
    :mode ("\\*.el\\'" . emacs-lisp-mode)
    
    :general
    (:keymaps 'macrostep-keymap
     :prefix   nil
     "q" #'macrostep-collapse-all
     "e" #'macrostep-expand)

    (:keymaps 'emacs-lisp-mode-map
     "em" #'macrostep-expand)
    
    :config
    (with-eval-after-load 'evil-collection
      (add-to-list 'evil-collection-mode-list 'macrostep))))

(use-package rye-lang-mode
  :load-path "modules/rye"
  :mode "\\.rye\\'")

(use-package scala-mode :ensure t
  :mode        ("\\.\\(scala\\|sbt\\|sc\\)\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  
  :hooks
  (4l/fix-scala-fonts)
  
  :general
  (:keymaps 'scala-mode-map
   "s" '(:ignore t :wk "Scala"))
  
  (:keymaps 'scala-mode-map
   :states  '(normal insert)
   :prefix   nil
   
   "<C-return>"     #'newline-or-comment)

  (:keymaps 'scala-mode-map
   :states  'normal
   :prefix   nil
   
   "J" '(lambda () (interactive) (scala-indent:join-line t)))

  :init
  (setq
   scala-indent:use-javadoc-style t
   scala-mode:debug-messages nil)

  (defun 4l/fix-scala-fonts ()
    (interactive)
    (mapc
     (lambda (kw)
       (let ((face-ref (intern (format "scala-font-lock:%s-face" kw))))
         (copy-face font-lock-keyword-face face-ref)))
     '("final" "private" "protected" "implicit" "abstract" "sealed" "lazy" "override" "inline")))

  :config
  (with-eval-after-load 'company
    (configure-company-backends-for-mode scala-mode
      '(company-dabbrev
        company-keywords
        company-capf
        company-yasnippet
        company-files)))
  
  (use-package sbt-mode :ensure t
    :general
    (:keymaps 'scala-mode-map :prefix "<SPC> sb"
     ""  '(:ignore t :wk "SBT")
     "b" '(4lex1v:open-sbt-build-file :wk "build.sbt")
     "s" 'sbt-start
     "r" 'sbt-command
     "c" '((lambda () (interactive) (sbt-command "compile")) :wk "compile")
     "t" '((lambda () (interactive) (sbt-command "test")) :wk "test")
     )
    
    (:keymaps 'scala-mode-map :prefix ","
     "c" '((lambda () (interactive) (sbt-command "compile")) :wk "compile")
     "t" '((lambda () (interactive) (sbt-command "test")) :wk "test")
     "r" 'sbt-run-previous-command
     "i" '4l/open-in-intellij)

    :init
    (setq sbt:prompt-regexp  "^\\(\\(scala\\|\\[[^\]]*\\] \\)?[>$]\\|[ ]+|\\)[ ]*")
    :config
    (add-to-list 'sbt:program-options "-Dsbt.supershell=false")))

(use-package cc-mode
  :mode (("\\.h\\'"  . c++-mode)
         ("\\.glsl\\'" . c++-mode)
         ("\\.mm\\'" . objc-mode))
  
  :commands c-toggle-auto-newline
  :defines (c-mode-common-hook)
  
  :general
  (:keymaps 'c-mode-base-map
   "m"  '(:ignore t :wk "Native")
   "ma" 'projectile-find-other-file
   "mA" 'projectile-find-other-file-other-window)
  
  ;; #TODO :: Check if this could be defined in the global configuration or it needs to be overriden for these modes?
  (:keymaps 'c-mode-base-map  
   :prefix nil
   "C-S-j" #'next-error
   "C-S-k" #'previous-error
   ",r"    #'recompile
   ",m"    #'helm-semantic-or-imenu)
  
  :init
  (defconst 4l/c-lang-style ;; added later under the label '4l'
    '((c-basic-offset . 2)
      ;; add alignment in || and && expressions
      ;;      b32 result = ((value >= 'A') && (value <= 'Z')) ||
      ;;                   ((value >= 'a') && (value <= 'z'));
      ;; instead of
      ;;         b32 result = ((value >= 'A') && (value <= 'Z')) ||
      ;;           ((value >= 'a') && (value <= 'z'));
      (c-offsets-alist . ((innamespace . [0])
                          (case-label . +)
                          (substatement-open . 0)))))

  (setq
   win32-system-include-paths '("c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/shared"
                                "c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/ucrt"
                                "c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/um"
                                "c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/winrt")
   c-default-style "4l")
  
  :config
  (c-add-style "4l" 4l/c-lang-style)
  
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
    (sp-local-pair 'c++-mode "{" nil
                   :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))
  
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(C . t)))

  (use-package cmake-mode :ensure t :disabled t
    :after cc-mode
    
    :init
    (with-eval-after-load 'evil-collection
      (add-to-list 'evil-collection-mode-list 'cmake-mode))
    
    :config
    (with-eval-after-load 'company
      (configure-company-backends-for-mode cmake-mode
        '(company-cmake company-files company-dabbrev company-capf)))))

(use-package rust-mode :ensure t
  :hooks (cargo-minor-mode)
  
  :init 
  (setq
   rust-indent-offset  2
   rust-format-on-save nil)
  
  :config
  (use-package smartparens-rust
    :after (rust-mode smartparens-mode)
    :config
    (add-hook 'rust-mode #'smartparens-rust)
    (sp-with-modes 'rust-mode
      (sp-local-pair "(" nil :post-handlers '(("||\n[i]" "RET")))
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))))
  

  (use-package cargo :ensure t
    :after rust-mode
    
    :general
    (:prefix ","
     :keymaps 'rust-mode-map
     
     "c" '(:ignore t :wk "Cargo")
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

  (use-package toml-mode :ensure t
    :mode ("/\\(Cargo.lock\\|\\.cargo/config\\)\\'" . toml-mode)))

(use-package ssh-agency :if IS-WINDOWS :ensure t
  :after magit
  :commands ssh-agency-ensure
  :init
  (setq
   ssh-agency-keys (list (expand-file-name "~/.ssh/github_rsa") (expand-file-name "~/.ssh/bamtech"))
   ssh-agency-add-executable "c:/WINDOWS/System32/OpenSSH/ssh-add.exe"
   ssh-agency-agent-executable "c:/WINDOWS/System32/OpenSSH/ssh-agent.exe")  

  :config
  (setenv "SSH_ASKPASS" "git-gui--askpass")
  (ssh-agency-ensure))

(use-package org 
  :hooks
  (:org-mode-hook yas-minor-mode)
  
  :general
  ;; Global Org-mode fucntionality
  ("o" '(org-control-panel/body :wk "Org"))
  
  (:keymaps 'org-mode-map
   :states  'normal
   :prefix   nil

   "C-M-k" 'org-cut-subtree

   "C-'"   'ace-window
   "C-#"   'helm-org-list-agenda-files
   
   "C-j"   'org-next-visible-heading
   "C-k"   'org-previous-visible-heading)

  (:keymaps 'org-agenda-mode-map
   :states  'motion
   :prefix   nil

   "j"  'org-agenda-next-line
   "k"  'org-agenda-previous-line
   "gj" 'org-agenda-next-item
   "gk" 'org-agenda-previous-item

   "gr" 'org-agenda-redo
   "gR" 'org-agenda-redo-all

   "z" 'org-agenda-view-mode-dispatch

   "sc" 'org-agenda-filter-by-category
   "sr" 'org-agenda-filter-by-regexp
   "se" 'org-agenda-filter-by-effort
   "st" 'org-agenda-filter-by-tag
   "s^" 'org-agenda-filter-by-top-headline
   "ss" 'org-agenda-limit-interactively
   "S" 'org-agenda-filter-remove-all

   "." 'org-agenda-goto-today
   "gc" 'org-agenda-goto-calendar
   "gC" 'org-agenda-convert-date
   "gd" 'org-agenda-goto-date
   "gh" 'org-agenda-holidays
   "gm" 'org-agenda-phases-of-moon
   "gs" 'org-agenda-sunrise-sunset
   "gt" 'org-agenda-show-tags

   "p" 'org-agenda-date-prompt
   "P" 'org-agenda-show-the-flagging-note)
  
  :init
  (setq
   org-log-done                  'time ;; When completing a task, prompt for a closing note...
   org-log-reschedule            'time
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
   
   ;;org-babel-load-languages      '((sql . t) (shell . t) (plantuml . t))
   org-babel-C-compiler "clang"
   org-babel-C++-compiler "clang++"
   
   ;; Templates configuration
   org-capture-templates '(("t" "Task"  entry (file "~/Sandbox/Library/Org/universe.org") "* TODO %i%?"))
   
   ;; Keywords
   org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "ACTIVE" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)"))
   org-todo-keyword-faces '(("ACTIVE" . "yellow"))

   org-refile-use-outline-path 'file
   org-refile-targets '((org-agenda-files :maxlevel . 1))
   org-refile-target-verify-function #'(lambda () (member "project" (org-get-local-tags)))

   ;; NEW EXPERIMENTAL SETTINGS
   org-adapt-indentation nil
   org-agenda-files (f-files "~/Sandbox/Library/Org" (lambda (path)
                                                       (and (f-ext? path "org")
                                                            (not (s-starts-with-p "_" (f-filename path))))))
   
   org-agenda-custom-commands '(("c" . "My Custom Agendas")
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
  
  (with-eval-after-load 'hydra
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
      ("z" (org-info "Timers"))))

  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'outline))
  
  (with-eval-after-load 'evil
    (use-package evil-org :demand t :after org
      :load-path "modules/evil-org-mode"
      :hook (org-mode . evil-org-mode)
      :config
      (require 'evil-org-agenda)
      (evil-set-initial-state 'org-agenda-mode 'motion)
      (evil-org-agenda-set-keys)))

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
  (org-indent-mode -1)
  
  ;; Since this config depends on the runtime value, this should be configured in this section
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  
  (add-to-list 'org-structure-template-alist '("scala" "#+BEGIN_SRC scala \n\t?\n#+END_SRC"))

  ;; Configure hooks for clock's persistance
  (org-clock-persistence-insinuate)

  (add-hook 'org-agenda-finalize-hook
            (lambda () (remove-text-properties
                        (point-min) (point-max) '(mouse-face t)))))

(use-package markdown-mode :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown"
        markdown-open-command "grip -b"))

(use-package yaml-mode :ensure t)

(use-package eshell
  :defines (eshell-visual-commands eshell-mode-hook)
  :functions (eshell/alias eshell/pwd eshell-cmpl-initialize)
  
  :init
  (defun 4lex1v:helm-eshell-history ()
    (eshell-cmpl-initialize)
    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))

  (defun eshell/clear ()
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  
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
   ansi-color-for-comint-mode-on)
  
  :config
  (with-eval-after-load 'em-term
    (add-to-list 'eshell-visual-commands "htop"))

  (use-package em-alias
    :if IS-MAC
    :config
    (eshell/alias "bubu" "brew update && brew upgrade")
    (eshell/alias "sshs" "ssh-add ~/.ssh/github_rsa"))

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


