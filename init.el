(setq gc-cons-threshold (* 50 1024 1024))

(defconst USER-EMACS-DIRECTORY (file-name-directory (or load-file-name (buffer-file-name))))
(defconst USER-INIT-FILE       (concat USER-EMACS-DIRECTORY "init.el"))

(defconst IS-MAC               (eq system-type 'darwin))
(defconst IS-WINDOWS           (eq system-type 'windows-nt))
(defconst IS-UNIX              (not IS-WINDOWS))

;; Frame configuration
(let ((font-setting "Iosevka SS08 Slab LtEx-16"))
  ;; TODO: create a window in the middle of th screen 
  (add-to-list 'initial-frame-alist (cons 'font font-setting))
  (setq default-frame-alist initial-frame-alist)
  ;; Would reload active fonts on eval
  (set-frame-font font-setting))

(add-to-list 'load-path (expand-file-name "themes/sirthias" USER-EMACS-DIRECTORY))
(when (locate-library "sirthias-theme")
  (setq sirthias-easy-mode t sirthias-cold-mode nil)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (load-theme 'sirthias t)))
  (require 'sirthias-theme nil nil)
  (load-theme 'sirthias t))

(setq-default
 auto-window-vscroll nil
 truncncate-lines    nil
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
 line-spacing 2

 case-fold-search nil
 case-replace     nil)

(setq
 inhibit-startup-message        t
 initial-scratch-message        nil
 initial-buffer-choice          "~/Dropbox/Sandbox/Library/Org/scratch.org"
 show-paren-delay               0.0
 ring-bell-function            'ignore
 tramp-default-method          "ssh"
 make-backup-files              nil
 auto-save-default              nil
 kill-do-not-save-duplicates    t
 ad-redefinition-action        'accept
 next-line-add-newlines         nil
 desktop-save-mode              nil
 desktop-save                   nil
 user-ref-name                 "4lex1v"
 mouse-wheel-scroll-amount     '(1)
 mouse-wheel-progressive-speed  nil

 ;; EXPERIMENTAL: Aparently this may help stopping Emacs to shit into my init.el
 ;; Found here - https://emacs.stackexchange.com/questions/55018/init-el-and-trampling-of-custom-set-variables
 custom-file (concat USER-EMACS-DIRECTORY "custom.el")
 
 ;; Performance related settings
 inhibit-compacting-font-caches t
 jit-lock-defer-time 0
 fast-but-imprecise-scrolling t

 default-directory              "~/Sandbox/"
 search-upper-case              nil
 safe-local-variable-values    (quote ((user-ref-name . aivanov)))

 dabbrev-case-distinction nil)

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
       frame-resize-pixelwise       t)
      (global-set-key (kbd "M-`") #'other-frame)))

(tooltip-mode          -1)
(tool-bar-mode         -1)
(scroll-bar-mode       -1)
(blink-cursor-mode     -1)
(show-paren-mode       -1)
(menu-bar-mode         -1)

(delete-selection-mode t)
(global-auto-revert-mode t)

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(fset 'yes-or-no-p   'y-or-n-p)

;; To avoid acidental hits on the touchpad
(dolist (binding '("<mouse-1>" "<C-mouse-1>" "<C-down-mouse-1>"))
  (define-key global-map (kbd binding)
    (lambda nil (interactive) nil)))

(defun 4l/swap-two-windows ()
  (interactive)
  (if (not (eq (length (window-list)) 2))
      (error "Can swap only two windows for now...")
    (let* ((windows-l (window-list))
           (left-window (car windows-l))
           (lw-buffer (window-buffer left-window))
           (right-window (cadr windows-l))
           (rw-buffer (window-buffer right-window)))
      (set-window-buffer left-window rw-buffer)
      (set-window-buffer right-window lw-buffer))))

;; (define-key global-map (kbd "C-,") #'4l/swap-two-windows)

(defun 4l/close-buffer (&optional arg)
  (interactive "P")
  (kill-buffer (current-buffer))
  (if (and (not (equal arg 'nil))
           (> (count-windows) 1))
      (delete-window)))

(defun 4l/close-other-window ()
  (interactive)
  (if (> (count-windows) 1) 
      (progn
        (other-window 1)
        (kill-buffer (current-buffer))
        (delete-window))))

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

;; (define-key global-map (kbd "M-[") (lambda () (interactive) (4l/insert-block nil)))
;; (define-key global-map (kbd "M-{") (lambda () (interactive) (4l/insert-block t)))

(setq
 dabbrev-case-replace t
 dabbrev-case-fold-search t
 dabbrev-upcase-means-case-search t)

(abbrev-mode 1)

(when (and (require 'ls-lisp) (require 'dired))
  (setq
   ls-lisp-dirs-first t
   ls-lisp-use-insert-directory-program nil))

(setq
 package-enable-at-startup nil
 package-check-signature nil
 package--init-file-ensured t
 package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                    ("gnu"          . "http://elpa.gnu.org/packages/")))

;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(defun package--save-selected-packages (&rest opt) nil)
(require 'package) 
(package-initialize)

(require 'epa-file)
(epa-file-enable)

(package-install 'use-package)

(setq
 use-package-verbose               t
 use-package-always-defer          t
 use-package-enable-imenu-support  t
 use-package-check-before-init     t
 use-package-minimum-reported-time 0.1

 ;; Only when the config is stable
 use-package-expand-minimally t)

(use-package diminish :ensure t)
(use-package s        :ensure t :demand t) ;; Strings manipulation library
(use-package f        :ensure t :demand t) ;; Files manipulation library
(use-package dash     :ensure t :demand t) ;; List manipulation library

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

(general-define-key
 :keymaps 'prog-mode
 :prefix nil
 :states 'normal
 ",." #'call-last-kbd-macro)

(use-package evil :ensure t :pin melpa-stable :demand t :disabled t
  :after general ;; To enable evil-leader in initial buffers
  
  :defines (evil-hook)
  :functions (prog-mode-hook)
  
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

           "<M-wheel-up>" 'text-scale-increase
           "<M-wheel-down>" 'text-scale-decrease

           ;; Navigation keys
           "C-S-o" #'evil-jump-forward)

  (:prefix   nil
             :states  'normal
             :keymaps 'global-map 

             "C-q" #'(4l/close-buffer :wk "close-buffer")
             "M-q" #'(4l/close-other-window :wk "close-other-window"))

  (:prefix   nil
             :states  '(normal insert)
             :keymaps 'global-map

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
             "C-w i"  '(clone-indirect-buffer-other-window :wk "Indirect Buffer")
             "C-w ,"  '(4l/swap-two-windows :wk "Swap Windows"))

  :init
  (setq-default evil-kill-on-visual-paste nil)

  (setq
   evil-default-state              'normal
   evil-default-cursor             t
   evil-want-C-u-scroll            t
   evil-want-keybinding            nil
   evil-want-integration           t
   evil-ex-substitute-global       t
   evil-ex-search-vim-style-regexp t
   evil-ex-interactive-search-highlight 'selected-window
   evil-collection-company-use-tng nil)

  ;; #NOTE :: This makes things like `just_an_example' selectable as a single word
  (defun fix-word-def () (modify-syntax-entry ?_ "w"))
  (add-hook #'prog-mode-hook 'fix-word-def)

  (use-package undo-tree :ensure t :demand t
    :diminish undo-tree-mode
    :commands global-undo-tree-mode
    :general
    (:prefix nil :states 'normal
             "M-/" 'undo-tree-visualize)
    
    (:prefix nil :keymaps 'undo-tree-visualizer-mode-map :states  '(motion)
             "j" 'undo-tree-visualize-redo
             "k" 'undo-tree-visualize-undo
             "l" 'undo-tree-visualize-switch-branch-right
             "h" 'undo-tree-visualize-switch-branch-left)
    
    :config (global-undo-tree-mode))
  
  :config
  (evil-set-initial-state 'prog-mode        'normal)
  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'comint-mode      'normal)
  (evil-set-initial-state 'org-mode         'normal)

  (evil-set-initial-state 'package-menu-mode 'motion)

  (evil-set-initial-state 'rg 'emacs)
  
  (evil-ex-define-cmd "e[val]" #'eval-buffer)
  (evil-ex-define-cmd "we" #'(lambda () (interactive) (save-buffer) (eval-buffer)))
  
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode)

  (use-package evil-collection :ensure t :demand t
    :after evil
    :commands evil-collection-init
    :init
    (setq
     evil-collection-setup-minibuffer nil
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
(use-package which-key :demand t :ensure t :pin melpa-stable
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.8
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
  (:keymaps 'smartparens-mode-map :prefix nil :states '(normal insert)
            "M-t"   'sp-transpose-sexp
            "C-M-k" 'sp-kill-sexp
            "C-M-w" 'sp-copy-sexp)

  (:prefix nil :keymaps 'smartparens-mode-map :states '(insert)
           "<C-backspace>" 'sp-backward-kill-word)
  
  :init
  (setq sp-base-key-bindings nil
        sp-autoinsert-if-followed-by-word t
        sp-autoskip-closing-pair 'always-end
        sp-hybrid-kill-entire-symbol nil)
  
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  
  (sp-pair "(" ")"   :wrap "C-(")
  (sp-pair "[" "]"   :wrap "s-[") ;; This one doesn't work as expected
  (sp-pair "\"" "\"" :wrap "C-\"")
  
  (sp-pair "{" "}"   :wrap "C-{"))

(use-package yasnippet :ensure t :pin melpa-stable
  :diminish (yas-minor-mode . " Y")

  :hook
  ((prog-mode org-mode) . yas-minor-mode)

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
  
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")
        yas-wrap-around-region t
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  
  (add-hook 'after-save-hook
            (lambda ()
              (when (eq major-mode 'snippet-mode)
                (yas-reload-all))))
  
  :config
  (yas-reload-all))

(use-package helm :demand t :ensure t :pin melpa-stable
  :general
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

  (:prefix nil :states nil
           "C-c h"   'helm-command-prefix
           "M-y"     'helm-show-kill-ring
           "C-x b"   'helm-mini
           "C-x C-f" 'helm-find-files         
           "C-x f" 'helm-find-files         
           "M-x"     'helm-M-x
           "M-:"     'helm-eval-expression-with-eldoc
           "M-i"     'helm-occur
           "M-3"     'helm-mini
           "M-6"     'helm-bookmarks)
  
  (:prefix  nil :states '(normal)
            "ga" 'helm-apropos)

  (:prefix nil :keymaps 'helm-find-files-map :states nil
           "C-<backspace>"   'backward-kill-word
           "C-d"             '(lambda ()
                                (interactive)
                                (helm-exit-and-execute-action
                                 'helm-point-file-in-dired)))

  (:prefix   nil :keymaps 'helm-map :states   nil
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
  (setq
   helm-buffer-max-length                 nil
   helm-idle-delay                        0.0
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
   helm-grep-ag-command "rg --vimgrep --no-heading --smart-case")
  
  (use-package async :ensure t)
  (use-package helm-config :demand t)
  
  :config 
  (use-package helm-mode :demand t)
  
  (helm-autoresize-mode)

  (substitute-key-definition 'find-tag 'helm-etags-select global-map))

(use-package projectile :demand t :ensure t :pin melpa-stable
  :commands projectile-project-root

  :general
  (:prefix nil :states nil
   "M-1" 'projectile-find-file
   "M-!" 'projectile-run-shell-command-in-root)
  
  ;;  Projectile-only bindigs for Evil mode
  (:states 'normal
   "p"  '(:ignore t :wk "Projectile")
   "pp" 'projectile-switch-project
   "pc" 'projectile-compile-project
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
  (require 'subr-x)
  (setq
   projectile-enable-caching       t
   projectile-completion-system   'helm
   projectile-indexing-method     'hybrid
   projectile-require-project-root t
   projectile-use-git-grep         nil
   projectile-mode-line            '(:eval (format " {%s}" (projectile-project-name)))

   projectile-git-submodule-command "git submodule --quiet foreach 'echo $path' | tr '\\r\\n' '\\0'"
   projectile-project-root-files-functions '(projectile-root-local
                                             projectile-root-top-down
                                             projectile-root-bottom-up
                                             projectile-root-top-down-recurring))

  :config
  (projectile-register-project-type 'bloop '(".bloop")
                                    :compile "bloop compile --reporter scalac --no-color root"
                                    :test "bloop test --propagate --reporter scalac root"
                                    :src-dir "src/main/"
                                    :test-dir "src/test/"
                                    :test-suffix "Spec")

  (projectile-mode))

(use-package avy :ensure t
  :general
  (:keymaps 'global :states 'normal
            "jj" 'avy-goto-char
            "jl" 'avy-goto-line))

(use-package magit :ensure t :pin melpa-stable :disabled t
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

  (defun 4l/magit-latest-tag ()
    (car (nreverse
          (cl-sort (magit-list-tags) #'version<
                   :key (lambda (tag)
                          (if (string-prefix-p "v" tag)
                              (substring tag 1)
                            tag))))))
  
  (defun 4l/magit-create-github-release (tag)
    (interactive (list (read-string "Create tag: " (4l/magit-latest-tag))))

    ;; Update README file in the repo if it has tag refs
    (with-temp-file "README.md"
      (insert-file-contents "README.md")
      (let ((prev-tag (s-chop-prefix "v" (4l/magit-latest-tag)))
            (tag-v    (s-chop-prefix "v" tag)))
        (while (search-forward-regexp prev-tag nil t)
          (replace-match tag-v nil t))))

    ;; Get a list of change between two tags
    ;; magit-insert-log
    ;; (magit-commit-create) ;; Should contain a list of change between two tags

    ;; (magit-git-wash (apply-partially #'magit-log-wash-log 'log)
    ;; "log"
    ;; "--pretty=oneline"
    ;; "--pretty=format:* %s"
    ;; )
    )

  (defun 4l/magit-insert-changes-since-rev (tag)
    (interactive (list (read-string "Last rev: " (4l/magit-latest-tag))))

    (magit-git-wash (apply-partially #'magit-log-wash-log 'log)
                    "log"
                    "--pretty=oneline"
                    "--pretty=format:* %s"
                    (format "HEAD...%s" tag)))
  
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
  
  :hook ((4l/fix-scala-fonts hs-minor-mode) . scala-mode)
  
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
  (use-package sbt-mode :ensure t
    :general
    (:keymaps 'scala-mode-map :prefix "<SPC> sb"
              ""  '(:ignore t :wk "SBT")
              "b" '(4lex1v:open-sbt-build-file :wk "build.sbt")
              "s" 'sbt-start
              "r" 'sbt-command
              "c" '((lambda () (interactive) (sbt-command "compile")) :wk "compile")
              "t" '((lambda () (interactive) (sbt-command "test")) :wk "test"))
    
    (:keymaps 'scala-mode-map :prefix ","
              "c" '((lambda () (interactive) (sbt-command "compile")) :wk "compile")
              "t" '((lambda () (interactive) (sbt-command "test")) :wk "test")
              "r" 'sbt-run-previous-command
              "i" '4l/open-in-intellij)

    :init
    (setq sbt:prompt-regexp  "^\\(\\(scala\\|\\[[^\]]*\\] \\)?[>$]\\|[ ]+|\\)[ ]*"
          sbt:prefer-nested-projects t)
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
  :hook ((cargo-minor-mode) . rust-mode)
  
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
    (setq cargo-process--enable-rust-backtrace t)))

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
  :general
  ;; Global Org-mode fucntionality
  ("o" '(:ignore t :wk "Org")
   "oa" 'org-agenda-list
   "ou" '((lambda () (interactive) (find-file "~/Dropbox/Sandbox/Library/Org/universe.org")) :wk "universe.org")
   "oi" '((lambda () (interactive) (find-file "~/Dropbox/Sandbox/Library/Org/inbox.org")) :wk "inbox.org")
   "oo" 'org-agenda
   "oc" 'org-capture
   "ot" 'org-todo-list)
  
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
  (defun 4l/get-org-agenda-files ()
    (let ((block-list '("inbox.org")))
      (f-files "~/Dropbox/Sandbox/Library/Org/"
              (lambda (path)
                (and (f-ext? path "org")
                     (not (s-starts-with-p "_" (f-filename path)))
                     (not (-contains-p block-list (f-filename path))))))))

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

  (setq
   org-log-done                  'time ;; When completing a task, prompt for a closing note...
   org-src-fontify-natively       t
   org-descriptive-links          t
   org-startup-with-inline-images t
   org-use-tag-inheritance        nil

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
   
   ;; Keywords
   org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "ACTIVE" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)"))
   org-todo-keyword-faces '(("ACTIVE" . "yellow"))

   org-refile-use-outline-path 'file
   org-refile-targets '((org-agenda-files . (:tag . "project")))
;;   org-refile-target-verify-function #'(lambda () (member "project" (org-get-local-tags)))

   org-adapt-indentation nil

   org-archive-location "./archives/%s_archive::"

   org-agenda-files '("~/Dropbox/Sandbox/Library/Org/universe.org")

   org-capture-templates
   '(("n" "New" entry (file "~/Dropbox/Sandbox/Library/Org/inbox.org")      "* TODO %? \n:PROPERTIES: \n:created: %U \n:END:")
     ("t" "Today" entry (file "~/Dropbox/Sandbox/Library/Org/universe.org") "* TODO %? \nSCHEDULED: %t \n:PROPERTIES: \n:created: %U \m:processed: %U \n:END:")
     ("w" "Work" entry (file "~/Dropbox/Sandbox/Library/Org/universe.org")  "* TODO %? \nSCHEDULED: %t \nSCHEDULED: %t \n:PROPERTIES: \n:created: %U \m:processed: %U \n:END:"))

   ;; Stuck project is the one that has no scheduled TODO tasks
   org-stuck-projects '("+project/-DONE-CANCELLED" ("TODO") nil "SCHEDULED:\\|DEADLINE:")
   
   org-agenda-custom-commands '(("c" . "My Custom Agendas")
                                ("cu"  "Unscheduled"
                                 ((todo ""
                                        ((org-agenda-overriding-header "\nUnscheduled Tasks")
                                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline)))))
                                 nil ;; general settings for the whole set
                                 nil))
   
   org-agenda-span 'day ;; To myself: it's better then a week, trust me.
   org-agenda-start-on-weekday 7 ;; Sunday
   org-agenda-include-diary nil
   org-agenda-skip-deadline-if-done t
   org-agenda-log-mode-items '(closed clock state)
   org-agenda-prefix-format '((agenda . "  %?-12t ")
                              (todo   . " %i %-12:c")
                              (tags   . " %i %-12:c")
                              (search . " %i %-12:c"))
   
   ;; Display agenda in full window
   org-agenda-window-setup 'current-window)

  :config
  (org-indent-mode -1)

  (with-eval-after-load 'evil
    (use-package evil-org :ensure t :demand t
      :config
      (require 'evil-org-agenda)
      (evil-set-initial-state 'org-agenda-mode 'motion)
      (evil-org-agenda-set-keys))

    (with-eval-after-load 'evil-collection
      (add-to-list 'evil-collection-mode-list 'outline)))

  ;; Configure hooks for clock's persistance
  (org-clock-persistence-insinuate)

  (add-hook 'org-agenda-finalize-hook
            (lambda () (remove-text-properties
                        (point-min) (point-max) '(mouse-face t)))))

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
  
  :hook
  ((4lex1v:helm-eshell-history
    ansi-color-for-comint-mode-on) . eshell-mode)
  
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

(if (not (file-exists-p (concat USER-EMACS-DIRECTORY "server/server")))
    (server-start))
