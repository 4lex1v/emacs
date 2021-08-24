(require 'seq)

(setq
 gc-cons-threshold       (* 100 1024 1024)  ;; 100MB
 read-process-output-max (*   1 1024 1024)) ;;   1MB

(defconst USER-EMACS-DIRECTORY (file-name-directory (or load-file-name (buffer-file-name))))
(defconst USER-INIT-FILE       (concat USER-EMACS-DIRECTORY "init.el"))

(defconst IS-MAC               (eq system-type 'darwin))
(defconst IS-WINDOWS           (eq system-type 'windows-nt))
(defconst IS-UNIX              (not IS-WINDOWS))

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

(defun 4l/project-root ()
  (expand-file-name (cdr (project-current t))))

;; TODO: Added in Emacs 28
(defun 4l/project-compile ()
  "Run `compile' in the project root."
  (declare (interactive-only compile))
  (interactive)
  (let ((default-directory (4l/project-root)))
    (call-interactively #'compile)))

(defun 4l/open-git-bash-shell ()
  (interactive)
  "Start a git-bash.exe process on Windows as a shell"
  (let ((default-directory (4l/project-root))
        (shell-file-name "C:\\Users\\Aleksandr\\scoop\\apps\\git\\current\\bin\\bash.exe")
        (explicit-bash-args '("--login -i")))
    (call-interactively 'shell)))

(defun 4l/project-rebuild ()
  (interactive)
  (let ((default-directory (cdr (project-current t))))
    (call-interactively 'recompile)))

(let* ((font-name "Iosevka SS08 Slab Extended")
       (font-setting (concat font-name "-" "14")))
  (if (member font-name (font-family-list))
      (progn
        (add-to-list 'initial-frame-alist (cons 'font font-setting))
        (setq default-frame-alist initial-frame-alist)
        (set-frame-font font-setting))))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(fset 'yes-or-no-p   'y-or-n-p)

(show-paren-mode       t)
(delete-selection-mode t)
(global-auto-revert-mode t)
(prefer-coding-system 'utf-8-unix)
(column-number-mode 1)
(recentf-mode 1)
(blink-cursor-mode 0)
(tool-bar-mode -1)

(setq-default
 auto-window-vscroll nil
 truncate-lines      nil
 initial-major-mode (quote fundamental-mode)
 mode-line-default-help-echo nil
 tab-width           2
 indent-tabs-mode    nil
 cursor-type        'box
 cursor-in-non-selected-windows 'bar
 frame-title-format "%f"
 linum-format       "%3d " 
 load-prefer-newer  t
 left-fringe-width  20
 word-wrap t
 line-spacing 2

 buffer-file-coding-system 'utf-8-unix

 case-fold-search nil
 case-replace     nil)

(setq
 inhibit-startup-message        t
 initial-scratch-message        nil
 initial-buffer-choice          nil
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
 default-directory             "~/"
 user-ref-name                 "4lex1v"
 mouse-wheel-scroll-amount     '(1)
 mouse-wheel-progressive-speed  nil
 history-delete-duplicates      t
 custom-file                   (concat USER-EMACS-DIRECTORY "custom.el")

 ;; Performance related settings
 inhibit-compacting-font-caches t
 jit-lock-defer-time 0
 fast-but-imprecise-scrolling t
 search-upper-case              nil
 dabbrev-case-distinction nil
 scroll-step 5) ;; optimal balance between scroll-speed and line flickering

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
       frame-resize-pixelwise       t)))

(if IS-WINDOWS
    (progn
      (require 'subr-x)
      (if-let ((powershell-exe-path (executable-find "pwsh.exe")))
          (setq shell-file-name powershell-exe-path))))

;; To avoid acidental hits on the touchpad
(dolist (binding '("<mouse-1>" "<C-mouse-1>" "<C-down-mouse-1>"))
  (define-key global-map (kbd binding)
    (lambda nil (interactive) nil)))

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

(use-package sirthias-theme :demand t
  :load-path "themes/sirthias"
  :init
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (load-theme 'sirthias t)))
  :config
  (load-theme 'sirthias t))

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

;; Goes before others to correctly load which-key-declare-prefixes
(use-package which-key :demand t :ensure t :pin melpa-stable
  :diminish which-key
  :init
  (setq
   which-key-idle-delay 0.8
   which-key-sort-order 'which-key-prefix-then-key-order-reverse
   which-key-show-operator-state-maps t ;; Hack to make this work with Evil
   which-key-prefix-prefix ""
   which-key-side-window-max-width 0.5

   which-key-popup-type           'side-window 
   which-key-side-window-location 'bottom) 
  
  :config
  (which-key-mode)

  (define-key which-key-C-h-map "l" 'which-key-show-next-page-cycle)
  (define-key which-key-C-h-map "j" 'which-key-show-previous-page-cycle)

  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'while-key)))

(use-package org :demand t  
  :init
  (defun 4l/org/make-quick-note (name)
    (interactive "B")
    (let ((note-path (concat org-quick-note-folder name ".org")))
      (unless (not (file-exists-p note-path))
        (write-region "" nil note-path))
      (find-file note-path)))

  (defun 4l/org/open-daily-note ()
    (interactive)
    (let* ((today-string (format-time-string "%Y-%m-%d"))
           (dailies-folder (concat org-directory "/dailies/"))
           (today-file (concat dailies-folder today-string ".org")))
      (find-file today-file)))
  
  (defun 4l/org/list-agenda-files ()
    (interactive)
    (helm :sources (helm-build-sync-source "Org-mode Agenda Files:"
                     :candidates 'org-agenda-files
                     :fuzzy-match t
                     :action 'find-file)))

  (defun 4l/org/open-universe ()
    (interactive)
    (find-file (concat org-directory "universe.org")))
  
  (setq
   org-directory "~/org/"
   org-startup-with-inline-images t
   org-startup-truncated nil 

   org-id-link-to-org-use-id t

   4l/org-dailies-folder-path     "~/org/dailies"
   org-agenda-files              '("~/org/universe.org") ;; NOTE: Seems like this should be a list

   org-log-done                  'time ;; When completing a task, prompt for a closing note...
   org-src-fontify-natively       t
   org-descriptive-links          t
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
   org-babel-C-compiler   "clang"
   org-babel-C++-compiler "clang++"

   org-ellipsis " [...]"
   
   org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "ACTIVE" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)"))
   org-todo-keyword-faces '(("ACTIVE" . "yellow"))

   org-refile-use-outline-path 'file
   org-refile-targets '((org-agenda-files . (:tag . "project")))
   ;;   org-refile-target-verify-function #'(lambda () (member "project" (org-get-local-tags)))

   org-adapt-indentation nil

   org-archive-location "./archives/%s_archive::"

   org-capture-templates
   `(("n" "New Task"       entry (file ,(concat org-directory "universe.org"))    "* TODO %? \n")
     ("t" "Task for Today" entry (file ,(concat org-directory "universe.org"))    "* TODO %? \nSCHEDULED: %t\n")
     ("r" "Journal Entry"  entry (file ,(concat org-directory "ruminations.org")) "* %?\n:PROPERTIES:\n:ID:      %(org-id-new)\n:CREATED: %U\n:END:")
     ("d" "Daily Entry"    entry (file ,(concat org-directory "dailies.org"))     "* %?\n:PROPERTIES:\n:ID:      %(org-id-new)\n:CREATED: %U\n:END:"))

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

  :bind
  (("M-3"     . org-agenda-list)
   ("C-c o c" . org-capture)
   ("C-c o s" . org-store-link)
   ("C-c o l" . org-insert-link)
   ("C-c o u" . 4l/org/open-universe)
   ("C-c o d" . 4l/org/open-daily-note)
   ("C-c o p" . (lambda () (interactive) (find-file (concat org-directory "the_plan.org"))))
   
   :map org-mode-map
   ("C-."   . org-mark-ring-goto)
   ("C-,"   . org-archive-subtree)
   ("C-c S" . org-store-link))

  :config
  ;; Force org to open file links in the same window
  (add-to-list 'org-link-frame-setup '(file . find-file))

  ;; Configure hooks for clock's persistance
  (org-clock-persistence-insinuate)

  (add-hook 'org-agenda-finalize-hook
            (lambda () (remove-text-properties
                        (point-min) (point-max) '(mouse-face t)))))

(use-package ace-window :demand t :ensure t
  :init
  (setq aw-scope 'frame))

(use-package ggtags
  :hook ((c-mode c++-mode rust-mode) . ggtags-mode))

(use-package helm :demand t :ensure t :pin melpa-stable
  :init
  (setq
   helm-buffer-max-length                 nil
   helm-idle-delay                        0.0
   helm-input-idle-delay                  0.01
   helm-quick-update                      t
   helm-split-window-inside-p             t
   helm-buffers-fuzzy-matching            t
   helm-move-to-line-cycle-in-source      t
   helm-scroll-amount                     8

   helm-follow-mode-persistent            t
   helm-show-completion-display-function  nil
   helm-grep-ag-command                  "rg --vimgrep --no-heading --smart-case"

   helm-ff-search-library-in-sexp         t
   helm-ff-file-name-history-use-recentf  t
   helm-ff-fuzzy-matching                 t)
  
  (require 'helm-config)

  (defun 4l/helm-open-dired ()
    (interactive)
    (helm-exit-and-execute-action 'helm-point-file-in-dired))
  
  :config
  (helm-mode)
  (helm-autoresize-mode)
  (substitute-key-definition 'find-tag 'helm-etags-select global-map))

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
       "\n" (user-login-name) " in " (abbreviate-file-name (eshell/pwd)) 
       (if branch-name (format " @ %s " branch-name) "") "\n"
       "$ "
       )))

  (setq eshell-prompt-function #'4lex1v:eshell-prompt
        eshell-prompt-regexp ".*>>+ ")

  :hook
  ((4lex1v:helm-eshell-history
    ansi-color-for-comint-mode-on) . eshell-mode)

  :config
  (with-eval-after-load 'em-term
    (add-to-list 'eshell-visual-commands "htop")))

(use-package rust-mode :ensure t
  :init 
  (setq
   rust-indent-offset  2
   rust-format-on-save nil))

(use-package evil :ensure t :pin melpa-stable :demand t
  :init
  (setq-default
   evil-kill-on-visual-paste nil)

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

  ;; TODO: Does evil still requires undo-tree?
  (use-package undo-tree :ensure t :demand t
    :diminish undo-tree-mode
    :commands global-undo-tree-mode
    :config (global-undo-tree-mode))

  :config
  (evil-set-leader 'normal (kbd "SPC"))

  (evil-set-initial-state 'prog-mode        'normal)
  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'comint-mode      'normal)
  (evil-set-initial-state 'org-mode         'normal)

  (evil-set-initial-state 'package-menu-mode 'motion)

  (evil-set-initial-state 'rg 'emacs)

  (evil-set-initial-state 'eshell 'insert)

  (evil-ex-define-cmd "e[val]" #'eval-buffer)
  (evil-ex-define-cmd "we" #'(lambda () (interactive) (save-buffer) (eval-buffer)))

  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode)

  (evil-define-key nil evil-motion-state-map
    "j" 'next-line
    "k" 'previous-line)

  (evil-define-key nil evil-insert-state-map
    (kbd "C-;")    'comment-line
    (kbd "C-x \\") 'align-regexp
    (kbd "C-c r")  'revert-buffer
    (kbd "M-j")   'join-line
    (kbd "M-[")   '(lambda () (interactive) (4l/insert-block nil))
    (kbd "M-{")   '(lambda () (interactive) (4l/insert-block t))
    (kbd "C-S-d")  '4l/duplicate-line)

  (evil-define-key nil evil-normal-state-map
    "$"                    'evil-end-of-visual-line
    (kbd "C-j")            'evil-forward-paragraph
    (kbd "C-k")            'evil-backward-paragraph
    (kbd "<M-wheel-up>")   'text-scale-increase
    (kbd "<M-wheel-down>") 'text-scale-decrease
    (kbd "C-S-o")          'evil-jump-forward
    "m"                    'back-to-indentation
    (kbd "g .")            'xref-find-definitions)

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
                                 (term term ansi-term multi-term)
                                 xref))

    :config
    (add-hook 'after-init-hook (lambda () (evil-collection-init))))

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

(use-package avy :ensure t
  :config
  (evil-define-key 'normal 'global 
    (kbd "<leader> jj") 'avy-goto-char
    (kbd "<leader> jl") 'avy-goto-line))

(use-package elisp-mode
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode        (("\\.el$" . emacs-lisp-mode)
                ("Cask"   . emacs-lisp-mode))
  
  :init
  (evil-define-key nil global-map
    (kbd "M-.")     'find-function-at-point
    (kbd "M-,")     'find-variable-at-point
    (kbd "C-c e r") 'eval-region)

  (use-package macrostep :ensure t
    :commands macrostep-expand
    :mode ("\\*.el\\'" . emacs-lisp-mode)
    
    :init
    (evil-define-key nil macrostep-keymap
      "q" #'macrostep-collapse-all
      "e" #'macrostep-expand)

    (evil-define-key 'normal emacs-lisp-mode-map
      (kbd "<leader> em") #'macrostep-expand)
    
    :config
    (with-eval-after-load 'evil-collection
      (add-to-list 'evil-collection-mode-list 'macrostep))))

(use-package cc-mode :demand t
  :mode (("\\.\\(glsl\\|vert\\|frag\\)\\'" . c-mode)
         ("\\.mm\\'" . objc-mode))
  
  :commands c-toggle-auto-newline
  :defines (c-mode-common-hook)

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
                          (inextern-lang . [0])
                          (case-label . +)
                          (substatement-open . 0)))))

  (setq
   win32-system-include-paths '("c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/shared"
                                "c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/ucrt"
                                "c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/um"
                                "c:/Program Files (x86)/Windows Kits/10/Include/10.0.17134.0/winrt")
   c-default-style "4l")

  (defun 4l/start-remedy ()
    (interactive)
    (let* ((default-directory (vc-root-dir))
           (debug-project (concat default-directory "project.rdbg")))
      (start-process "remedybg" nil "remedybg.exe" debug-project)))

  :config
  (c-add-style "4l" 4l/c-lang-style)
  
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(C . t)))
  
  ;; Something helpful in Handmade Hero... Not sure if i'm going to use it in other projects...
  (font-lock-add-keywords 'c++-mode '(("\\<\\(assert\\|defer\\|internal\\|global_var\\|local_persist\\|nullptr\\)\\>" 1 font-lock-keyword-face)))
  (font-lock-add-keywords 'objc-mode '(("\\<\\(assert\\|internal\\|global_var\\|local_persist\\)\\>" 1 font-lock-keyword-face)))
  
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(C . t))))

(use-package lsp-mode :ensure t :disabled t
  :commands lsp-mode
  :hook (((c-mode c++-mode rust-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))

  :bind
  (:map lsp-mode-map
   ("C-c C-d" . lsp-describe-thing-at-point)
   ([remap xref-find-definitions] . lsp-find-definition)
   ([remap xref-find-references] . lsp-find-references))

  :custom-face
  (lsp-face-highlight-textual
   ((t :underlying (:style line :color ,(face-foreground 'default)))))

  :init
  (setq
   lsp-prefer-capf t
   lsp-auto-guess-root t

   ;; clangd
   lsp-clients-clangd-args '("--header-insertion=never"))

  :config
  (evil-define-key 'normal lsp-mode-map
    (kbd "<leader> l") lsp-command-map)
  
  (use-package lsp-ui
    :init
    (setq
     lsp-ui-doc-position 'bottom)

    :hook (lsp-mode . lsp-ui-mode)

    :custom-face
    (lsp-ui-doc-url
     ((t :inherit default)))

    :config
    ;; `C-g'to close doc
    (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

  (use-package helm-lsp :ensure t
    :commands helm-lsp-workspace-symbol))

;; General bindings
(evil-define-key 'normal global-map
  ;; Project related bindings
  (kbd "<leader> pf") 'project-find-file
  (kbd "<leader> ps") 'rg-project
  (kbd "<leader> pc") '4l/project-compile

  (kbd "C-w C-w") 'ace-window
  
  ;; Navigation
  (kbd "<leader> fi")  '(lambda () (interactive) (find-file (concat USER-EMACS-DIRECTORY "init.el")))

  ;; Org
  (kbd "<leader> oc") 'org-capture

  (kbd "<leader> ff") 'helm-find-files
  (kbd "<leader> fl") 'find-library
  (kbd "M-i")         'helm-occur

  (kbd "<leader> ps") 'rg-menu)

(evil-define-key nil global-map
  (kbd "C-c r")  #'revert-buffer
  (kbd "<f7>")   #'4l/project-rebuild
  (kbd "<f8>")   #'next-error

  (kbd "C-x C-f") 'helm-find-files
  (kbd "C-x f")   'helm-find-files
  (kbd "C-c h a") 'helm-apropos
  (kbd "C-c h s") '4l/helm-git-grep-project-files
  (kbd "M-x")     'helm-M-x
  (kbd "M-2")     'helm-mini
  (kbd "M-4")     'helm-bookmarks
  (kbd "M-:")     'helm-eval-expression-with-eldoc
  (kbd "M-i")     'helm-occur
  (kbd "M-y")     'helm-show-kill-ring
  (kbd "M-.")     'helm-etags-select)

(evil-define-key nil helm-find-files-map
  (kbd "C-<backspace>") 'backward-kill-word
  (kbd "C-d")           '4l/helm-open-dired)

(evil-define-key nil helm-map
  (kbd "<tab>") 'helm-execute-persistent-action
  (kbd "C-i")   'helm-execute-persistent-action
  (kbd "C-z")   'helm-select-action
  (kbd "C-o")   'helm-next-source
  (kbd "C-j")   'helm-next-line
  (kbd "C-k")   'helm-previous-line
  (kbd "C-f")   'helm-toggle-full-frame)

(mapc
 (lambda (mode)
   (font-lock-add-keywords ;;`font-lock-keywords`
    mode
    '(("#\\<\\(TODO\\)\\>" 1 '(error :underline t) t)
      ("#\\<\\(NOTE\\)\\>" 1 '(warning :underline t) t))))
 '(prog-mode))


