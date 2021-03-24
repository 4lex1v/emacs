(require 'seq)

(setq gc-cons-threshold (* 50 1024 1024))

(defconst USER-EMACS-DIRECTORY (file-name-directory (or load-file-name (buffer-file-name))))
(defconst USER-INIT-FILE       (concat USER-EMACS-DIRECTORY "init.el"))

(defconst IS-MAC               (eq system-type 'darwin))
(defconst IS-WINDOWS           (eq system-type 'windows-nt))
(defconst IS-UNIX              (not IS-WINDOWS))

(let ((customs '("fixes.el" "__private.el" "modals.el")))
  (mapc
   (lambda (filename)
     (let ((file-path (concat USER-EMACS-DIRECTORY filename)))
       (load-file file-path)))
   customs))

(let ((font-setting (if IS-WINDOWS "Iosevka SS08 Slab LtEx-16" "Iosevka Light-20")))
  (add-to-list 'initial-frame-alist (cons 'font font-setting))
  (setq default-frame-alist initial-frame-alist)
  (set-frame-font font-setting))

(add-to-list 'load-path (expand-file-name "themes/sirthias" USER-EMACS-DIRECTORY))
(when (locate-library "sirthias-theme")
  (setq sirthias-easy-mode t)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (load-theme 'sirthias t)))
  (require 'sirthias-theme nil nil)
  (load-theme 'sirthias t))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(fset 'yes-or-no-p   'y-or-n-p)

(show-paren-mode       -1)
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
 user-ref-name                 "4lex1v"
 mouse-wheel-scroll-amount     '(1)
 mouse-wheel-progressive-speed  nil
 history-delete-duplicates      t
 custom-file                   (concat USER-EMACS-DIRECTORY "custom.el")
 
 ;; Performance related settings
 inhibit-compacting-font-caches t
 jit-lock-defer-time 0
 fast-but-imprecise-scrolling t
 default-directory              "~/Sandbox/"
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
       frame-resize-pixelwise       t)
      (global-set-key (kbd "M-`") #'other-frame)))

;; (if IS-WINDOWS
;;     (setq shell-file-name "C:\\Users\\Aleksandr\\.universe\\windows\\bin\\pwshproxy.bat"))

;; To avoid acidental hits on the touchpad
(dolist (binding '("<mouse-1>" "<C-mouse-1>" "<C-down-mouse-1>"))
  (define-key global-map (kbd binding)
    (lambda nil (interactive) nil)))

(defun 4l/close-compilation-window ()
  (interactive)
  (if-let ((compilation-window (get-buffer-window "*compilation*")))
      (quit-window nil compilation-window)))

(defun 4l/open-shell-in-root ()
  (interactive)
  (let ((default-directory (vc-root-dir)))
    (call-interactively 'shell)))

(defun 4l/next-sexp ()
  (interactive)
  (forward-sexp)
  (forward-sexp)
  (backward-sexp))

(define-key global-map (kbd "C-c p s s") #'4l/open-shell-in-root)

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
  (insert "{")
  (newline-and-indent)
  (newline)
  (insert (if end-with-semicolon-p "};" "}"))
  (beginning-of-line)
  (indent-according-to-mode)
  (previous-line)
  (indent-according-to-mode))

(defun 4l/project-shell ()
  (interactive)
  (let ((default-directory (cdr (project-current t))))
    (call-interactively 'shell)))

(defun 4l/project-build ()
  (interactive)
  (let ((default-directory (cdr (project-current t))))
    (call-interactively 'compile)))

(defun 4l/project-rebuild ()
  (interactive)
  (let ((default-directory (cdr (project-current t))))
    (call-interactively 'recompile)))

;; Bindings
(global-set-key (kbd "M-[") (lambda () (interactive) (4l/insert-block nil)))
(global-set-key (kbd "M-{") (lambda () (interactive) (4l/insert-block t)))
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "<C-up>") 'scroll-down)
(global-set-key (kbd "<C-down>") 'scroll-up)
(global-set-key (kbd "C-j") 'join-line)
(global-set-key (kbd "C-c r") (lambda () (interactive) (revert-buffer nil t t)))
(global-set-key (kbd "M-`") 'next-error)
(global-set-key (kbd "C-,") '4l/close-compilation-window)
;; (define-key global-map (kbd "C-,") #'4l/swap-two-windows)
(define-key global-map (kbd "M-4") #'list-bookmarks)
(define-key global-map (kbd "M-1") 'project-find-file)
(define-key global-map (kbd "C-c h a") 'apropos)
(define-key global-map (kbd "C-c p c") #'4l/project-build)
(define-key global-map (kbd "<f5>") #'4l/project-rebuild)

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
 package--init-file-ensured t
 package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                    ("gnu"          . "http://elpa.gnu.org/packages/")))

;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(defun package--save-selected-packages (&rest opt) nil)
(require 'package) 
(package-initialize)

(require 'epa-file)
(epa-file-enable)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq
 use-package-verbose               t
 use-package-always-defer          t
 use-package-enable-imenu-support  t
 use-package-check-before-init     t
 use-package-minimum-reported-time 0.1

 ;; Only when the config is stable
 use-package-expand-minimally t)

;; Packages

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

(use-package ace-window :demand t :ensure t
  :bind
  ("M-o" . ace-window)
  :init
  (setq aw-scope 'frame))

;; Goes before others to correctly load which-key-declare-prefixes
(use-package which-key :demand t :ensure t
  :bind
  (:map which-key-C-h-map
   ("l" . which-key-show-next-page-cycle)
   ("j" . which-key-show-previous-page-cycle))
 
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
  (which-key-mode))

(use-package helm :demand t :ensure t
  :bind
  (("C-c h"   . helm-command-prefix)
   ("C-c h a" . helm-apropos)
   ("C-c h s" . 4l/helm-git-grep-project-files)
   ("M-x"     . helm-M-x)
   ("C-x f"   . helm-find-files)
   ("C-x C-f" . helm-find-files)
   ("M-2"     . helm-mini)
   ("M-4"     . helm-bookmarks)
   ("M-:"     . helm-eval-expression-with-eldoc)
   ("M-i"     . helm-occur)
   ("M-y"     . helm-show-kill-ring)
   
   :map helm-find-files-map
   ("C-<backspace>" . backward-kill-word)
   ("C-d"           . 4l/helm-open-dired)

   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i"   . helm-execute-persistent-action)
   ("C-z"   . helm-select-action)
   ("C-o"   . helm-next-source)
   ("C-f"   . helm-toggle-full-frame))

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

  (defun 4l/helm-open-dired ()
    (interactive)
    (helm-exit-and-execute-action
     'helm-point-file-in-dired))

  (defun 4l/helm-git-grep-project-files (arg)
    (interactive "P")
    (require 'helm-files)
    (let ((project-root (cdr (project-current t))))
      (helm-grep-git-1 project-root arg)))

  (use-package async :ensure t)
  (use-package helm-config :demand t)
  
  :config 
  (use-package helm-mode :demand t)
  
  (helm-autoresize-mode)

  (substitute-key-definition 'find-tag 'helm-etags-select global-map))

(use-package rg :ensure t
  :bind
  ("C-c s" . 'rg-menu)
  :config
  (rg-enable-default-bindings)

  (if IS-WINDOWS (defun rg-executable () (executable-find "rg.exe"))))

(use-package elisp-mode
  :interpreter ("emacs" . emacs-lisp-mode)
  :defines (emacs-lisp-mode-hook)
  :mode (("\\.el$" . emacs-lisp-mode)
         ("Cask"   . emacs-lisp-mode))
    
  :config
  (with-eval-after-load 'smartparens
    (sp-with-modes 'emacs-lisp-mode
      (sp-local-pair "'" nil :actions nil)))

  (use-package macrostep :ensure t
    :commands macrostep-expand
    :mode ("\\*.el\\'" . emacs-lisp-mode)))
    
(use-package rye-lang-mode
  :load-path "modules/rye"
  :mode "\\.rye\\'")

(use-package scala-mode :ensure t
  :mode        ("\\.\\(scala\\|sbt\\|sc\\)\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  
  :hook ((4l/fix-scala-fonts hs-minor-mode) . scala-mode)
  
  :bind
  (:map scala-mode-map
   ("<C-return>" . #'newline-or-comment)
   ("C-c p c" . #'4l/compile-scala))

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

  (defun 4l/compile-scala ()
    (interactive)
    (let* ((default-directory (vc-root-dir)))
      (setq compile-command "bloop compile --reporter scalac --no-color root")
      (call-interactively 'compile)))

  :config
  (use-package sbt-mode :ensure t
    :init
    (setq
     sbt:prompt-regexp "^\\(\\(sbt:[^>]+\\)?\\|scala\\)>[ ]+"
     sbt:prefer-nested-projects t)

    :config
    (add-to-list 'sbt:program-options "-Dsbt.supershell=false")))

(use-package cc-mode
  :mode (("\\.h\\'"  . c++-mode)
         ("\\.glsl\\'" . c++-mode)
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
  (font-lock-add-keywords 'c++-mode '(("\\<\\(assert\\|internal\\|global_var\\|local_persist\\|nullptr\\)\\>" 1 font-lock-keyword-face)))
  (font-lock-add-keywords 'objc-mode '(("\\<\\(assert\\|internal\\|global_var\\|local_persist\\)\\>" 1 font-lock-keyword-face)))
  
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(C . t))))

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
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))))

(use-package web-mode :ensure t
  :mode 
  (("\\.mustache\\'" . web-mode)       
   ("\\.html?\\'" . web-mode))
  :init
  (setq
   web-mode-enable-block-face t
   web-mode-enable-comment-keywords t
   web-mode-enable-current-element-highlight t
   web-mode-enable-current-column-highlight t
   web-mode-script-padding 2
   web-mode-style-padding 2
   web-mode-comment-style 2
   web-mode-code-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

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

(use-package org :demand t  
  :init
  (setq
   org-directory "~/Sandbox/Org/"
   org-startup-with-inline-images t
   org-startup-truncated nil 

   org-agenda-files '("~/Sandbox/Org/universe.org" "~/Sandbox/Org/dailies.org")

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
   org-babel-C-compiler "clang"
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
   '(("n" "Add to Inbox"   entry (file "~/Sandbox/Org/inbox.org")       "* TODO %? \n")
     ("t" "Task for Today" entry (file "~/Sandbox/Org/universe.org")    "* TODO %? \nSCHEDULED: %t\n")
     ("r" "Journal Entry"  entry (file "~/Sandbox/Org/ruminations.org") "* %?\n:PROPERTIES:\n:ID:      %(org-id-new)\n:CREATED: %U\n:END:")
     ("d" "Daily Entry"    entry (file "~/Sandbox/Org/dailies.org")     "* %?\n:PROPERTIES:\n:ID:      %(org-id-new)\n:CREATED: %U\n:END:"))

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

  :bind
  (("M-3" . org-agenda-list)
   ("C-c o c" . org-capture)
   ("C-c o s" . org-store-link)
   ("C-c o l" . org-insert-link)
   
   :map org-mode-map
   ("C-." . org-mark-ring-goto)
   ("C-," . org-archive-subtree)
   ("C-c S" . org-store-link))

  :config
  ;; Force org to open file links in the same window
  (add-to-list 'org-link-frame-setup '(file . find-file))

  ;; Configure hooks for clock's persistance
  (org-clock-persistence-insinuate)

  (add-hook 'org-agenda-finalize-hook
            (lambda () (remove-text-properties
                        (point-min) (point-max) '(mouse-face t)))))

(use-package org-roam :ensure t :demand t
  :commands (org-roam)
  :init
  (setq
   org-roam-directory "~/Sandbox/Org/roam/"
   org-roam-list-files-commands '(rg)
   org-roam-completion-system 'helm
   org-roam-buffer-window-parameters '((no-other-window . t)
                                       (mode-line-format . nil))
   org-roam-capture-templates '(("d" "default" plain #'org-roam-capture--get-point
                                 "%?"
                                 :file-name "${slug}"
                                 :head "#+title: ${title}"
                                 :unnarrowed t)))
  
  :bind
  (("C-c n l" . org-roam)
   ("C-c n t" . org-roam-dailies-find-today)
   ("C-c n f" . org-roam-find-file)
   ("C-c n i" . org-roam-insert-immediate)

   :map org-roam-mode-map
   ("C-c l" . org-roam-dailies-map)))

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
  (setq
   yas-snippet-dirs '("~/.emacs.d/snippets")
   yas-wrap-around-region t
   yas-indent-line 'auto
   yas-also-auto-indent-first-line t)
  
  (add-hook 'after-save-hook
            (lambda ()
              (when (eq major-mode 'snippet-mode)
                (yas-reload-all))))
  
  :config
  (yas-reload-all))

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
    (eshell/alias "sshs" "ssh-add ~/.ssh/github_rsa")))

(mapc
 (lambda (mode)
   (font-lock-add-keywords ;;`font-lock-keywords`
    mode
    '(("#\\<\\(TODO\\)\\>" 1 '(error :underline t) t)
      ("#\\<\\(NOTE\\)\\>" 1 '(warning :underline t) t))))
 '(emacs-lisp-mode lua-mode scala-mode c-mode objc-mode c++-mode rust-mode))

(make-variable-buffer-local 'compile-command)
