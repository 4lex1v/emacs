
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
    (add-to-list 'evil-collection-mode-list 'while-key))

  (message "which-key loaded"))

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
    (define-key evil-normal-state-map "K" 'evil-jump-out-args))

  (message "evil loaded"))

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
    (helm-exit-and-execute-action
     'helm-point-file-in-dired))
  
  (evil-define-key nil global-map
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
  
  (evil-define-key 'normal global-map
    (kbd "<leader> ff") 'helm-find-files
    (kbd "<leader> fl") 'find-library
    (kbd "M-i")         'helm-occur)

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
  
  :config
  (helm-mode)
  (helm-autoresize-mode)
  (substitute-key-definition 'find-tag 'helm-etags-select global-map))

(use-package ggtags
  :hook ((c-mode c++-mode rust-mode) . ggtags-mode))

(use-package rg :ensure t
  :init
  (setq rg-custom-type-aliases '())
  :config
  (rg-enable-default-bindings)
  (if IS-WINDOWS (defun rg-executable () (executable-find "rg.exe")))
  (evil-define-key 'normal 'global
    (kbd "<leader> ps") 'rg-menu))

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

(use-package rust-mode :ensure t
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

(use-package ace-window :demand t :ensure t
  :init
  (setq aw-scope 'frame)
  (evil-define-key 'normal global-map
    (kbd "C-w o") 'ace-window))

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

(use-package magit
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

  (setq
   magit-last-seen-setup-instructions "2.11.0"
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
  
  ;; This function was added to speed up my PR review workflow in a way that i can diff current branch
  ;; with master by a single keystroke...
  (defun 4l/magit-diff-branch-with (branch-name)
    (interactive)
    (let* ((args (magit-diff-arguments))
           (diff-cmd (format "%s...%s" branch-name (magit-get-current-branch))))
      (magit-diff-range diff-cmd args)))
  
  :config
  (magit-define-popup-action 'magit-submodule-popup   
                             ?l "List" 'magit-list-submodules)
  
  (magit-define-popup-switch 'magit-log-popup
                             ?f "First Parent" "--first-parent")

  (define-key magit-file-section-map [remap magit-visit-thing] #'magit-diff-visit-file-other-window)

  (add-hook 'magit-submodule-list-mode-hook
            (lambda () (setq-local tabulated-list-sort-key (cons "L<U" t)))))

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

;; General bindings
(evil-define-key 'normal global-map
  ;; Project related bindings
  (kbd "<leader> pf") 'project-find-file
  (kbd "<leader> ps") 'rg-project
  (kbd "<leader> pc") '4l/project-compile

  ;; Navigation
  (kbd "<leader> fi")  '(lambda () (interactive) (find-file (concat USER-EMACS-DIRECTORY "init.el")))

  ;; Org
  (kbd "<leader> oc") 'org-capture)

(evil-define-key nil global-map
  (kbd "C-c r") #'revert-buffer
  (kbd "<f7>")  #'4l/project-rebuild
  (kbd "<f8>")  #'next-error)

