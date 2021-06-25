
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

(defun 4l/project-build ()
  (interactive)
  (let ((default-directory (cdr (project-current t))))
    (call-interactively 'compile)))

(defun 4l/project-rebuild ()
  (interactive)
  (let ((default-directory (cdr (project-current t))))
    (call-interactively 'recompile)))

;; Bindings
(global-set-key (kbd "<C-return>") #'newline-and-indent)
(global-set-key (kbd "<M-return>") #'newline-and-indent)
(global-set-key (kbd "M-[") (lambda () (interactive) (4l/insert-block nil)))
(global-set-key (kbd "M-{") (lambda () (interactive) (4l/insert-block t)))
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "<C-up>") 'scroll-down)
(global-set-key (kbd "<C-down>") 'scroll-up)
(global-set-key (kbd "C-j") 'join-line)
(global-set-key (kbd "C-c r") (lambda () (interactive) (revert-buffer nil t t)))
(global-set-key (kbd "M-`") 'next-error)
(global-set-key (kbd "M-4") #'list-bookmarks)
(global-set-key (kbd "M-1") 'project-find-file)
(global-set-key (kbd "C-c h a") 'apropos)
(global-set-key (kbd "C-c p c") #'4l/project-build)
(global-set-key (kbd "<f7>") #'4l/project-rebuild)
(global-set-key (kbd "<f8>") #'next-error)
(global-set-key (kbd "C-.") #'xref-find-definitions)
(global-set-key (kbd "C-,") #'xref-pop-marker-stack)

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
 package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                    ("gnu"          . "https://elpa.gnu.org/packages/")))

;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(defun package--save-selected-packages (&rest opt) nil)
(require 'package) 
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq
 use-package-verbose               t
 use-package-always-defer          t
 use-package-enable-imenu-support  t
 use-package-check-before-init     t
 use-package-minimum-reported-time 0.1

 ;; Only when the config is stable
 use-package-expand-minimally t)

;; Packages

(use-package exec-path-from-shell :ensure t :demand t :disabled t
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

(use-package which-key :demand t :ensure t :pin melpa-stable
  :init
  (setq
   which-key-idle-delay 0.8
   which-key-sort-order 'which-key-prefix-then-key-order-reverse
   which-key-show-operator-state-maps t ;; Hack to make this work with Evil
   which-key-prefix-prefix ""
   which-key-side-window-max-width 0.5

   which-key-popup-type           'side-window 
   which-key-side-window-location 'bottom) 
  
  :bind
  (:map which-key-C-h-map
   ("l" . which-key-show-next-page-cycle)
   ("j" . which-key-show-previous-page-cycle))

  :config
  (which-key-mode))

(use-package ace-window :demand t :ensure t
  :bind
  ("M-o" . ace-window)
  :init
  (setq aw-scope 'frame))

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
   ("M-."     . helm-etags-select)
   
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
  :init
  (setq rg-custom-type-aliases '())
  :config
  (rg-enable-default-bindings)

  (if IS-WINDOWS (defun rg-executable () (executable-find "rg.exe"))))

(use-package elisp-mode
  :interpreter ("emacs" . emacs-lisp-mode)
  :defines (emacs-lisp-mode-hook)
  :mode (("\\.el$" . emacs-lisp-mode)
         ("Cask"   . emacs-lisp-mode))
  :config
  (use-package macrostep :ensure t
    :commands macrostep-expand
    :mode ("\\*.el\\'" . emacs-lisp-mode)))

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
  :hook ((cargo-minor-mode) . rust-mode)
  
  :init 
  (setq
   rust-indent-offset  4
   rust-format-on-save nil))

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

(use-package ssh-agency :if IS-WINDOWS :ensure t
  :commands ssh-agency-ensure
  :init
  (setq
   ssh-agency-keys (list (expand-file-name "~/.ssh/github_rsa") (expand-file-name "~/.ssh/bamtech"))
   ssh-agency-add-executable "c:/WINDOWS/System32/OpenSSH/ssh-add.exe"
   ssh-agency-agent-executable "c:/WINDOWS/System32/OpenSSH/ssh-agent.exe")  

  :config
  (setenv "SSH_ASKPASS" "git-gui--askpass")
  (ssh-agency-ensure))

;;  [ ] - Add a function to create / open today notes in org
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

;; Disabled because it depends on magit that I'm not using on Windows and it corrupts my setup
(use-package org-roam :demand t :disabled t
  :load-path "vendor/org-roam"
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

(use-package lsp-mode :ensure t :disabled t
  :commands lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (((c-mode c++-mode rust-mode) . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :bind
  (:map lsp-mode-map
   ("C-c C-d" . lsp-describe-thing-at-point)
   ([remap xref-find-definitions] . lsp-find-definition)
   ([remap xref-find-references] . lsp-find-references))
  :custom-face
  (lsp-face-highlight-textual
   ((t :underlying (:style line :color ,(face-foreground 'default)))))
  :config
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

(mapc
 (lambda (mode)
   (font-lock-add-keywords ;;`font-lock-keywords`
    mode
    '(("#\\<\\(TODO\\)\\>" 1 '(error :underline t) t)
      ("#\\<\\(NOTE\\)\\>" 1 '(warning :underline t) t))))
 '(emacs-lisp-mode lua-mode scala-mode c-mode objc-mode c++-mode rust-mode))

(make-variable-buffer-local 'compile-command)
