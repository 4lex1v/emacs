
;; Goes before others to correctly load which-key-declare-prefixes
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
    "$"              'evil-end-of-visual-line
    (kbd "C-j")      'evil-forward-paragraph
    (kbd "C-k")      'evil-backward-paragraph
    (kbd "<M-wheel-up>")   'text-scale-increase
    (kbd "<M-wheel-down>") 'text-scale-decrease
    (kbd "C-S-o")          'evil-jump-forward
    "m"              'back-to-indentation)

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
    (add-to-list 'eshell-visual-commands "htop")))

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
  (kbd "<leader> ps") 'project-search
  (kbd "<leader> pc") '4l/project-compile

  ;; Navigation
  (kbd "<leader> fi")  '(lambda () (interactive) (find-file (concat USER-EMACS-DIRECTORY "init.el"))))

(evil-define-key nil global-map
  (kbd "C-c r") 'revert-buffer)
