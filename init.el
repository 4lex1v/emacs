;; Preloaded libraries
(defconst preloaded '("use-package" "cask" "pallet"))

(eval-and-compile
  (mapc #'(lambda (path)
            (let ((full-path (concat "core/" path)))
              (add-to-list 'load-path (expand-file-name full-path user-emacs-directory))))
        preloaded)
  (require 'use-package))

(use-package cask
  :config
  (cask-initialize)
  (add-to-list 'auto-mode-alist '("\\Cask\\'" . emacs-lisp-mode))

	(use-package package
		:config
		(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
		(package-initialize nil))

  (use-package pallet :config (pallet-mode)))

(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))

;; General configurations
(use-package ui)
(use-package general)
(use-package functions)
(use-package keys)

;; Here comes the packages
(use-package helm-config
  :demand t
  :bind* ("C-c h o" . helm-occur)
  :bind (("C-c h"   . helm-command-prefix)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-1"     . helm-projectile)
         ("M-x"     . helm-M-x))

  :config
  (use-package helm-mode
    :diminish helm-mode
    :init
    (helm-mode 1))
    
  (helm-autoresize-mode)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-quick-update                      t ; do not display invisible candidates
        helm-split-window-in-side-p            t ; open helm buffer inside current window, not occupy whole other window
        helm-buffers-fuzzy-matching            t ; fuzzy matching buffer names when non--nil
        helm-move-to-line-cycle-in-source      t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp         t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                     8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf  t)

  ;; Place under :bind when key-maps would be supported
  (bind-key "<tab>" 'helm-execute-persistent-action  helm-map) ; rebihnd tab to do persistent action
  (bind-key "C-i"   'helm-execute-persistent-action  helm-map) ; make TAB works in terminal
  (bind-key "C-z"   'helm-select-action              helm-map) ; list actions using C-z
  (bind-key "C-o"   'helm-next-source                helm-map)
  (bind-key "M-o"   'helm-previous-source            helm-map)
  (bind-key "C-x o" 'helm-buffer-switch-other-window helm-map)

  (use-package helm-descbinds :bind ("C-c h d" . helm-descbinds)))

(use-package projectile
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (projectile-global-mode)
  :config
  (push ".cask" projectile-globally-ignored-directories)
  (setq projectile-enable-caching              t
        projectile-require-project-root        t)

  (use-package ibuffer-projectile
    :config
    (setq ibuffer-default-sorting-mode     'major-mode
          ibuffer-show-empty-filter-groups  nil)
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic)))))

  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

(use-package neotree
  :demand t
  :commands neotree
  :bind ("<f8>" . 4lex1v/neotree-projectile-toggle)
  :init
  ;; Toogle neotree buffer for current projectile root
  (defun 4lex1v/neotree-projectile-toggle ()
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (neotree-projectile-action))))

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-char-mode)
         ("C-c j c" . ace-jump-char-mode)
         ("C-c j w" . ace-jump-word-mode)
         ("C-c j l" . ace-jump-line-mode)))

(use-package haskell-mode
  :defer t
  :config
  (add-to-list 'completion-ignored-extensions ".hi")
  (add-hook 'haskell-mode-hook
            (lambda ()
              (turn-on-haskell-indent)
              (turn-on-font-lock)
              (turn-on-eldoc-mode)
              (turn-on-haskell-doc-mode)
              (turn-on-haskell-unicode-input-method)
              (interactive-haskell-mode))))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (hook-into-modes #'yas-minor-mode 'scala-mode-hook)
  (yas-reload-all))

(use-package multiple-cursors)

(use-package smartparens-config
  :diminish smartparens-mode
  :config
  (setq smartparens-strict-mode t
        sp-autoinsert-if-followed-by-word t
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  (hook-into-modes #'smartparens-mode 'scala-mode-hook 'emacs-lisp-mode-hook))

(use-package hlinum
  :defer t)

(use-package company
  :diminish company-mode
  :config
  (global-company-mode)

  (use-package helm-company
    :config
    (bind-key "C-:" 'helm-company company-mode-map)
    (bind-key "C-:" 'helm-company company-active-map)))

(use-package magit
  :defer t
  :diminish magit-auto-revert-mode
  :bind (("C-c m s" . magit-status)
         ("C-c m b" . magit-branch-manager))
  :init
  (unbind-key "C-c m")
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package scala-mode2
  :commands scala-mode
  :init
  ;; Load ensime mode for scala only if there is an ensime
  ;; project file .ensime defined in the root directory
  (defun 4lex1v/ensime-project-p ()
    (let* ((root-dir (projectile-project-root))
           (ensime-project-file (concat root-dir ".ensime")))
      (file-exists-p ensime-project-file)))

  (defun sbt-ext:open-build-file ()
    (interactive)
    (let ((sbt-build-file (concat (projectile-project-root) "build.sbt")))
      (if (file-exists-p sbt-build-file)
          (find-file sbt-build-file)
        (error "build.sbt is not defined"))))

  :config
  ;; Insert * if in the middle of the comment
  (defun newline-or-comment ()
    (interactive)
    (indent-new-comment-line)
    (scala-indent:insert-asterisk-on-multiline-comment))

  ;; Indent new line between braces with smartparens mode
  (defun 4lex1v/indent-in-braces (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (add-hook 'scala-mode-hook
            #'(lambda ()
                (if (4lex1v/ensime-project-p)
                    (ensime-mode 1))))

  (bind-key "C-c b"      'sbt-ext:open-build-file scala-mode-map)
  (bind-key "<C-return>" 'newline-or-comment      scala-mode-map)
  (bind-key "M-j"        'scala-indent:join-line  scala-mode-map)
  
  (setq scala-indent:use-javadoc-style t
        popup-complete-enabled-modes '(scala-mode))

  (sp-local-pair 'scala-mode "{" nil :post-handlers '((4lex1v/indent-in-braces "RET")))
  
  (use-package sbt-mode :commands sbt-start)
  
  (use-package ensime
    :commands ensime-mode
    :init
    (setq ensime-default-buffer-prefix "ENSIME-")

    (defun 4lex1v/start-ensime ()
      (interactive)
      (if (4lex1v/ensime-project-p)
          (let ((port-file (concat (projectile-project-root) ".ensime_cache/port")))
            (if (file-exists-p port-file) (delete-file port-file))
            (ensime))
        (message "Not an ENSIME project")))

    :config
    (bind-key "C-c e" 'ensime-print-errors-at-point scala-mode-map)
    (bind-key "C-c t" 'ensime-print-type-at-point   scala-mode-map)
    (bind-key "C-c i" 'ensime-import-type-at-point  scala-mode-map)
    (bind-key "C-M-." 'ensime-edit-definition-other-window scala-mode-map)))

(use-package web-mode
  :defer t
  :mode "\\.html?\\'"
  :config
  (use-package company-web-html))

(use-package org-mode
  :defer t
  :config
  (setq org-directory "~/Notes"))

(use-package shell-mode
  :defer t
  :config
  (defun shell-clear ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))
  (bind-key "C-c k" 'shell-clear shell-mode-map))

(use-package help+
  :defer t
  :config
  (use-package help-fns+)
  (use-package help-mode+))

(use-package hideshow
  :diminish hs-minor-mode
  :bind (("C-c [" . hs-hide-block)
         ("C-c ]" . hs-show-block))
  :config
  (hook-into-modes #'hs-minor-mode
                   'less-css-mode-hook
                   'js-mode-hook
                   'scala-mode-hook
                   'emacs-lisp-mode-hook))

(use-package er/expand-region
  :bind ("C-=" . er/expand-region))
  
(add-hook 'dired-load-hook (lambda () (load "dired-x")))
