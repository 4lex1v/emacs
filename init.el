;; Preloaded libraries
(message "Loading emacs...")

(eval-and-compile
  (mapc #'(lambda (path) (add-to-list 'load-path (expand-file-name path user-emacs-directory)))
        '("core/use-package" "configs"))

  (require 'use-package)

  (use-package ui)
  (use-package general)
  (use-package functions)
  (use-package keys))

(use-package cask
  :load-path "core/cask"
  :config
  (cask-initialize)

  (use-package package
    :config
    (package-initialize nil)
    (message "Package manager loaded"))

  (use-package pallet
    :load-path "core/pallet"
    :config
    (pallet-mode)
    (message "Pallet package loaded"))

  (message "Cask package loaded"))

(use-package projectile
  :demand t
  :load-path "packages/projectile"
  :bind-keymap ("C-c p" . projectile-command-map)

  :init
  (setq projectile-enable-caching              t
        projectile-require-project-root        t)

  :config
  (projectile-global-mode)
  (push ".cask" projectile-globally-ignored-directories)

  (message "Projectile package loaded"))

(use-package helm
  :demand t
  :diminish helm-mode
  :load-path "packages/helm"
  :bind* ("C-c h o" . helm-occur)
  :bind (("C-c h"   . helm-command-prefix)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)         
         ("M-x"     . helm-M-x))
  :init
  (use-package helm-config :config (message "Helm-config loaded"))
  (use-package helm-mode :config (helm-mode 1) (message "Helm-mode loaded"))
  
  (setq helm-quick-update                      t ; do not display invisible candidates
        helm-split-window-in-side-p            t ; open helm buffer inside current window, not occupy whole other window
        helm-buffers-fuzzy-matching            t ; fuzzy matching buffer names when non--nil
        helm-move-to-line-cycle-in-source      t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp         t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                     8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf  t
        helm-ag-insert-at-point                'symbol)

  :config
  (helm-autoresize-mode)
  
  ;;  Place under :bind when key-maps would be supported
  (bind-key "<tab>" 'helm-execute-persistent-action  helm-map) ; rebihnd tab to do persistent action
  (bind-key "C-i"   'helm-execute-persistent-action  helm-map) ; make TAB works in terminal
  (bind-key "C-z"   'helm-select-action              helm-map) ; list actions using C-z
  (bind-key "C-o"   'helm-next-source                helm-map)
  (bind-key "M-o"   'helm-previous-source            helm-map)
  (bind-key "C-j"   'helm-buffer-switch-other-window helm-map)

  (use-package helm-descbinds
    :bind ("C-c h d" . helm-descbinds))

  (use-package helm-projectile
    :demand t
    :load-path "packages/projectile"
    :bind ("M-1" . helm-projectile)
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (message "Helm-projectile loaded"))

  (message "Helm loaded"))

(use-package magit
  :defer t
  :load-path "packages/magit/lisp"
  :bind (("C-c m s" . magit-status)
         ("C-c m b" . magit-show-refs-popup)
         ("C-c m m" . magit-dispatch-popup))
  :init
  (unbind-key "C-c m")
  (setq magit-last-seen-setup-instructions "2.1.0"))

(use-package smartparens
  :diminish smartparens-mode
  :init
  (use-package smartparens-config
    :init
    (setq sp-autoinsert-if-followed-by-word t
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil))

  :config
  (hook-into-modes #'smartparens-mode
                   'scala-mode-hook
                   'emacs-lisp-mode-hook
                   'haskell-mode-hook))

(use-package company
  :diminish company-mode
  :load-path "packages/company"
  :config
  (global-company-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :load-path "packages/yasnippet"
  :commands yas-minor-mode
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (hook-into-modes #'yas-minor-mode 'scala-mode-hook)
  (yas-reload-all))


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

  :config  
  (add-hook 'scala-mode-hook
            #'(lambda ()
                (if (4lex1v/ensime-project-p)
                    (ensime-mode 1))))

  (add-hook 'scala-mode-hook 'hs-minor-mode)

  (bind-key "C-c b"      'sbt-ext:open-build-file scala-mode-map)
  (bind-key "<C-return>" 'newline-or-comment      scala-mode-map)
  (bind-key "M-j"        'scala-indent:join-line  scala-mode-map)

  (setq scala-indent:use-javadoc-style t
        popup-complete-enabled-modes '(scala-mode))

  (sp-local-pair 'scala-mode "{" nil :post-handlers '((4lex1v/indent-in-braces "RET")))
  
  (use-package sbt-mode :commands sbt-start)
  
  (use-package ensime
    :load-path "packages/ensime"
    :commands ensime-mode
    :init
    (setq ensime-default-buffer-prefix "ENSIME-")

    (defun 4lex1v/start-ensime ()
      (interactive)
      (if (4lex1v/ensime-project-p)
          (let ((port-file (concat (projectile-project-root) ".ensime_cache/port"))
                (config-file (concat (projectile-project-root) ".ensime")))
            (if (file-exists-p port-file) (delete-file port-file))
            (ensime--1 config-file))
        (message "Not an ENSIME project")))

    (defun 4lex1v/ensime-cleanup ()
      (interactive)
      (if (4lex1v/ensime-project-p)
          (let ((ensime-file (concat (projectile-project-root) ".ensime"))
                (ensime-cache-folder (concat (projectile-project-root) ".ensime_cache")))
            ;; Drop ensime cache folder
            (if (file-exists-p ensime-cache-folder) (delete-directory ensime-cache-folder t))
            (if (file-exists-p ensime-file) (delete-file ensime-file)))))

    :config
    (bind-key "C-c e" 'ensime-print-errors-at-point scala-mode-map)
    (bind-key "C-c t" 'ensime-print-type-at-point   scala-mode-map)
    (bind-key "C-c i" 'ensime-import-type-at-point  scala-mode-map)
    (bind-key "C-M-." 'ensime-edit-definition-other-window scala-mode-map)
    (unbind-key "M-p" ensime-mode-map)
    (message "Ensime mode loaded"))

  (message "Scala-mode loaded"))

;; (use-package neotree
;;   :demand t
;;   :commands neotree
;;   :bind ("<f8>" . 4lex1v/neotree-projectile-toggle)
;;   :init
;;   ;; Toogle neotree buffer for current projectile root
;;   (defun 4lex1v/neotree-projectile-toggle ()
;;     (interactive)
;;     (if (neo-global--window-exists-p)
;;         (neotree-hide)
;;       (neotree-projectile-action))))

;; (use-package ace-jump-mode
;;   :bind (("C-c SPC" . ace-jump-char-mode)
;;          ("C-c j c" . ace-jump-char-mode)
;;          ("C-c j w" . ace-jump-word-mode)
;;          ("C-c j l" . ace-jump-line-mode)))

;; (use-package haskell-mode
;;   :defer t
;;   :init (add-to-list 'completion-ignored-extensions ".hi")
;;   :config

;;   (bind-key "C-`"     'haskell-interactive-bring      haskell-mode-map)
;;   (bind-key "C-c C-l" 'haskell-process-load-or-reload haskell-mode-map)

;;   (use-package haskell-interactive-mode :init (hook-into-modes #'haskell-mode-hook 'haskell-interactive-mode))
;;   (use-package haskell-process)
  
;;   (hook-into-modes #'haskell-mode-hook 'haskell-unicode)

;;   (custom-set-variables
;;    '(haskell-process-suggest-remove-import-lines t)
;;    '(haskell-process-auto-import-loaded-modules t)
;;    '(haskell-process-log t)
;;    '(haskell-process-type 'cabal-repl))

;;   (add-hook 'haskell-mode-hook
;;             (lambda ()
;;               (turn-on-haskell-indent)
;;               (turn-on-font-lock)
;;               (turn-on-eldoc-mode)
;;               (turn-on-haskell-doc-mode)
;;               (turn-on-haskell-unicode-input-method)
;;               (interactive-haskell-mode))))


;; (use-package multiple-cursors)


;; (use-package hlinum
;;   :defer t)

;; (use-package web-mode
;;   :defer t
;;   :mode "\\.html?\\'"
;;   :config
;;   (use-package company-web-html))

;; ;; OCaml language configuration
;; (use-package opam
;;   :init (opam-init))

;; (use-package tuareg
;;   :defer t
;;   :config

;;   (use-package utop
;;     :defer t
;;     :init
;;     (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
;;     (add-hook 'tuareg-mode-hook 'utop-minor-mode))

;;   (use-package merlin
;;     :defer t
;;     :init
;;     (add-hook 'tuareg-mode-hook 'merlin-mode)
;;     (use-package company)
;;     (add-to-list 'company-backends 'merlin-company-backend)

;;     :config
;;     (setq merlin-use-auto-complete-mode 'easy
;;           merlin-command 'opam))

;;   (use-package ocp-indent
;;     :init
;;     (setq tuareg-use-smie nil)))

;; (use-package org-mode
;;   :defer t
;;   :config
;;   (setq org-directory "~/Notes"))

;; (use-package shell-mode
;;   :defer t
;;   :config
;;   (defun shell-clear ()
;;     (interactive)
;;     (let ((comint-buffer-maximum-size 0))
;;       (comint-truncate-buffer)))
;;   (bind-key "C-c k" 'shell-clear shell-mode-map))

;; (use-package help+
;;   :defer t
;;   :config
;;   (use-package help-fns+)
;;   (use-package help-mode+))

;; ;;(use-package emacs-lisp-mode :init (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode))

;; (use-package hideshow
;;   :diminish hs-minor-mode
;;   :bind (("M-[" . hs-hide-block)
;;          ("M-]" . hs-show-block))
;;   :config
;;   (push '(scala-mode "\\({\\|(\\)" "\\(}\\|)\\)" "/[*/]" nil nil) hs-special-modes-alist)) 
  
;; (use-package hideshowvis
;;   :init (hideshowvis-symbols)
;;   :config (hideshowvis-enable))

;; (use-package er/expand-region :bind ("C-=" . er/expand-region))

;; (use-package guide-key
;;   :diminish guide-key-mode
;;   :init (setq guide-key/idle-delay 0.3
;;               guide-key/guide-key-sequence t
;;               guide-key/recursive-key-sequence-flag t)
;;   :config
;;   (guide-key-mode))

;; (use-package js2-mode
;;   :mode (("\\.js$" . js2-mode)
;;          ("\\.json$" . j2-more)
;;          ("Jakefile$" . js2-mode))
;;   :interpreter ("node" . js2-mode)
;;   :config
;;   (add-hook 'js2-mode-hook 'hs-minor-mode)
;;   (setq-default js2-basic-offset 2))

;; (use-package ace-window
;;   :defer t
;;   :bind ("M-p" . ace-window))
