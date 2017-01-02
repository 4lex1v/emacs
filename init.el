(setq gc-cons-threshold          10000000
      package--init-file-ensured t)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "foundation/use-package" user-emacs-directory))

  (let ((pre-load-folders '("boot" "vendor"))
        (core-dir          (concat user-emacs-directory "core/")))
    (mapc #'(lambda (folder) (add-to-list 'load-path (expand-file-name folder core-dir)))
          pre-load-folders))

  (setq use-package-verbose               t
        use-package-enable-imenu-support  t
        use-package-check-before-init     t
        use-package-minimum-reported-time 0.01)

  (require 'use-package))

(use-package async
  :load-path "core/async"
  :config (use-package async-bytecomp))

(use-package bootstrap
  :demand t
  :bind (("RET"     . newline-and-indent)
         ("M-j"     . join-line)
         ("C-a"     . back-to-indentation)
         ("M-m"     . beginning-of-line)
         ("C-S-d"   . 4lex1v/duplicate-line)
         ("M-`"     . other-frame)
         ("C-c r"   . revert-buffer)
         ("C-x \\"  . align-regexp)
         ("<f10>"   . customize-themes)
         
         ("C-q"     . 4lex1v/close-buffer)
         ("M-q"     . 4lex1v:w/close-other-window)
         ("C-;"     . toggle-comment-on-line))

  :init (progn 
          (setq-default tab-width 2
                        cursor-type 'box
                        cursor-in-non-selected-windows 'bar
                        frame-title-format " %@%b% -"
                        linum-format "%3d "  ;; Try dynamic?
                        indent-tabs-mode  nil
                        load-prefer-newer t)

          (setq user-full-name             "Aleksandr Ivanov"
                user-mail-address          "4lex1v@gmail.com"
                show-paren-delay            0.0
                ring-bell-function         'ignore
                initial-major-mode         'scala-mode
                tramp-default-method       "ssh"
                make-backup-files           nil
                auto-save-default           nil
                inhibit-startup-message     t
                initial-scratch-message     nil
                kill-do-not-save-duplicates t
                ad-redefinition-action     'accept
                next-line-add-newlines      t
                desktop-save-mode           t)

          ;; Find a better alternative to basic linum mode
          (global-hl-line-mode t)
          (global-linum-mode   nil)

          (column-number-mode  t)
          (show-paren-mode     t)

          (put 'narrow-to-region 'disabled nil)
          (put 'narrow-to-page 'disabled nil)

          (fset 'yes-or-no-p   'y-or-n-p)

          (if (window-system)
              (progn
                (tooltip-mode -1)
                (tool-bar-mode -1)
                (menu-bar-mode -1)
                (scroll-bar-mode -1)
                
                (use-package themes
                  :load-path "themes"
                  :config (load-theme 'dracula t)))))
  
  :config (progn
            (4lex1v/configure-frame-size 'maximized)
            (4lex1v/transparent-ui 100 100)))

(use-package osx
  :if (eq system-type 'darwin)
  :init (progn
          ;; Should also be active for *nix?
          (use-package exec-path-from-shell) ;; defined in core/vendor

          (setq ns-use-srgb-colorspace       nil
                browse-url-browser-function 'browse-url-default-macosx-browser
                delete-by-moving-to-trash    t
                mac-option-key-is-meta       nil
                mac-command-key-is-meta      t
                mac-command-modifier        'meta
                mac-option-modifier          nil)

          (let* ((brew-bin-path "/usr/local/homebrew/bin")
                 (current-path  (getenv "PATH"))
                 (new-path      (format "%s:%s" brew-bin-path current-path)))

            ;; To find apps, e.g SBT
            (add-to-list 'exec-path brew-bin-path)
            
            ;; For Eshell
            (setenv "PATH" new-path))
          
          (4lex1v/configure-font '("Ayuthaya" :size 18))))

(use-package f :load-path "core/f")

;; Goes before others to correctly load which-key-declare-prefixes
(use-package which-key
  :diminish which-key-mode
  :load-path "core/which-key"
  :init (setq which-key-idle-delay 0.2
              which-key-popup-type 'side-window
              which-key-sort-order 'which-key-prefix-then-key-order)
  :config (progn
            (which-key-setup-side-window-bottom)
            (which-key-mode)))

(use-package helm
  :diminish helm-mode
  :load-path "core/helm/helm-core"
  :commands helm-mode

  :bind* ("C-c h o" . helm-occur) ;; NOTE :: Replace with Swoop?
  :bind (("C-c h"   . helm-command-prefix)
         ("C-h a"   . helm-apropos)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)         
         ("M-x"     . helm-M-x)
         ("M-3"     . helm-mini))
  
  :bind (:map helm-map
              ("<tab>" . helm-execute-persistent-action)
              ("C-i"   . helm-execute-persistent-action)
              ("C-z"   . helm-select-action)
              ("C-o"   . helm-next-source)
              ("M-o"   . helm-previous-source)
              ("C-j"   . helm-buffer-switch-other-window))

  :init (progn
          (setq helm-idle-delay                        0.0
                helm-input-idle-delay                  0.01
                helm-quick-update                      t
                helm-split-window-in-side-p            t
                helm-buffers-fuzzy-matching            t
                helm-move-to-line-cycle-in-source      t
                helm-scroll-amount                     8
                helm-ff-search-library-in-sexp         t
                helm-ff-file-name-history-use-recentf  t
                helm-ag-insert-at-point                'symbol)

          (use-package helm-config)
          (use-package helm-mode))
  
  :config (progn
            (helm-autoresize-mode)

            (which-key-declare-prefixes "C-c h" "helm")

            (use-package helm-descbinds
              :load-path "core/helm/helm-descbinds"
              :commands helm-descbinds
              :init (progn
                      (fset 'describe-bindings 'helm-descbinds)
                      (setq helm-descbinds-window-style 'same-window)))

            (use-package helm-swoop
              :load-path "core/helm/helm-swoop"
              :commands helm-swoop

              :bind (("M-i"     . helm-swoop)
                     ("M-I"     . helm-swoop-back-to-last-point)
                     ("C-c M-i" . helm-multi-swoop)
                     ("C-x M-i" . helm-multi-swoop-all))

              :bind (:map isearch-mode-map
                          ("M-i" . helm-swoop-from-isearch)
                          ("M-I" . helm-multi-swoop-all-from-isearch))

              ;; FIXME :: For whatever reason map is void
              ;; :bind (:map helm-swoop-map
              ;;             ("M-i" . helm-multi-swoop-all-from-helm-swoop)
              ;;             ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop))

              :init (setq helm-multi-swoop-edit-save t
                          helm-swoop-split-with-multiple-windows nil
                          helm-swoop-split-direction 'split-window-vertically
                          helm-swoop-move-to-line-cycle t
                          helm-swoop-use-line-number-face t))

            (use-package helm-ag
              :load-path "core/helm/helm-ag"
              :commands helm-projectile-ag)))

(use-package projectile
  :load-path "core/projectile"
  :commands projectile-project-root
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("M-1" . helm-projectile)
         ("M-4" . projectile-switch-project))

  :init (setq projectile-enable-caching       t
              projectile-require-project-root t
              projectile-use-git-grep         t)

  :config (progn
            (projectile-global-mode)

            (which-key-declare-prefixes
              "C-c p" "projectile")

            (use-package helm-projectile
              :demand t
              :load-path "core/helm/helm-projectile"
              :config (progn
                        (setq projectile-completion-system 'helm)
                        (helm-projectile-on)))

            ;; Stored in core/vendor
            (use-package ibuffer-projectile
              :init (add-hook 'ibuffer-hook
                              (lambda ()
                                (ibuffer-projectile-set-filter-groups)
                                (unless (eq ibuffer-sorting-mode 'alphabetic)
                                  (ibuffer-do-sort-by-alphabetic)))))
            
            (setq projectile-mode-line '(:eval (format " {%s}" (projectile-project-name))))))

(use-package magit
  :load-path "core/magit/lisp"
  :commands magit-clone
  :bind (("C-c m s" . magit-status)
         ("C-c m p" . magit-push-popup)
         ("C-c m f" . magit-pull-popup)
         ("C-c m b" . magit-blame)
         ("C-c m r" . magit-show-refs-popup)
         ("C-c m m" . magit-dispatch-popup)
         ("C-c m o" . magit-submodule-popup))

  :init (progn
          (use-package with-editor :load-path "core/with-editor")

          (unbind-key "C-c m")

          (which-key-declare-prefixes "C-c m" "magit")

          (setq magit-last-seen-setup-instructions "2.3.2")))

(use-package ranger
  :load-path "core/ranger"
  :bind (("M-2" . ranger)
         ("M-5" . helm-ranger-bookmarks))
  :init (setq ranger-override-dired  nil
              ranger-show-literal    nil ;; Turn on highlighting in ranger mode
              ranger-cleanup-eagerly t
              ranger-show-dotfiles   t)

  :config (defun helm-ranger-bookmarks ()
            (interactive)
            (helm :sources (helm-build-in-buffer-source "Ranger Bookmarks"
                             :data (lambda ()
                                     (bookmark-maybe-load-default-file)
                                     (ranger--directory-bookmarks))
                             :fuzzy-match t
                             :action 'ranger)
                  :buffer "*helm ranger bookmarks*")))

(use-package ace-control
  :load-path "core/ace"
  :init (progn

          ;; Improved version of ace-jump-mode
          (use-package avy
            :bind (("C-c SPC" . avy-goto-char)
                   ("C-c j w" . avy-goto-word-1)
                   ("C-c j l" . avy-goto-line)))

          (use-package ace-window
            :bind (("C-'"  . ace-window)
                   ("<f7>" . ace-window)))

          (which-key-declare-prefixes "C-c j" "ace-jump")))

(use-package smartparens
  :diminish smartparens-mode
  :load-path "core/smartparens"
  :commands smartparens-mode

  :bind (:map sp-keymap
              ("M-F"              . sp-forward-symbol)
              ("M-B"              . sp-backward-symbol)

              ("C-M-k"            . sp-kill-sexp)
              ("C-M-w"            . sp-copy-sexp)
              ("C-M-t"            . sp-transpose-sexp)
              
              ("M-<delete>"       . sp-unwrap-sexp)
              ("M-<backspace>"    . sp-backward-unwrap-sexp)

              ("M-<left>"         . sp-forward-slurp-sexp)
              ("C-M-<left>"       . sp-forward-barf-sexp)
              ("M-<right>"        . sp-backward-slurp-sexp)
              ("C-M-<right>"      . sp-backward-barf-sexp)

              ("M-D"              . sp-splice-sexp)
              ("C-M-<delete>"     . sp-splice-sexp-killing-forward)
              ("C-M-<backspace>"  . sp-splice-sexp-killing-backward)
              ("C-S-<backspace>"  . sp-splice-sexp-killing-around)

              ("C-M-["            . sp-select-previous-thing)
              ("C-M-]"            . sp-select-next-thing)

              ("C-c s u"          . sp-up-sexp)
              ("C-c s d"          . sp-down-sexp)
              ("C-c s t"          . sp-prefix-tag-object)
              ("C-c s p"          . sp-prefix-pair-object)
              ("C-c s c"          . sp-convolute-sexp)
              ("C-c s a"          . sp-absorb-sexp)
              ("C-c s e"          . sp-emit-sexp)
              ("C-c s p"          . sp-add-to-previous-sexp)
              ("C-c s n"          . sp-add-to-next-sexp)
              ("C-c s j"          . sp-join-sexp)
              ("C-c s s"          . sp-split-sexp))

  :init (progn
          (use-package smartparens-config
            :init (setq sp-autoinsert-if-followed-by-word t
                        sp-autoskip-closing-pair 'always-end
                        sp-hybrid-kill-entire-symbol nil))

          (4lex1v/hook-into-modes #'smartparens-mode
                                  'scala-mode-hook
                                  'emacs-lisp-mode-hook
                                  'clojure-mode-hook
                                  'cider-repl-mode-hook))

  :config (progn

            (sp-pair "(" ")" :wrap "C-(")
            (sp-pair "[" "]" :wrap "s-[")
            (sp-pair "{" "}" :wrap "C-{")

            (which-key-declare-prefixes
              "C-c s" "smartparens")))

(use-package company
  :diminish company-mode
  :load-path "core/company"
  :commands global-company-mode

  :bind (("M-&" . company-complete))

  :init (progn
          (setq company-dabbrev-ignore-case nil
                company-dabbrev-code-ignore-case nil
                company-dabbrev-downcase nil
                company-idle-delay 0
                company-minimum-prefix-length 4)

          (4lex1v/hook-into-modes #'global-company-mode
                                  'scala-mode-hook
                                  'emacs-lisp-mode-hook)))

(use-package yasnippet
  :diminish yas-minor-mode
  :load-path "core/yasnippet"
  :commands yas-minor-mode

  :init (progn
          (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
          (4lex1v/hook-into-modes #'yas-minor-mode 'scala-mode-hook))

  :config (progn
            (yas-reload-all)
            (which-key-declare-prefixes "C-c &" "yasnippet")))

(use-package hideshowvis
  :diminish hs-minor-mode
  :commands hideshowvis-enable

  :bind (("M-[" . hs-hide-block)
         ("M-]" . hs-show-block))

  :init (progn
          (which-key-declare-prefixes
            "C-c @" "hideshow")

          (push '(scala-mode "\\({\\|(\\)" "\\(}\\|)\\)" "/[*/]" nil nil) hs-special-modes-alist)
          (let ((modes '(emacs-lisp-mode-hook scala-mode-hook)))
            (apply #'4lex1v/hook-into-modes #'hs-minor-mode modes)
            (apply #'4lex1v/hook-into-modes #'hideshowvis-enable modes)))
  
  :config (progn
            (hideshowvis-symbols)
            (hideshowvis-enable)))

(use-package elisp-mode
  :diminish (emacs-lisp-mode . "ELisp")
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("\\.el$" . emacs-lisp-mode)

  :bind (("M-." . find-function-at-point)
         ("M-," . find-variable-at-point))

  :init (use-package macrostep
          :load-path "packages/elisp/macrostep"
          :commands macrostep-expand
          :bind (:map emacs-lisp-mode-map
                      ("C-c e" . macrostep-expand))))

(use-package scala
  :load-path "packages/scala"
  :init (use-package scala-mode
          :commands scala-mode
          :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode)
          :interpreter ("scala" . scala-mode)

          :init (setq scala-indent:use-javadoc-style t)

          :bind (:map scala-mode-map
                      ("C-c b"      . sbt-ext:open-build-file)
                      ("<C-return>" . newline-or-comment)
                      ("M-j"        . scala-indent:join-line)
                      ("C-c c"      . sbt-command))

          :config (progn
                    (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
                    (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

                    (use-package sbt-mode :commands (sbt-start sbt-command))

                    (use-package ensime
                      :commands ensime
                      :bind (:map scala-mode-map
                                  ("C-c e" . ensime-print-errors-at-point)
                                  ("C-c t" . ensime-print-type-at-point)
                                  ("C-c o" . ensime-import-type-at-point)
                                  ("C-M-." . ensime-edit-definition-other-window))

                      :init (progn
                              (setq ensime-server-version "1.0.0"
                                    ensime-startup-notification nil)

                              (which-key-declare-prefixes-for-mode 'scala-mode
                                "C-c C-d" "ensime/debug"
                                "C-c C-c" "ensime/compiler"
                                "C-c C-r" "ensime/refactoring"
                                "C-c C-t" "ensime/tests"
                                "C-c C-v" "ensime/general"
                                "C-c C-b" "ensime/sbt")
                              
                              (use-package popup :load-path "core/popup")
                              (setq ensime-default-buffer-prefix "ENSIME-")

                              ;; Smart Ensime loader
                              (4lex1v/hook-into-modes
                               #'(lambda () 
                                   (if (and (if-bound-f projectile-project-p)
                                            (if-bound-f projectile-project-root))
                                       (4lex1v/connect-running-ensime (projectile-project-root))
                                     (progn
                                       (message (format "Projectile is not loaded, using %s" default-directory))
                                       (4lex1v/connect-running-ensime default-directory))))
                               'scala-mode-hook)

                              ;; Eldoc support in Scala + Ensime mode
                              (setq-local eldoc-documentation-function
                                          #'(lambda ()
                                              (when (ensime-connected-p)
                                                (let ((err (ensime-print-errors-at-point)))
                                                  (or (and err (not (string= err "")) err)
                                                      (ensime-print-type-at-point)))))))

                      :config (unbind-key "M-p" ensime-mode-map))
                    
                    (4lex1v/hook-into-modes #'4lex1v/fix-scala-fonts 'scala-mode-hook))))



;; Configuration inspired by -
;; https://github.com/flyingmachine/emacs-for-clojure/blob/master/customizations/setup-clojure.el
(use-package clojure
  :load-path "packages/clojure"
  :init (progn
          (use-package clojure-mode
            :commands clojure-mode
            :mode (("\\.clj\\'" . clojure-mode)
                   ("\\.edn\\'" . clojure-mode)
                   ("\\.boot\\'" . clojure-mode)
                   ("\\.cljs.*\\'" . clojure-mode)
                   ("lein-env" . ruby-mode))

            :bind (("C-c C-v" . cider-start-http-server)
                   ("C-M-r"   . cider-refresh)
                   ("C-c u"   . cider-user-ns))

            :init (setq inferior-lisp-program "lein repl")

            :config (progn
                      (use-package clojure-mode-extra-font-locking
                        :init (font-lock-add-keywords
                               nil
                               '(("(\\(facts?\\)"
                                  (1 font-lock-keyword-face))
                                 ("(\\(background?\\)"
                                  (1 font-lock-keyword-face)))))
                      
                      (define-clojure-indent (fact 1))
                      (define-clojure-indent (facts 1))))

          (use-package cider
            :commands cider-mode
            :bind (("C-c u" . cider-user-ns))

            :init (progn
                    (4lex1v/hook-into-modes #'cider-mode 'clojure-mode-hook)
                    (4lex1v/hook-into-modes #'eldoc-mode 'cider-mode-hook)

                    (setq cider-repl-pop-to-buffer-on-connect t ;; go right to the REPL buffer when it's finished connecting
                          cider-show-error-buffer t ;; When there's a cider error, show its buffer and switch to it
                          cider-auto-select-error-buffer t
                          cider-repl-history-file "~/.emacs.d/cider-history"
                          cider-repl-wrap-history t)))))

(use-package web-mode
  :load-path "packages/web"

  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.php\\'" . web-mode))

  :init (progn
          (setq web-mode-engines-alist '(("\\.jinja\\'"  . "django"))
                web-mode-enable-auto-pairing t
                web-mode-enable-css-colorization t
                web-mode-enable-current-element-highlight t
                web-mode-enable-current-column-highlight nil)

          (which-key-declare-prefixes-for-mode 'web-mode
            "C-c C-t" "web/tag"
            "C-c C-b" "web/block"
            "C-c C-d" "web/dom"
            "C-c C-a" "web/attribute"
            "C-c C-e" "web/element")))

(use-package docker
  :load-path "core/docker"
  :commands docker-images

  :init (setq default-docker-machine-name "universe")
  
  :config (progn
            (defun docker-machine-connect (name)
              "Connects to a running machine by its `name'"
              (interactive "sDocker-machine name: ")
              (let ((docker-env
                     (mapcar
                      #'(lambda (line)
                          (split-string-and-unquote
                           (replace-regexp-in-string "\\(export \\|=\\)" " " line)))
                      (-filter
                       #'(lambda (line) (string/starts-with line "export"))
                       (split-string
                        (shell-command-to-string
                         (concat "docker-machine env " name)) "\n" t)))))
                (mapc
                 #'(lambda (params) (apply 'setenv params))
                 docker-env)))

            (if (eq (shell-command
                     (concat "docker-machine env "
                             default-docker-machine-name))
                    0)
                (docker-machine-connect default-docker-machine-name))))

(use-package centered-cursor-mode
  :diminish centered-cursor-mode
  :init (setq ccm-recenter-at-end-of-file t
              ccm-ignored-commands '(mouse-drag-region
                                     mouse-set-point
                                     widget-button-click
                                     scroll-bar-toolkit-scroll))
  :config (global-centered-cursor-mode t))

(use-package yaml-mode
  :load-path "packages/yaml"
  :mode ("\\.yml$" . yaml-mode))

;; (use-package idris-mode
;;   :load-path "packages/idris")

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :init (progn
          (which-key-declare-prefixes "C-c i" "flyspell")
          
          (use-package ispell
            :bind (("C-c i c" . ispell-comments-and-strings)
                   ("C-c i d" . ispell-change-dictionary)
                   ("C-c i k" . ispell-kill-ispell)
                   ("C-c i m" . ispell-message)
                   ("C-c i r" . ispell-region))))
  :config (unbind-key "C-." flyspell-mode-map))

(use-package undo-tree
  :load-path "core/undo-tree"
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("M-/" . undo-tree-visualize))

;; just for now
(use-package cmake-mode
  :load-path "packages/native/cmake")

(use-package haskell-mode
  :load-path "packages/haskell/haskell-mode")

(use-package markdown-mode
  :load-path "packages/markdown-mode"
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package jabber
  :load-path "packages/emacs-jabber"
  :init (setq jabber-account-list
              `(("4lex1v@livecoding.tv"
                 (:password . ,(exec-path-from-shell-getenv "LC_PWD"))))))

(use-package org
  :load-path "packages/org/org-mode"

  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o c" . org-capture))

  :init (setq org-log-done             t
              org-src-fontify-natively t
              org-babel-load-languages '((emacs-lisp . t)
                                         (scala      . t)
                                         (haskell    . t))))

(setq gc-cons-threshold 100000)
