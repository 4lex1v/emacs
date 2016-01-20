;; Preloaded libraries
(message "Loading emacs...")

;; Change back after config has been loaded
(setq gc-cons-threshold 10000000)

;; Add bootstrap libs to the load path 
(add-to-list 'load-path (concat user-emacs-directory "core/boot"))
(add-to-list 'load-path (concat user-emacs-directory "core/vendor"))
(add-to-list 'load-path (concat user-emacs-directory "core/use-package"))

(eval-when-compile (require 'use-package))
(setq use-package-verbose t
      use-package-minimum-reported-time 0.01)

;; UI config goes first, if any subsequent config fails
;; at least we can have a pretty UI to work with emacs...
(use-package bootstrap
  :demand t
  :bind (("RET"         . newline-and-indent)
         ("M-j"         . join-line)
         ("C-x C-b"     . ibuffer)
         ("C-a"         . back-to-indentation)
         ("M-m"         . beginning-of-line)
         ("C-S-d"       . 4lex1v/duplicate-line)
         ("M-`"         . other-frame)
         ("C-c r"       . revert-buffer)
         ("C-c C-d"     . 4lex1v/delete-current-file)
         ("C-x \\"      . align-regexp)

         ("<f9>"        . open-init-file)
         ("<f10>"       . customize-themes)
         
         ("C-+"         . text-scale-increase)
         ("C--"         . text-scale-decrease)
         
         ("C-q"         . 4lex1v/close-buffer)
         ("M-q"         . 4lex1v:w/close-other-window)
         ("C-;"         . toggle-comment-on-line))

  :init (progn 
          (tooltip-mode -1)
          (tool-bar-mode -1)
          (menu-bar-mode -1)
          (scroll-bar-mode -1)
          
          (setq-default tab-width 2
                        cursor-type 'box
                        cursor-in-non-selected-windows 'bar
                        frame-title-format " %@%b% -"
                        linum-format "%3d "  ;; Try dynamic?
                        indent-tabs-mode  nil)

          (setq user-full-name             "Aleksandr Ivanov"
                user-mail-address          "4lex1v@gmail.com"
                show-paren-delay            0.0
                ring-bell-function         'ignore
                initial-major-mode         'emacs-lisp-mode
                tramp-default-method       "ssh"
                make-backup-files           nil
                auto-save-default           nil
                inhibit-startup-message     t
                initial-scratch-message     nil
                kill-do-not-save-duplicates t
                ad-redefinition-action     'accept
                next-line-add-newlines      nil)

          ;; Find a better alternative to basic linum mode
          (global-hl-line-mode t)
          (global-linum-mode   nil)

          (column-number-mode  t)
          (show-paren-mode     t)

          (put 'narrow-to-region 'disabled nil)
          (put 'narrow-to-page 'disabled nil)

          (fset 'yes-or-no-p   'y-or-n-p))

  :config (progn 
            ;;(4lex1v:ui/transparent-ui 95 95)
            ;;(4lex1v/configure-frame-size 'maximized)
            ))

(use-package solarized
  :demand t
  :if window-system
  :load-path "themes/solarized-emacs"
  
  :init (setq solarized-contrast                   'high
              solarized-visibility                 'high
              solarized-termcolors                  256
              solarized-distinct-fringe-background  nil
              solarized-use-variable-pitch          nil
              solarized-use-less-bold               nil
              solarized-use-more-italic             nil
              x-underline-at-descent-line           t)

  :config (progn
            (load-theme 'solarized-light t)
            ;; (set-face-attribute 'mode-line nil :height 180)
            ))

;; Should go into `core/boot'?
(use-package f
  :defer 2
  :load-path "core/f")

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
                   ("<f7>" . ace-window)))))

(use-package osx
  :if (eq system-type 'darwin)
  :init (progn
            (menu-bar-mode 1)

            (setq browse-url-browser-function 'browse-url-default-macosx-browser
                  delete-by-moving-to-trash    t
                  mac-option-key-is-meta       nil
                  mac-command-key-is-meta      t
                  mac-command-modifier        'meta
                  mac-option-modifier          nil)

            (4lex1v/configure-font "Monaco" 14)))

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
         ("M-x"     . helm-M-x))
  
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

            (use-package helm-descbinds
              :load-path "core/helm/helm-descbinds"
              :commands helm-descbinds
              :init (fset 'describe-bindings 'helm-descbinds))

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
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind ("M-1" . helm-projectile)

  :init (setq projectile-enable-caching       t
              projectile-require-project-root t
              projectile-use-git-grep         t)

  :config (progn
            (projectile-global-mode)

            (use-package helm-projectile
              :demand t
              

              :config (progn
                        (setq projectile-completion-system 'helm)
                        (helm-projectile-on)))
            
            (setq projectile-mode-line '(:eval (format " {%s}" (projectile-project-name))))))

(use-package magit
  :load-path "core/magit/lisp"
  :commands (magit-status magit-dispatch-popup magit-show-refs-popup)

  :bind (("C-c m s" . magit-status)
         ("C-c m b" . magit-blame)
         ("C-c m r" . magit-show-refs-popup)
         ("C-c m m" . magit-dispatch-popup))

  :init (progn
          (unbind-key "C-c m")
          (setq magit-last-seen-setup-instructions "2.3.2")))

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
                                  'cider-repl-mode-hook)))

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

  :config (yas-reload-all))

(use-package hideshowvis
  :diminish hs-minor-mode
  :commands hideshowvis-enable

  :bind (("M-[" . hs-hide-block)
         ("M-]" . hs-show-block))

  :init (progn
          (push '(scala-mode "\\({\\|(\\)" "\\(}\\|)\\)" "/[*/]" nil nil) hs-special-modes-alist)
          (let ((modes '(emacs-lisp-mode-hook scala-mode-hook)))
            (apply #'4lex1v/hook-into-modes #'hs-minor-mode modes)
            (apply #'4lex1v/hook-into-modes #'hideshowvis-enable modes)))
  
  :config (progn
            (hideshowvis-symbols)
            (hideshowvis-enable)))

(use-package scala
  :load-path "packages/scala"
  :init (progn
          (use-package scala-mode2
            :commands scala-mode
            :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode)

            :init (progn
                    (setq scala-indent:use-javadoc-style t
                          popup-complete-enabled-modes '(scala-mode))

                    (4lex1v/hook-into-modes #'4lex1v/connect-running-ensime 'scala-mode-hook)

                    (use-package sbt-mode :commands (sbt-start sbt-command)))

            :config (progn
                      ;; This should be fixed in future versions of use-package
                      ;; Ref - https://github.com/jwiegley/use-package/issues/269
                      (bind-key "C-c b"      'sbt-ext:open-build-file scala-mode-map)
                      (bind-key "<C-return>" 'newline-or-comment scala-mode-map)
                      (bind-key "M-j"        'scala-indent:join-line   scala-mode-map)

                      (sp-local-pair 'scala-mode "{" nil
                                     :post-handlers '((4lex1v/indent-in-braces "RET")))

                      (use-package ensime
                        :commands ensime

                        :bind (:map scala-mode-map
                                    ("C-c e" . ensime-print-errors-at-point)
                                    ("C-c t" . ensime-print-type-at-point)
                                    ("C-c i" . ensime-import-type-at-point)
                                    ("C-M-." . ensime-edit-definition-other-window))

                        :init (progn
                                (use-package popup :load-path "core/popup")
                                (setq ensime-default-buffer-prefix "ENSIME-"))

                        :config (unbind-key "M-p" ensime-mode-map))))))

(use-package elisp
  :load-path "packages/elisp"
  :bind (("C-c w n" . wrap-with-parens))
  :init (progn
          (use-package emacs-lisp-mode
               :bind (("M-." . find-function-at-point)
                      ("M-," . find-variable-at-point))

               :init (progn
                       (use-package eldoc
                         :diminish eldoc-mode
                         :init (progn
                                 (setq eldoc-idle-delay 0)
                                 (4lex1v/hook-into-modes #'eldoc-mode 'emacs-lisp-mode-hook)))

                       (use-package macrostep
                         :load-path "packages/elisp/macrostep"
                         :commands macrostep-expand
                         :bind (("C-c e" . macrostep-expand)))))))

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

(use-package guide-key
  :load-path "core/guide-key"
  :diminish guide-key-mode
  :init (setq guide-key/idle-delay 0.3
              guide-key/guide-key-sequence t
              guide-key/recursive-key-sequence-flag t)
  :config (guide-key-mode))

(setq gc-cons-threshold 1000000)
