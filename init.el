;; Preloaded libraries
(message "Loading emacs...")

;; Change back after config has been loaded
(setq gc-cons-threshold 10000000)

;; Add bootstrap libs to the load path 
(add-to-list 'load-path (concat user-emacs-directory "core/boot"))
(add-to-list 'load-path (concat user-emacs-directory "core/use-package"))

(require 'use-package)
(setq use-package-verbose t
      use-package-minimum-reported-time 0.01)

;; UI config goes first, if any subsequent config fails
;; at least we can have a pretty UI to work with emacs...
(use-package bootstrap
  :demand t
  :bind (("RET"         . newline-and-indent)
         ("M-j"         . join-line)
         ("C-c m"       . execute-extended-command)
         ("C-x C-b"     . ibuffer)
         ("C-c l"       . view-mode)
         ("C-a"         . back-to-indentation)
         ("M-m"         . beginning-of-line)
         ("S-C-<left>"  . shrink-window-horizontally)
         ("S-C-<right>" . enlarge-window-horizontally)
         ("S-C-<down>"  . shrink-window)
         ("S-C-<up>"    . enlarge-window)
         ("C-S-d"       . duplicate-line)
         ("M-`"         . other-frame)
         ("C-c r"       . revert-buffer)
         ("C-c C-d"     . 4lex1v/delete-current-file)
         ("C-x \\"      . align-regexp)

         ("<f9>"        . list-packages)
         ("<f10>"       . customize-themes)
         
         ("C-+"         . text-scale-increase)
         ("C--"         . text-scale-decrease)
         
         ("C-q"         . 4lex1v/closeBuffer)
         ("M-q"         . 4lex1v/closeOtherBuffer))

  :init (progn 
          (tool-bar-mode   -1)
          (scroll-bar-mode -1)
          (menu-bar-mode   -1)

          (setq-default tab-width 2
                        cursor-type 'bar
                        frame-title-format " %@%b% -"
                        linum-format "%3d "
                        indent-tabs-mode  nil) ;; Try dynamic?

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
                next-line-add-newlines      t)

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
            (4lex1v/configure-font "Monaco for Powerline" 20)
            (4lex1v/configure-frame-size 'maximized)
            (set-face-attribute 'mode-line nil  :height 180)))

(use-package solarized
  :demand t
  :load-path "themes/solarized-emacs"
  
  :init (setq solarized-contrast                   'high
              solarized-visibility                 'high
              solarized-termcolors                  256
              solarized-distinct-fringe-background  nil
              solarized-use-variable-pitch          nil
              solarized-use-less-bold               nil
              solarized-use-more-italic             nil
              x-underline-at-descent-line           t)

  :config (load-theme 'solarized-light t))

(use-package projectile
  :load-path "core/projectile"
  :bind-keymap ("C-c p" . projectile-command-map)

  :init (setq projectile-enable-caching       t
              projectile-require-project-root t
              projectile-use-git-grep         t)

  :config (progn
            (projectile-global-mode)
            (setq projectile-mode-line '(:eval (format " {%s}" (projectile-project-name))))))

(use-package helm
  :diminish helm-mode
  :load-path "core/helm"

  :bind* ("C-c h o" . helm-occur)
  :bind (("C-c h"   . helm-command-prefix)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         ("<f7>"    . helm-mini)
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
          (setq helm-quick-update                      t
                helm-split-window-in-side-p            t
                helm-buffers-fuzzy-matching            t
                helm-move-to-line-cycle-in-source      t
                helm-ff-search-library-in-sexp         t
                helm-scroll-amount                     8
                helm-ff-file-name-history-use-recentf  t
                helm-ag-insert-at-point                'symbol)

          (use-package helm-config)
          (use-package helm-mode :config (helm-mode 1)))
  
  :config (progn
            (helm-autoresize-mode)

            (use-package helm-projectile
              :demand t
              :load-path "packages/projectile"
              :bind ("M-1" . helm-projectile)

              :config (progn
                        (setq projectile-completion-system 'helm)
                        (helm-projectile-on)))

            (use-package helm-descbinds
              :commands helm-descbinds
              :bind ("C-c h d" . helm-descbinds))

            (use-package helm-ag
              :commands helm-projectile-ag)))

(use-package magit
  :load-path "core/magit/lisp"

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

  :bind (("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp))

  :init (progn
          (use-package smartparens-config
            :init (setq sp-autoinsert-if-followed-by-word t
                        sp-autoskip-closing-pair 'always-end
                        sp-hybrid-kill-entire-symbol nil))

          (4lex1v/hook-into-modes #'smartparens-mode
                                  'scala-mode-hook
                                  'emacs-lisp-mode-hook)))

(use-package company
  :diminish company-mode
  :load-path "core/company"
  :commands global-company-mode

  :init (progn
          (setq company-dabbrev-ignore-case nil
              company-dabbrev-code-ignore-case nil
              company-dabbrev-downcase nil
              company-idle-delay 0
              company-minimum-prefix-length 4)

          (4lex1v/hook-into-modes #'global-company-mode 'scala-mode-hook)))

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
          (let ((modes '(emacs-lisp-mode-hook scala-mode-hook)))
            (apply #'4lex1v/hook-into-modes 'hs-minor-mode modes)
            (apply #'4lex1v/hook-into-modes #'hideshowvis-enable modes)))
  
  :config (progn
            (hideshowvis-symbols)
            (hideshowvis-enable)))


(use-package scala
  :load-path "packages/scala"
  :config (use-package scala-mode2
            :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode)
            :bind (:map scala-mode-map
                        ("C-c b"      . sbt-ext:open-build-file)
                        ("<C-return>" . newline-or-comment)
                        ("M-j"        . scala-indent:join-line))

            :init (progn
                    (setq scala-indent:use-javadoc-style t
                          popup-complete-enabled-modes '(scala-mode))

                    ;; FIXME :: for some reason if i omit this requirement,
                    ;;          key bindings throw an error that map not found
                    (use-package scala-mode2-map)

                    (4lex1v/hook-into-modes
                     #'(lambda () (if (4lex1v/ensime-project-p) (ensime-mode 1)))
                     'scala-mode-hook))

            :config (progn
                      (sp-local-pair 'scala-mode "{" nil :post-handlers '((4lex1v/indent-in-braces "RET")))

                      (use-package sbt-mode
                        :commands sbt-start)

                      ;; Required by ENSIME
                      (use-package popup :load-path "core/popup")

                      (use-package ensime
                        :commands ensime-mode
                        :bind (:map scala-mode-map
                                    ("C-c e" . ensime-print-errors-at-point)
                                    ("C-c t" . ensime-print-type-at-point)
                                    ("C-c i" . ensime-import-type-at-point)
                                    ("C-M-." . ensime-edit-definition-other-window))

                        :init (progn
                                (setq ensime-default-buffer-prefix "ENSIME-")
                                (4lex1v/hook-into-modes #'(lambda ()
                                                            (set (make-local-variable 'company-backends)
                                                                 '(ensime-company
                                                                   (company-semantic
                                                                    company-keywords
                                                                    company-dabbrev-code
                                                                    company-yasnippet))))
                                                        'ensime-mode-hook))
                        :config (unbind-key "M-p" ensime-mode-map)))))

;; ;; (use-package ace-jump-mode
;; ;;   :bind (("C-c SPC" . ace-jump-char-mode)
;; ;;          ("C-c j c" . ace-jump-char-mode)
;; ;;          ("C-c j w" . ace-jump-word-mode)
;; ;;          ("C-c j l" . ace-jump-line-mode)))

;; ;; (use-package multiple-cursors)

;; ;; (use-package hlinum
;; ;;   :defer t)

;; ;; ;; OCaml language configuration
;; ;; (use-package opam
;; ;;   :init (opam-init))

;; ;; (use-package tuareg
;; ;;   :defer t
;; ;;   :config

;; ;;   (use-package utop
;; ;;     :defer t
;; ;;     :init
;; ;;     (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
;; ;;     (add-hook 'tuareg-mode-hook 'utop-minor-mode))

;; ;;   (use-package merlin
;; ;;     :defer t
;; ;;     :init
;; ;;     (add-hook 'tuareg-mode-hook 'merlin-mode)
;; ;;     (use-package company)
;; ;;     (add-to-list 'company-backends 'merlin-company-backend)

;; ;;     :config
;; ;;     (setq merlin-use-auto-complete-mode 'easy
;; ;;           merlin-command 'opam))

;; ;;   (use-package ocp-indent
;; ;;     :init
;; ;;     (setq tuareg-use-smie nil)))

;; (use-package shell-mode
;;   :defer t

;;   :init
;;   (defun shell-clear ()
;;     (interactive)
;;     (let ((comint-buffer-maximum-size 0))
;;       (comint-truncate-buffer)))

;;   :config (bind-key "C-c k" 'shell-clear shell-mode-map))

;; ;; (use-package er/expand-region :bind ("C-=" . er/expand-region))

;; (use-package guide-key
;;   :diminish guide-key-mode

;;   :init (setq guide-key/idle-delay 0.3
;;               guide-key/guide-key-sequence t
;;               guide-key/recursive-key-sequence-flag t)

;;   :config (guide-key-mode))

(setq gc-cons-threshold 1000000)
