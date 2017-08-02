;; Goes before others to correctly load which-key-declare-prefixes
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.2
        which-key-sort-order 'which-key-prefix-then-key-order-reverse
        which-key-show-operator-state-maps t ;; Hack to make this work with Evil
        which-key-prefix-prefix ""
        which-key-side-window-max-width 0.5

        which-key-popup-type           'side-window 
        which-key-side-window-location 'bottom) 
  
  :config
  (which-key-mode))

(use-package imenu)

(use-package general
  :init
  (setq general-default-states  'normal
        general-default-prefix  "<SPC>"))

(use-package evil
  :after general ;; To enable evil-leader in initial buffers
  :init
  (setq evil-default-cursor             t
        evil-ex-substitute-global       t
        evil-ex-search-vim-style-regexp t
        evil-want-C-u-scroll            t
        evil-ex-interactive-search-highlight 'selected-window)
  
  :general
  (:prefix ""
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "$" 'evil-end-of-visual-line
   "ga" 'helm-apropos
   "C-j" 'evil-forward-paragraph
   "C-k" 'evil-backward-paragraph
   "g," 'evil-jump-backward)

  (:states '(normal)
   "wo"  'other-window
   "wsb" 'split-window-below
   "wsh" 'split-window-horizontally
   "wdd" 'delete-window
   "wdo" 'delete-other-windows
   "fi"  'init.el
   "fe"  'eshell)
  
  :config
  (general-evil-setup t)

  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'comint-mode 'normal)

  ;; Use `§' key to switch between emacs and normal state
  (evil-global-set-key 'normal "§" #'evil-emacs-state)
  (evil-global-set-key 'emacs  "§" #'evil-exit-emacs-state)

  (evil-ex-define-cmd "e[val]" #'eval-buffer)
  
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode))

(use-package helm-config)
(use-package helm-mode)

(use-package helm
  :diminish helm-mode
  :commands helm-mode
  
  :bind* ("C-c h o" . helm-occur)
  
  :bind
  (("C-c h"   . helm-command-prefix)
   ("M-y"     . helm-show-kill-ring)
   ("C-x b"   . helm-mini)
   ("C-x C-f" . helm-find-files)         
   ("M-x"     . helm-M-x)
   ("M-:"     . helm-eval-expression-with-eldoc)

   ;; Number keys
   ("M-3"     . helm-mini)
   ("M-6"     . helm-bookmarks)

   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i"   . helm-execute-persistent-action)
   ("C-z"   . helm-select-action)
   ("C-j"   . helm-next-line)
   ("C-k"   . helm-previous-line)
   ("M-j"   . helm-next-source)
   ("M-k"   . helm-previous-source)

   :map helm-find-files-map
   ("C-h"   . helm-find-files-up-one-level))
  
  :init
  (setq helm-idle-delay                        0.0
        helm-input-idle-delay                  0.01
        helm-quick-update                      t
        helm-split-window-in-side-p            t
        helm-buffers-fuzzy-matching            t
        helm-move-to-line-cycle-in-source      t
        helm-scroll-amount                     8
        helm-ff-search-library-in-sexp         t
        helm-ff-file-name-history-use-recentf  t
        helm-follow-mode-persistent            t)
  
  :config 
  (helm-autoresize-mode)
  ;(spaceline-helm-mode)
  (which-key-declare-prefixes "<SPC> f" "Files")

  (func init.el (find-file (concat user-emacs-directory "/" "init.el")))
  (general-define-key "ff" 'helm-find-files)
  
  (substitute-key-definition 'find-tag 'helm-etags-select global-map))

(use-package helm-swoop
  :commands helm-swoop

  :bind
  (("M-i"     . helm-swoop)
   ("M-I"     . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   :map isearch-mode-map
   ("M-i" . helm-swoop-from-isearch)
   ("M-I" . helm-multi-swoop-all-from-isearch)
   :map helm-swoop-map
   ("M-i" . helm-multi-swoop-all-from-helm-swoop)
   ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop))

  :init
  (setq helm-multi-swoop-edit-save t
        helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-move-to-line-cycle t
        helm-swoop-use-line-number-face t))

(use-package helm-descbinds
  :commands helm-descbinds
  :bind (:map helm-command-map
         ("b" . helm-descbinds))
  :init
  (fset 'describe-bindings 'helm-descbinds)
  (setq helm-descbinds-window-style 'same-window))

(use-package helm-ag
  :commands helm-projectile-ag
  :init
  (setq helm-ag-insert-at-point 'symbol
        helm-ag-fuzzy-match     t))

(use-package foundation-helm
  :general
  ("el" '(fnd:helm-list-modules :which-key "Modules")))

(use-package projectile
  :commands projectile-project-root
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind
  (("M-1" . helm-projectile)
   ("M-4" . projectile-switch-project))
  
  :general
  ("p" '(:ignore t :which-key "Projectile")
   "pp" 'helm-projectile-switch-project
   
   ;; Search
   "ps" '(:ignore t :which-key "Search [Ag]")
   "pss" 'helm-projectile-ag
   "psr" 'helm-ag-project-root
   "psa" 'helm-do-ag

   ;; Refactoring
   "pr" 'projectile-replace
   
   "pi" 'projectile-invalidate-cache)

  :init
  (setq projectile-enable-caching       t
        projectile-require-project-root t
        projectile-use-git-grep         t
        projectile-mode-line            '(:eval (format " {%s}" (projectile-project-name))))

  :config
  (projectile-global-mode))

(use-package ibuffer-projectile
  :after projectile
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package helm-projectile
  :after (helm projectile)
  :demand t
  :init (setq projectile-completion-system 'helm)
  :config (helm-projectile-on))

(use-package ranger
  :commands ranger
  :bind
  (("M-5"     . helm-ranger-bookmarks)
   ("C-c C-l" . org-store-link))
  :init
  (setq ranger-override-dired 'ranger
        ranger-show-literal    nil ;; Turn on highlighting in ranger mode
        ranger-cleanup-eagerly t
        ranger-show-dotfiles   t)

  (bind-key "M-2" #'(lambda (&optional arg)
                      (interactive "P")
                      (if (not arg)
                          (ranger arg)
                        (ranger
                         (or (projectile-project-root)
                             default-directory)))))

  (cl-defun drill-folder-down (&optional (entry-point (dired-get-filename nil t)))
    (interactive)
    (if (file-directory-p entry-point)
        (let* ((subentries (f-entries entry-point)))
          ;; If `entry-point' contains a single folder navigate into it
          (if (and (eq (length subentries) 1)
                   (file-directory-p (car subentries)))
              (drill-folder-down (car subentries))
            (ranger-find-file entry-point)))))

  (defun drill-folder-up (&optional initial)
    (interactive)
    (let* ((entry  (or initial default-directory))
           (parent-folder (f-parent entry))
           (parent-content (f-entries parent-folder))
           (nr-of-entryies (length parent-content)))
      (if (eq nr-of-entryies 1)
          (drill-folder-up parent-folder)
        (ranger-find-file parent-folder))))           
  
  :general
  ("fr" 'ranger)
  
  :config
  (ranger-override-dired-mode t)

  (bind-key "l" #'drill-folder-down ranger-mode-map)
  (bind-key "h" #'drill-folder-up   ranger-mode-map)

  (with-package helm
    (defun helm-ranger-bookmarks ()
      (interactive)
      (helm :sources (helm-build-in-buffer-source "Ranger Bookmarks"
                       :data (lambda ()
                               (bookmark-maybe-load-default-file)
                               (ranger--directory-bookmarks))
                       :fuzzy-match t
                       :action 'ranger)
            :buffer "*helm ranger bookmarks*"))))

(use-package helm-dash)

(general-nmap 
  "eq" #'save-buffers-kill-emacs
  "er" #'revert-buffer
  "q"  #'4lex1v/close-buffer)

(define-key evil-normal-state-map "g." #'find-function-at-point)

;; Need to organize this to avoid disambiguity and not to forget
(delete-selection-mode t)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(which-key-declare-prefixes "<SPC> w" "Windows")
(which-key-declare-prefixes "<SPC> w s" "Spliting")
(which-key-declare-prefixes "<SPC> w d" "Deliting")

(fset 'yes-or-no-p   'y-or-n-p)

(use-package avy
  :bind
  (("C-c SPC" . avy-goto-char)
   ("C-c j c" . avy-goto-char)
   ("C-c j w" . avy-goto-word-1)
   ("C-c j l" . avy-goto-line))
  :init
  (general-define-key "j" #'avy-goto-char))

(use-package ace-window
  :bind
  (("C-'"  . ace-window))
  :general
  ("wj" 'ace-window))

(use-package ggtags
  :general
  ("g" '(:ignore t :which-key "GTags")
   "gs" 'ggtags-find-other-symbol
   "gh" 'ggtags-view-tag-history
   "gr" 'ggtags-find-reference
   "gf" 'ggtags-find-file
   "gc" 'ggtags-create-tags
   "gu" 'ggtags-update-tags)
  
  :config
  (ggtags-mode 1))
