(use-package helm
  :diminish helm-mode
  :load-path "platform/behaviour/helm-core"
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
   ("M-k"   . helm-previous-source))
  
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

  (use-package helm-config)
  (use-package helm-mode)
  
  :config 
  (helm-autoresize-mode)

  (substitute-key-definition 'find-tag 'helm-etags-select global-map)

  (with-mode which-key
    (which-key-declare-prefixes "C-c h" "helm")))

(use-package helm-swoop
  :load-path "platform/behaviour/helm-swoop"
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
  :load-path "platform/behaviour/helm-descbinds"
  :commands helm-descbinds
  :bind (:map helm-command-map
         ("b" . helm-descbinds))
  :init
  (fset 'describe-bindings 'helm-descbinds)
  (setq helm-descbinds-window-style 'same-window))

(use-package helm-ag
  :load-path "platform/behaviour/helm-ag"
  :commands helm-projectile-ag
  :init
  (setq helm-ag-insert-at-point 'symbol
        helm-ag-fuzzy-match     t))

(use-package projectile
  :load-path "platform/behaviour/projectile"
  :commands projectile-project-root
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind
  (("M-1" . helm-projectile)
   ("M-4" . projectile-switch-project))

  :init
  (setq projectile-enable-caching       t
        projectile-require-project-root t
        projectile-use-git-grep         t)

  :config
  (evil-leader/set-key "pp" 'helm-projectile-switch-project)
  (evil-leader/set-key "ps" 'helm-projectile-ag)
  (projectile-global-mode)

  (with-mode which-key
    (which-key-declare-prefixes "C-c p" "projectile"))
  
  ;; Stored in core/vendor
  (use-package ibuffer-projectile
    :ensure t
    :init
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic)))))
  
  (setq projectile-mode-line '(:eval (format " {%s}" (projectile-project-name)))))

(use-package helm-projectile
  :load-path "platform/behaviour/helm-projectile"
  :after (helm projectile)
  :demand t
  :init (setq projectile-completion-system 'helm)
  :config (helm-projectile-on))

(use-package ranger
  :load-path "platform/behaviour/ranger"
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

  (evil-leader/set-key "fr" 'ranger)
  :config
  (ranger-override-dired-mode t)

  (defun drill-folder-down (&optional initial)
    (interactive)
    (let ((folder (or initial (dired-get-filename nil t))))
      (when (and folder (file-directory-p folder))
        (let* ((subfolder (f-entries folder))
               (nr-of-subfolders (length subfolder)))
          (if (eq nr-of-subfolders 1)
              (drill-folder-down (car subfolder))
            (ranger-find-file initial))))))

  (defun drill-folder-up (&optional initial)
    (interactive)
    (let* ((entry  (or initial default-directory))
           (parent-folder (f-parent entry))
           (parent-content (f-entries parent-folder))
           (nr-of-entryies (length parent-content)))
      (if (eq nr-of-entryies 1)
          (drill-folder-up parent-folder)
        (ranger-find-file parent-folder))))           

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

(use-package helm-dash
  :load-path "platform/behaviour/helm-dash")

(evil-leader/set-key "eq" #'save-buffers-kill-emacs)

;; Need to organize this to avoid disambiguity and not to forget
(delete-selection-mode t)
