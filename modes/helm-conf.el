(use-package helm-config
  :demand t
  
  :diminish helm-mode

  :init
  (helm-mode 1)
  (helm-autoresize-mode)

  :bind (("C-c h"  . helm-command-prefix)
         ("C-c O"  . helm-occur)
         ("M-y"    . helm-how-kill-ring)
         ("C-x b"  . helm-mini)
         ("C-x C-" . helm-find-files)
         ("M-1"    . helm-rojectile)
         ("M-x"    . helm-M-x))

  :config
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
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map) ; rebihnd tab to do persistent action
  (bind-key "C-i"   'helm-execute-persistent-action helm-map) ; make TAB works in terminal
  (bind-key "C-z"   'helm-select-action             helm-map) ; list actions using C-z
  (bind-key "C-o"   'helm-next-source               helm-map)
  (bind-key "M-o"   'helm-previous-source           helm-map)

  (use-package helm-descbinds))
