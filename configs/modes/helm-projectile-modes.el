(setq helm-quick-update                      t ; do not display invisible candidates
      helm-split-window-in-side-p            t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching            t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source      t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp         t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                     8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf  t
      projectile-enable-caching              t
      projectile-completion-system          'helm)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(helm-mode 1)
(helm-projectile-on)
