(setq gc-cons-threshold 10000000
      package--init-file-ensured t
      debug-on-error t)

;; At this point i'm not sure what's the better way to solve this chiken-and-egg issue
;; There's a corresponding comment in org module.org config file
(add-to-list 'load-path (expand-file-name "modules/org/org-mode/lisp" user-emacs-directory))
(load (expand-file-name "foundation/foundation" user-emacs-directory))

(fnd:platform-module essentials)

;; So `vim' module goes before others to give access to `evil-leader' configuration
;; Conceptually, it's something i'd move to `essentials' as a config (e.g `:vim t')
;; Leaving it this way for now
(fnd:platform-module vim)

(fnd:platform-module appearance)
(fnd:platform-module behaviour)
(fnd:platform-module editor)
(fnd:platform-module system)

;; Modules: Languages
(fnd:install-module elisp)
(fnd:install-module scala)
(fnd:install-module haskell)
(fnd:install-module idris)
(fnd:install-module purescript)
(fnd:install-module native)
(fnd:install-module clojure)
(fnd:install-module groovy)
(fnd:install-module python)
(fnd:install-module web)

;; Modules: Others
(fnd:install-module org)
(fnd:install-module vcs)
(fnd:install-module infra)
(fnd:install-module formats)
(fnd:install-module statistics)
(fnd:install-module communications)

(setq gc-cons-threshold 100000)
