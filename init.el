(setq gc-cons-threshold 10000000
      package--init-file-ensured t
      debug-on-error t)

(eval-and-compile
  (load (expand-file-name "foundation/foundation" user-emacs-directory)))

;; Emacs experience configs 
(fnd:platform-module essentials)

;; So `vim' module goes before others to give access to `evil-leader' configuration
;; Conceptually, it's something i'd move to `essentials' as a config (e.g `:vim t')
;; Leaving it this way for now
(fnd:platform-module vim) ;; Enabled initially, turn-off manually if needed

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
(fnd:install-module python :ignore t)
(fnd:install-module web)

;; Modules: Others
(fnd:install-module org)
(fnd:install-module vcs)
(fnd:install-module infra)
(fnd:install-module formats)
(fnd:install-module statistics)
(fnd:install-module communications)
;(fnd:install-module reader)

(setq gc-cons-threshold 100000)
