(setq gc-cons-threshold 10000000
      package--init-file-ensured t)

(eval-and-compile
  (load (expand-file-name "foundation/foundation" user-emacs-directory)))

;; Emacs experience configs 
(fnd:platform-module essentials)
(fnd:platform-module appearance)
(fnd:platform-module behaviour)
(fnd:platform-module editor)

;; Modules: Languages
(fnd:install-module elisp)
(fnd:install-module scala)
(fnd:install-module haskell)
(fnd:install-module idris)
(fnd:install-module purescript)
(fnd:install-module native)
(fnd:install-module clojure)
(fnd:install-module python)
(fnd:install-module web)

;; Modules: Others
(fnd:install-module org)
(fnd:install-module vcs)
(fnd:install-module infra)
(fnd:install-module formats)

(fnd:install-module communications :ignore t)

(setq gc-cons-threshold 100000)
