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
(fnd:extension elisp)
(fnd:extension scala)
(fnd:extension haskell)
(fnd:extension idris)
(fnd:extension purescript)
(fnd:extension native)
(fnd:extension clojure)
(fnd:extension groovy)
(fnd:extension python)
(fnd:extension web)

;; Modules: Others
(fnd:extension org)
(fnd:extension vcs)
(fnd:extension infra)
(fnd:extension formats)
(fnd:extension statistics)
(fnd:extension communications)

(setq gc-cons-threshold 100000)
