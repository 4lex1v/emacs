(setq gc-cons-threshold 10000000)

(eval-and-compile
  (load (expand-file-name "foundation/foundation" user-emacs-directory)))

(setq default-directory "~/Sandbox/")

;;
;;; Platform Configuration
;; Unlike other modules, these should be loaded sequentially in a given order
;; 

(fnd:module essentials     "The basis for my `.emacs.d' configuration. Configures package-management system and `use-package', along with others less important settings.")
(fnd:module appearance     "Themes, fonts, mode-line and other fancy things for a better look")
(fnd:module behaviour      "All-things-navigation & movement")
(fnd:module editor         "A lot of things to improve editing experience")
(fnd:module system         "Low-level configuration for a better OS <=> Emacs integration")

;;
;;; Modules: Languages
;;

;; Lisps
(fnd:module clojure        "Clojure programming language support")
(fnd:module elisp          "Configuration for `emacs-lisp-mode' and other helpfull stuff to improve the experience with `ELisp'")
(fnd:module racket         "Support for the Racket programming language")
(fnd:module python         "Emacs support for Python ecosystem")
(fnd:module lua            "Emacs module with support for Lua programming language")
(fnd:module groovy         "Support for Groovy programming language")
(fnd:module haskell        "Haskell programming language support")
(fnd:module idris          "Idris programming language support")
(fnd:module purescript     "PureScript programming language support")
(fnd:module native         "C/C++ support")
(fnd:module scala          "Scala language support + Ensime language server")

;;
;;; Modules: Others
;;

(fnd:module communications "Slack, Jabber, etc...")
(fnd:module formats        "Minor formats like Markdown & YAML support")
(fnd:module infra          "DevOps related stuff, e.g Docker")
(fnd:module org            "Org-mode configuration package")
(fnd:module reader         "PDF reading tools")
(fnd:module statistics     "Configuration package for math & statistics related tasks")
(fnd:module vcs            "All-things-Magit")
(fnd:module web            "Web & Frontend engineering stuff")

(setq gc-cons-threshold 100000)
