(setq gc-cons-threshold 10000000)

(eval-and-compile
  (load (expand-file-name "foundation/foundation" user-emacs-directory)))

;;
;;; Platform Configuration
;; 

(fnd:module essentials     "The basis for my `.emacs.d' configuration. Configures package-management system and `use-package', along with others less important settings.")

(fnd:module appearance     "Themes, fonts, mode-line and other fancy things for a better look")
(fnd:module vim            "Platform configuration for EVIL layer")
(fnd:module behaviour      "All-things-navigation & movement")
(fnd:module editor         "A lot of things to improve editing experience")
(fnd:module system         "Low-level configuration for a better OS <=> Emacs integration")

;;
;;; Modules: Languages
;;

(fnd:module elisp          "Configuration for `emacs-lisp-mode' and other helpfull stuff to improve the experience with `ELisp'")
(fnd:module scala          "Scala language support + Ensime language server")
(fnd:module haskell        "Haskell programming language support")
(fnd:module idris          "Idris programming language support")
(fnd:module purescript     "PureScript programming language support")
(fnd:module native         "C/C++ support")
(fnd:module clojure        "Clojure programming language support")
(fnd:module groovy         "Groovy programming language support. Require for various DevOps related stuff, e.g Jenkinsfiles")
(fnd:module python         "Extensive support for Python ecosystem")

;;
;;; Modules: Others
;;

(fnd:module org            "Org-mode configuration package")
(fnd:module web            "Web & Frontend engineering stuff")
(fnd:module vcs            "All-things-Magit")
(fnd:module infra          "DevOps related stuff, e.g Docker")
(fnd:module formats        "Minor formats like Markdown & YAML support")
(fnd:module statistics     "Configuration package for math & statistics related tasks")
(fnd:module communications "Slack, Jabber, etc...")

(setq gc-cons-threshold 100000)
