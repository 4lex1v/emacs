(setq gc-cons-threshold 10000000)
(eval-and-compile
  (setq user-init-file (or load-file-name (buffer-file-name)))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (add-to-list 'load-path (expand-file-name "foundation" user-emacs-directory))
  (require 'foundation))

;;
;;; Platform Configuration
;; Unlike other modules, these should be loaded sequentially in a given order
;; 

(fnd:module essentials     "The basis for my `.emacs.d' configuration. Configures package-management system and `use-package', along with others less important settings.")
(fnd:module system         "Low-level configuration for a better OS <=> Emacs integration")
(fnd:module appearance     "Themes, fonts, mode-line and other fancy things for a better look")
(fnd:module behaviour      "All-things-navigation & movement")
(fnd:module editor         "A lot of things to improve editing experience")

;;
;;; Modules: Programming
;;
(fnd:module elisp          "Configuration for `emacs-lisp-mode' and other helpfull stuff to improve the experience with `ELisp'")
(fnd:module scala          "Scala language support + Ensime language server")
(fnd:module debbug         "Some extra stuff to improve Emacs' debbugging capabilities." :ignore t)
(fnd:module native         "C/C++ support.")
(fnd:module rust           "Rust-lang support.")
(fnd:module csharp         "C# support, mostly to work with Unity3D engine on Mac... or Windows")
(fnd:module haskell        "Haskell programming language support" :ignore t)
(fnd:module clojure        "Clojure programming language support" :ignore t)
(fnd:module racket         "Support for the Racket programming language" :ignore t)
(fnd:module python         "Emacs support for Python ecosystem" :ignore t)
(fnd:module lua            "Emacs module with support for Lua programming language")
(fnd:module groovy         "Support for Groovy programming language") ;; #NOTE(4lex1v, 05/14/18) :: Used by the Jenkinsfile
(fnd:module idris          "Idris programming language support" :ignore t)
(fnd:module purescript     "PureScript programming language support" :ignore t)
(fnd:module web            "Web & Frontend engineering stuff")

;;
;;; Modules: Others
;;

(fnd:module vcs            "All-things-Magit")
(fnd:module reader         "Support for effective PDF reading")
(fnd:module communications "Email, Slack, Jabber and others" :ignore t)
(fnd:module org            "Org-mode configuration package")
(fnd:module formats        "Minor formats like Markdown & YAML support")
(fnd:module infra          "DevOps related stuff, e.g Docker")

;;
;;; Modules: Projects & Work
;;

(fnd:module workspace      "Custom configuration for all-things-projects, i.e detailed org-mode config")
(fnd:module work           "Additional configuration to wire Emacs for work related projects" :ignore t)

(setq gc-cons-threshold 1000000)


