(setq gc-cons-threshold 10000000)

;; [TODOs]
;; Foundation :: Log warning if the module doesn't exist

;; Appearance :: Add line / column number 
;; Appearance :: Change cursor's color, fix mode-line colors
;; Appearance :: Rework modeline style (inspired by https://github.com/hlissner/.emacs.d/blob/master/modules/ui/doom-modeline/config.el)

;; Sirthias   :: Improve `org-verbatim' face
;; Sirthias   :: Improve `comint-highlight-prompt' face

(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name "foundation" user-emacs-directory))
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

;(fnd:module extensions     "A set of custom configurations and definitions to be created after Emacs has been configured, but before other modules are loaded")

;;
;;; Modules: Languages
;;

(fnd:module elisp          "Configuration for `emacs-lisp-mode' and other helpfull stuff to improve the experience with `ELisp'")
(fnd:module scala          "Scala language support + Ensime language server")
(fnd:module haskell        "Haskell programming language support")
(fnd:module native         "C/C++ support")
(fnd:module csharp         "C# support, mostly to work with Unity3D engine on Mac... or Windows")
(fnd:module clojure        "Clojure programming language support")
(fnd:module racket         "Support for the Racket programming language")
(fnd:module python         "Emacs support for Python ecosystem")
(fnd:module lua            "Emacs module with support for Lua programming language")
(fnd:module groovy         "Support for Groovy programming language")
(fnd:module idris          "Idris programming language support")
(fnd:module purescript     "PureScript programming language support")

;;
;;; Modules: Others
;;

(fnd:module vcs            "All-things-Magit")
(fnd:module org            "Org-mode configuration package")
(fnd:module infra          "DevOps related stuff, e.g Docker")
(fnd:module formats        "Minor formats like Markdown & YAML support")
(fnd:module web            "Web & Frontend engineering stuff")
(fnd:module communications "Slack, Jabber, etc...")
(fnd:module reader         "PDF reading tools")
(fnd:module statistics     "Configuration package for math & statistics related tasks")

;;
;;; Modules: Projects & Work
;;

;;(fnd:module work           "Additional configuration to wire Emacs for work related projects")

(setq gc-cons-threshold 100000)
