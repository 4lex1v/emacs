(use-package elisp-mode
  :diminish    (emacs-lisp-mode . "ELisp")
  :after       (yasnippet company which-key)
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode        (("\\.el$" . emacs-lisp-mode)
                ("Cask"   . emacs-lisp-mode))

  :bind
  (("M-." . find-function-at-point)
   ("M-," . find-variable-at-point)
   ("C-c e r" . eval-region))

  :init
  (which-key-declare-prefixes-for-mode 'emacs-lisp-mode "C-c e" "elisp"))
  ;(with-mode which-key))

(use-package macrostep
  :load-path "modules/elisp/macrostep"
  :after elisp-mode
  :commands macrostep-expand
  :bind
  (:map emacs-lisp-mode-map
   ("C-c e m" . macrostep-expand)))

(use-package deferred
  :load-path "modules/elisp/deferred"
  :after elisp-mode)

(use-package request
  :load-path "modules/elisp/request"
  :after (elisp-mode deferred)
  :init
  (use-package request-deferred :after deferred)
  (setq request-log-level 'debug
        request-message-level 'warn))
