(use-package elisp-mode
  :after       (yasnippet company which-key)
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode        (("\\.el$" . emacs-lisp-mode)
                ("Cask"   . emacs-lisp-mode))

  :bind
  (("M-." . find-function-at-point)
   ("M-," . find-variable-at-point)
   ("C-c e r" . eval-region))

  :general
  (:keymaps 'emacs-lisp-mode-map
            "e"  '(:ignore t :which-key "Emacs")
            "ev" '(:ignore t :which-key "Describe Variable")
            "ed" '(:ignore t :which-key "Docs & Help")
            "eda" #'helm-apropos)
  
  :init
  (setq-mode-local emacs-lisp-mode comment-note-comment-prefix ";;")
  
  :config
  (load "elisp-defuns")
  
  (setq company-backends '((company-abbrev company-dabbrev)))
  
  (add-hook 'emacs-lisp-mode-hook
            (company-add-mode-backends '(company-elisp company-yasnippet company-capf company-files))
            t)
  
  (add-hook 'emacs-lisp-mode-hook #'4lex1v:fix-elisp-indentation)
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)
  (add-hook 'emacs-lisp-mode-hook #'company-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'hideshowvis-enable)
  (add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)

  (sp-with-modes 'emacs-lisp-mode
    (sp-local-pair "'" nil :actions nil))))

(use-package macrostep
  :after elisp-mode
  :commands macrostep-expand
  :mode ("\\*.el\\'" . emacs-lisp-mode)
  
  :general
  (:keymaps 'macrostep-keymap
    "q" #'macrostep-collapse-all)

  (general-define-key :keymaps 'emacs-lisp-mode-map
   "em" #'macrostep-expand))

(use-package deferred
  :after elisp-mode)

(use-package request
  :after (elisp-mode deferred)
  :init
  (use-package request-deferred :after deferred)
  (setq request-log-level 'debug
        request-message-level 'warn))
