(use-package clojure-mode :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.boot\\'" . clojure-mode)
         ("\\.cljs.*\\'" . clojure-mode)
         ("lein-env" . ruby-mode))

  :bind (("C-c C-v" . cider-start-http-server)
         ("C-M-r"   . cider-refresh)
         ("C-c u"   . cider-user-ns))

  :init
  (setq inferior-lisp-program "lein repl")
  
  (eval-after-load 'org-mode
    (lambda ()
      (add-to-list 'org-babel-load-languages '(clojure . t)))) 

  :config (progn
            (use-package clojure-mode-extra-font-locking
              :init (font-lock-add-keywords
                     nil
                     '(("(\\(facts?\\)"
                        (1 font-lock-keyword-face))
                       ("(\\(background?\\)"
                        (1 font-lock-keyword-face)))))
            
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

(use-package cider
  :ensure t
  :load-path "modules/clojure/cider"
  :after clojure-mode
  :commands cider-mode
  :bind (("C-c u" . cider-user-ns))

  :init (progn
          (4lex1v/hook-into-modes #'cider-mode 'clojure-mode-hook)
          (4lex1v/hook-into-modes #'eldoc-mode 'cider-mode-hook)

          (setq cider-repl-pop-to-buffer-on-connect t ;; go right to the REPL buffer when it's finished connecting
                cider-show-error-buffer t ;; When there's a cider error, show its buffer and switch to it
                cider-auto-select-error-buffer t
                cider-repl-history-file "~/.emacs.d/cider-history"
                cider-repl-wrap-history t)))
