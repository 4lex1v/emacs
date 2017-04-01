(use-package smartparens-config
  :init
  (setq sp-autoinsert-if-followed-by-word t
        sp-autoskip-closing-pair 'always-end
        sp-hybrid-kill-entire-symbol nil))

(use-package smartparens
  :after smartparens-config
  :diminish smartparens-mode

  :bind
  (:map sp-keymap
   ("M-F"              . sp-forward-symbol)
   ("M-B"              . sp-backward-symbol)

   ("C-M-k"            . sp-kill-sexp)
   ("C-M-w"            . sp-copy-sexp)
   ("C-M-t"            . sp-transpose-sexp)
   
   ("M-<delete>"       . sp-unwrap-sexp)
   ("M-<backspace>"    . sp-backward-unwrap-sexp)

   ("M-<left>"         . sp-forward-slurp-sexp)
   ("C-M-<left>"       . sp-forward-barf-sexp)
   ("M-<right>"        . sp-backward-slurp-sexp)
   ("C-M-<right>"      . sp-backward-barf-sexp)

   ("M-D"              . sp-splice-sexp)
   ("C-M-<delete>"     . sp-splice-sexp-killing-forward)
   ("C-M-<backspace>"  . sp-splice-sexp-killing-backward)
   ("C-S-<backspace>"  . sp-splice-sexp-killing-around)

   ("C-M-["            . sp-select-previous-thing)
   ("C-M-]"            . sp-select-next-thing)

   ("C-c s u"          . sp-up-sexp)
   ("C-c s d"          . sp-down-sexp)
   ("C-c s t"          . sp-prefix-tag-object)
   ("C-c s p"          . sp-prefix-pair-object)
   ("C-c s c"          . sp-convolute-sexp)
   ("C-c s a"          . sp-absorb-sexp)
   ("C-c s e"          . sp-emit-sexp)
   ("C-c s p"          . sp-add-to-previous-sexp)
   ("C-c s n"          . sp-add-to-next-sexp)
   ("C-c s j"          . sp-join-sexp)
   ("C-c s s"          . sp-split-sexp))

  :config
  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "s-[")
  (sp-pair "{" "}" :wrap "C-{")
  
  (with-mode which-key
    (which-key-declare-prefixes
      "C-c s" "smartparens")))

(use-package company
  :diminish company-mode
  :commands global-company-mode

  :bind (("M-&" . company-complete))

  :init 
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-downcase nil
        company-idle-delay 0
        company-minimum-prefix-length 4))

(use-package yasnippet
  :commands yas-minor-mode
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/modules/editor/snippets"
                           "~/.emacs.d/modules/editor/yasnippet/snippets"))
  
  :config
  (yas-reload-all)
  (with-mode which-key
    (which-key-declare-prefixes "C-c &" "yasnippet")))

(use-package hideshowvis
  :diminish hs-minor-mode
  :ensure t
  :commands hideshowvis-enable
  :init
  (which-key-declare-prefixes "C-c @" "hideshow")
  (add-to-list 'hs-special-modes-alist
               (list 'nxml-mode
                     "<!--\\|<[^/>]*[^/]>"
                     "-->\\|</[^/>]*[^/]>"
                     "<!--"
                     'nxml-forward-element
                     nil))

  ;; Fix HTML folding
  (dolist (mode '(sgml-mode
                  html-mode
                  html-erb-mode))
    (add-to-list 'hs-special-modes-alist
                 (list mode
                       "<!--\\|<[^/>]*[^/]>"
                       "-->\\|</[^/>]*[^/]>"
                       "<!--"
                       'sgml-skip-tag-forward
                       nil)))

  (let ((modes '(nxml-mode-hook)))
    (apply #'4lex1v/hook-into-modes #'hideshowvis-enable modes)
    (apply #'4lex1v/hook-into-modes #'hs-minor-mode modes))
  
  :config
  (hideshowvis-symbols)
  (hideshowvis-enable))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package centered-cursor-mode
  :diminish centered-cursor-mode
  :ensure t
  :init
  (setq ccm-recenter-at-end-of-file t
        ccm-ignored-commands '(mouse-drag-region
                               mouse-set-point
                               widget-button-click
                               scroll-bar-toolkit-scroll))
  :config (global-centered-cursor-mode t))

(use-package flyspell
  :bind
  (("C-c i b" . flyspell-buffer)
   ("C-c i f" . flyspell-mode))
  :init
  (with-mode which-key
    (which-key-declare-prefixes "C-c i" "flyspell"))
  
  (use-package ispell
    :bind
    (("C-c i c" . ispell-comments-and-strings)
     ("C-c i d" . ispell-change-dictionary)
     ("C-c i k" . ispell-kill-ispell)
     ("C-c i m" . ispell-message)
     ("C-c i r" . ispell-region)))
  :config (unbind-key "C-." flyspell-mode-map))

(use-package undo-tree
  :load-path "core/undo-tree"
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("M-/" . undo-tree-visualize))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-args
  :after evil
  :config
  (add-to-list 'evil-args-delimiters " ")
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))


