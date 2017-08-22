;; -*- lexical-binding: t; -*-

(use-package smartparens-config
  :defer t
  :init
  (setq sp-autoinsert-if-followed-by-word t
        sp-autoskip-closing-pair 'always-end
        sp-hybrid-kill-entire-symbol nil))

;; TODO :: Replace bind with General
;; TODO :: Improve some combinations with Hyndra
;; NOTE :: Can we introduce another mode, like <S>, with defined Smartparens bindings? Ref :: Evil
(use-package smartparens
  :after smartparens-config
  :commands smartparens-mode
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
  (sp-pair "{" "}" :wrap "C-{"))



(defun anyfin:inject-global-backend (backend)
  (let ((active-backends (copy-tree company-backends)))
    (message "Active backends :: %s" active-backends)
    (setq company-backends (append (list backend) active-backends))))

(use-package yasnippet
  :diminish (yas-minor-mode . " Y")
  :commands yas-minor-mode
  :mode ("\\.yasnippet" . snippet-mode)
  
  :general
  ("es" '(:ignore t :which-key "Snippets")
   "esa" 'yas-new-snippet)
  
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           "~/.emacs.d/modules/editor/yasnippet/snippets")
        yas-wrap-around-region t
        yas-indent-line t)
  
  (add-hook 'after-save-hook
            (lambda ()
              (when (eq major-mode 'snippet-mode)
                (yas-reload-all))))
  
  :config
  (yas-reload-all)) 

(use-package company
  :commands company-mode

  ;; @NOTE :: For some reason can't make this work with general??
  ;; Example config with general:
  ;;   (general-define-key :keymaps 'company-active-map
  ;;     "C-j" 'company-select-next)
  ;; Though perfectly works via `define-key'
  :bind
  (:map company-active-map
   ("C-j" . company-select-next)
   ("C-k" . company-select-previous)
   ("C-d" . company-show-doc-buffer))
  
  :general
  (:prefix "" :states '(insert)
   "C-SPC" 'company-complete)

  :init 
  
  (defconst company-default-backends
    "A set of default backends used in all major-mode where company is activated")

  (setq company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-downcase nil
        
        company-idle-delay 0
        company-minimum-prefix-length 3
        
        company-selection-wrap-around t
        company-tooltip-align-annotations t

        company-transformers '(company-sort-by-occurrence)

        ;; NOTE :: no point moving this to a default var. This set will be
        ;;         extended further with other default modes like yasnippet.
        company-backends     '(company-yasnippet company-files
                              (company-abbrev company-dabbrev)
                               company-keywords company-capf))

  ;; (defun anyfin:inject-company-backends (backends-set)
  ;;   ;; default backends in this case should include all initial backends
  ;;   ;; plus additional default backends injected by minor mode like yasnippet.
  ;;   ;; On the other hand it shouldn't include other backends from major modes,
  ;;   ;; those should be buffer local by design.
  ;;   (let (default-backends (copy-tree company-backends))
  ;;     ;; Create a new buffer-local variable
  ;;     (make-local-variable 'company-backends)

  ;;     ;; TODO :: Investigate how the following code works
  ;;     (setq company-backends default-backends)
  ;;     (setf (car company-backends)
  ;;           (append backends-set (car company-backends)))))
  
  (defun company-add-mode-backends (backends-to-inject)
    (lambda ()
      (make-local-variable 'company-backends)
      (setq company-backends (copy-tree company-backends))
      
      ;;(setq company-backends (cons backends company-backends))

      ;; (setf (car company-backends)
      ;;       (append backends-to-inject (car company-backends)))

      (add-to-list 'company-backends backends-to-inject)))
  
  :config
  (add-hook 'after-init-hook #'anyfin:build-company-backends)
  (add-hook 'text-mode-hook #'company-mode))

(use-package flycheck :defer t :ensure t)

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

(diminish 'auto-revert-mode)
