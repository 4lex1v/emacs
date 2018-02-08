;; -*- lexical-binding: t; -*-

(use-package smartparens
  :commands (smartparens-mode sp-with-modes sp-local-pairs)
  
  :hook ((conf-mode text-mode) . smartparens-mode)
  
  :general
  (:prefix "" :keymaps 'sp-keymap
   "M-F"   'sp-forward-symbol
   "M-B"   'sp-backward-symbol
   "C-M-k" 'sp-kill-sexp
   "C-M-w" 'sp-copy-sexp
   "C-M-t" 'sp-transpose-sexp

   "M-<left>"    'sp-forward-slurp-sexp
   "C-M-<left>"  'sp-forward-barf-sexp
   "M-<right>"   'sp-backward-slurp-sexp
   "C-M-<right>" 'sp-backward-barf-sexp

   "M-D" 'sp-splice-sexp

   "C-M-[" 'sp-select-previous-thing
   "C-M-]" 'sp-select-next-thing

   "C-c s u" 'sp-up-sexp
   "C-c s d" 'sp-down-sexp
   "C-c s t" 'sp-prefix-tag-object
   "C-c s p" 'sp-prefix-pair-object
   "C-c s c" 'sp-convolute-sexp
   "C-c s a" 'sp-absorb-sexp
   "C-c s e" 'sp-emit-sexp
   "C-c s p" 'sp-add-to-previous-sexp
   "C-c s n" 'sp-add-to-next-sexp
   "C-c s j" 'sp-join-sexp
   "C-c s s" 'sp-split-sexp)
  
  :init
  (setq sp-autoinsert-if-followed-by-word t
        sp-autoskip-closing-pair 'always-end
        sp-hybrid-kill-entire-symbol nil)
  
  (defhydra hydra-smartparens (global-map "C-s")
    "smartparens"
    ("<left>"      sp-forward-slurp-sexp  "f-slurp")
    ("C-<left>"    sp-forward-barf-sexp   "f-barf")
    ("M-<right>"   sp-backward-slurp-sexp "f-slurp")
    ("C-M-<right>" sp-backward-barf-sexp  "b-barf"))
  
  :config
  (use-package smartparens-config :demand t)
  
  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "s-[")
  (sp-pair "\"" "\"" :wrap "C-\"")
  (sp-pair "{" "}" :wrap "C-{"))

(use-package yasnippet
  :diminish (yas-minor-mode . " Y")
  :commands yas-minor-mode
  
  :mode ("\\.yasnippet" . snippet-mode)
  
  :general
  ("es" '(:ignore t :which-key "Snippets")
   "esa" '(yas-new-snippet :which-key "Add Snippet"))
  
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
  
  :general
  (:prefix "" :states '(insert)
   "C-SPC" 'company-complete)
  
  (:prefix "" :keymaps 'company-active-map :states '()
   "C-j" 'company-select-next-or-abort
   "C-k" 'company-select-previous-or-abort
   "C-o" 'company-other-backend
   "C-d" 'company-show-doc-buffer)

  :hook ((text-mode) . company-mode)
  
  :init 
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-downcase nil
        
        company-idle-delay 0
        company-minimum-prefix-length 3
        
        company-selection-wrap-around t
        company-tooltip-align-annotations t

        company-transformers '(company-sort-by-occurrence)
        company-backends '())
  
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'company))

  (cl-defmacro configure-company-backends-for-mode (mode backends)
    (declare (indent 1))
    `(add-hook
      ',(intern (concat (symbol-name mode) "-hook"))
      (lambda ()
        (make-local-variable 'company-backends)
        (setq company-backends (remove nil ,backends))))))

(use-package flycheck :ensure t)

;; #TODO :: move over to appearance?
(use-package hideshowvis :ensure t
  :diminish (hs-minor-mode . " +/-")
  :commands hideshowvis-enable
  
  :hook ((conf-mode . hs-minor-mode)
         (conf-mode . hideshowvis-minor-mode))
  
  :init
  (add-to-list 'hs-special-modes-alist
               (list 'nxml-mode
                     "<!--\\|<[^/>]*[^/]>"
                     "-->\\|</[^/>]*[^/]>"
                     "<!--"
                     'nxml-forward-element
                     nil))

  ;; Fix HTML folding
  (dolist (mode '(sgml-mode html-mode html-erb-mode))
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
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package centered-cursor-mode :demand t :ensure t
  :diminish centered-cursor-mode
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

(use-package undo-tree :ensure t
  :diminish undo-tree-mode
  :bind ("M-/" . undo-tree-visualize)
  :config (global-undo-tree-mode))

(use-package evil-surround :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-args :ensure t
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

(defun 4lex1v/insert-line-and-jump (arg)
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

;; #NOTE :: DOESN'T REQUIRE Prefix
(general-evil-define-key 'insert 'global-map :prefix ""
  ;; Editor
  "C-S-d"  '4lex1v/duplicate-line
  "M-o"    '4lex1v/insert-line-and-jump
  "C-;"    'toggle-comment-on-line
  "C-x \\" 'align-regexp
  "C-c r"  'revert-buffer
  "M-j"    'join-line)

