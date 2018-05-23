;; -*- lexical-binding: t; -*-

(use-package smartparens
  :commands
  (smartparens-mode
   sp-with-modes
   sp-local-pairs)
  
  :hook
  ((conf-mode text-mode) . smartparens-mode)
  
  :general
  (:keymaps 'smartparens-mode-map
   :prefix   nil
   :states  '(normal)
   
   "M-t"   'sp-transpose-sexp
   
   "C-M-k" 'sp-kill-sexp
   "C-M-w" 'sp-copy-sexp
  
   "C-s" 'hydra-smartparens/body)
  
  :init
  (setq sp-base-key-bindings nil
        sp-autoinsert-if-followed-by-word t
        sp-autoskip-closing-pair 'always-end
        sp-hybrid-kill-entire-symbol nil)
  
  (defhydra hydra-smartparens (:color pink :hint nil)
    
    "
^Slurp^         ^Barfs^
--------------------
_l_: f-slurp    _L_: f-barf
_h_: b-slurp    _H_: b-barf
--------------------
"
    
    ("l" sp-forward-slurp-sexp)
    ("h" sp-backward-slurp-sexp)
    ("L" sp-forward-barf-sexp)
    ("H" sp-backward-barf-sexp)
    ("q" nil "cancel"))
  
  :config
  (use-package smartparens-config :demand t)
  
  (sp-pair "(" ")"   :wrap "C-(")
  (sp-pair "[" "]"   :wrap "s-[")
  (sp-pair "\"" "\"" :wrap "C-\"")
  (sp-pair "<" ">"   :wrap "C-<")
  (sp-pair "{" "}"   :wrap "C-{"))

(use-package yasnippet
  :diminish (yas-minor-mode . " Y")
  :commands yas-minor-mode
  
  :mode ("\\.yasnippet" . snippet-mode)
  
  :general
  ("es" '(hydra-yasnippet/body :which-key "Snippets"))
  
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           "~/.emacs.d/modules/editor/yasnippet/snippets")
        yas-wrap-around-region t
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  
  (add-hook 'after-save-hook
            (lambda ()
              (when (eq major-mode 'snippet-mode)
                (yas-reload-all))))
  
  (defhydra hydra-yasnippet (:color blue :hint nil)
  "
^Modes^    ^Load/Visit^    ^Actions^
--------------------------------------------
_m_inor   _d_irectory      _i_nsert
_e_xtra   _f_ile           _t_ryout
^ ^       _l_ist           _n_ew
^ ^       _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("m" yas/minor-mode)
  ("a" yas-reload-all))
  
  :config
  (yas-reload-all))

(use-package company
  :commands company-mode
  
  :general
  (:prefix  nil
   :states '(insert)
   
   "C-SPC" 'company-complete)
  
  (:prefix   nil
   :keymaps 'company-active-map
   :states   nil
   
   "C-j" 'company-select-next-or-abort
   "C-k" 'company-select-previous-or-abort
   "C-o" 'company-other-backend
   "C-l" 'company-other-backend
   "C-d" 'company-show-doc-buffer)

  :hook ((text-mode) . company-mode)
  
  :init
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        
        company-idle-delay 0.3
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

(use-package flycheck
  :general
  ("ef"  '(:ignore t :which-key "Flycheck")
   "efl" 'flycheck-list-errors))

;; #TODO :: move over to appearance?
(use-package hideshowvis :ensure t
  :diminish (hs-minor-mode . " +/-")
  :commands hideshowvis-enable
  
  :hook
  ((conf-mode . hs-minor-mode)
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

;; (use-package centered-cursor-mode :demand t :ensure t
;;   :diminish centered-cursor-mode
;;   :init
;;   (setq ccm-recenter-at-end-of-file t
;;         ccm-ignored-commands '(mouse-drag-region
;;                                mouse-set-point
;;                                widget-button-click
;;                                scroll-bar-toolkit-scroll))
;;   :config (global-centered-cursor-mode t))

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
  
  :config
  (unbind-key "C-." flyspell-mode-map))

(use-package undo-tree :ensure t
  :diminish undo-tree-mode
  
  :general
  (:prefix nil
   :states 'normal

   "M-/" 'undo-tree-visualize)
  
  (:prefix   nil
   :keymaps 'undo-tree-visualizer-mode-map
   :states  '(motion)

   "j" 'undo-tree-visualize-redo
   "k" 'undo-tree-visualize-undo
   "l" 'undo-tree-visualize-switch-branch-right
   "h" 'undo-tree-visualize-switch-branch-left)
  
  :config (global-undo-tree-mode))

(use-package string-inflection :ensure t
  :general
  (:prefix  nil
   :states '(normal)

   "gu" 'string-inflection-all-cycle))

(use-package evil-surround :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-args :ensure t :demand t
  :after evil
  :config
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

(defun 4lex1v/open-in-clion ()
  "Open current position in Intellij Idea"
  (interactive)
  (let* ((line (save-excursion
                 (beginning-of-line)
                 (1+ (count-lines 1 (point)))))
         (cmd (format "clion %s:%i" buffer-file-name line)))
    (start-process-shell-command "Idea" nil cmd)))

(defun 4lex1v/open-in-intellij ()
  "Open current position in Intellij Idea"
  (interactive)
  (let* ((line (save-excursion
                 (beginning-of-line)
                 (1+ (count-lines 1 (point)))))
         (cmd (format "idea %s:%i" buffer-file-name line)))
    (start-process-shell-command "Idea" nil cmd)))

;; #NOTE :: DOESN'T REQUIRE Prefix
;; #TODO :: Should this work for normal & insert states?
(general-evil-define-key '(normal insert) 'global-map
  :prefix ""

  ;; Editor
  "C-;"    'toggle-comment-on-line
  "C-x \\" 'align-regexp
  "C-c r"  'revert-buffer
  "M-j"    'join-line
  "M-o"    '4lex1v/insert-line-and-jump
  "C-S-d"  '4lex1v/duplicate-line)


