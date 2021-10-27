(require 'seq)

(setq
 gc-cons-threshold       (* 100 1024 1024)  ;; 100MB
 read-process-output-max (*   1 1024 1024)) ;;   1MB

(defconst USER-EMACS-DIRECTORY (file-name-directory (or load-file-name (buffer-file-name))))
(defconst USER-INIT-FILE       (concat USER-EMACS-DIRECTORY "init.el"))

(defconst IS-MAC               (eq system-type 'darwin))
(defconst IS-WINDOWS           (eq system-type 'windows-nt))
(defconst IS-UNIX              (not IS-WINDOWS))

;; https://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab/20290#20290
(define-key input-decode-map [(control ?i)] [control-i])
(define-key input-decode-map [(control ?I)] [(shift control-i)])

(let ((font-setting
       (pcase system-type
         ('darwin "Monaco-16")
         ('windows-nt "Iosevka SS08 Slab LtEx-16"))))
  (add-to-list 'initial-frame-alist (cons 'font font-setting)) 
  (setq default-frame-alist initial-frame-alist) 
  (set-frame-font font-setting))

(defun 4l/swap-two-windows ()
  (interactive)
  (if (not (eq (length (window-list)) 2))
      (error "Can swap only two windows for now...")
    (let* ((windows-l (window-list))
           (left-window (car windows-l))
           (lw-buffer (window-buffer left-window))
           (right-window (cadr windows-l))
           (rw-buffer (window-buffer right-window)))
      (set-window-buffer left-window rw-buffer)
      (set-window-buffer right-window lw-buffer))))

(defun 4l/close-buffer (&optional arg)
  (interactive "P")
  (kill-buffer (current-buffer))
  (if (and (not (equal arg 'nil))
           (> (count-windows) 1))
      (delete-window)))

(defun 4l/close-other-window ()
  (interactive)
  (if (> (count-windows) 1) 
      (progn
        (other-window 1)
        (kill-buffer (current-buffer))
        (delete-window))))

(defun 4l/insert-block (end-with-semicolon-p)
  (interactive "P")
  (insert "{")
  (newline-and-indent)
  (newline)
  (insert (if end-with-semicolon-p "};" "}"))
  (beginning-of-line)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun 4l/close-compilation-buffer ()
  (interactive)
  (let ((comp-window (get-buffer-window "*compilation*")))
    (if comp-window 
        (quit-window nil comp-window))))

(defun 4l/open-compilation-buffer ()
  (interactive)
  (if (not (get-buffer-window "*compilation*"))
      (if (not (get-buffer "*compilation*"))
          (4l/project-rebuild)
        (progn
          (display-buffer "*compilation*")))))

(defun 4l/project-root ()
  (expand-file-name (cdr (project-current t))))

(defun 4l/open-git-bash-shell ()
  (interactive)
  "Start a git-bash.exe process on Windows as a shell"
  (let ((default-directory (4l/project-root))
        (shell-file-name (executable-find "bash.exe"))
        (explicit-bash-args '("--login -i")))
    (call-interactively 'shell)))

(defun 4l/project-compile ()
  "Run `compile' in the project root."
  (declare (interactive-only compile))
  (interactive)
  (let ((default-directory (4l/project-root)))
    (call-interactively #'compile)))

;; (add-to-list 'compilation-finish-functions
;;              (lambda (buffer result)
;;                (message "Compilation process finished %s" result)
;;                ))

(defun 4l/project-rebuild ()
  (interactive)
  (let ((default-directory (cdr (project-current t))))
    (call-interactively 'recompile)))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(fset 'yes-or-no-p   'y-or-n-p)

(show-paren-mode       t)
(delete-selection-mode t)
(global-auto-revert-mode t)
(prefer-coding-system 'utf-8-unix)
(column-number-mode 1)
(recentf-mode 1)
(blink-cursor-mode 0)
(tooltip-mode          -1)
(tool-bar-mode         -1)
(scroll-bar-mode       -1)
(menu-bar-mode         -1)

(setq-default
 auto-window-vscroll nil
 truncate-lines      nil
 initial-major-mode (quote fundamental-mode)
 mode-line-default-help-echo nil
 tab-width           2
 indent-tabs-mode    nil
 cursor-type        'box
 cursor-in-non-selected-windows 'bar
 frame-title-format "%f"
 linum-format       "%3d " 
 load-prefer-newer  t
 left-fringe-width  20
 word-wrap t
 line-spacing 2

 buffer-file-coding-system 'utf-8-unix

 case-fold-search nil
 case-replace     nil)

(setq
 inhibit-startup-message        t
 initial-scratch-message        nil
 initial-buffer-choice          nil
 ;;show-paren-delay               0.0
 ring-bell-function            'ignore
 ;;tramp-default-method          "ssh"
 make-backup-files              nil
 auto-save-default              nil
 kill-do-not-save-duplicates    t
 ad-redefinition-action        'accept
 next-line-add-newlines         nil
 desktop-save-mode              nil
 ;;desktop-save                   nil
 default-directory             "~/"
 mouse-wheel-scroll-amount     '(1)
 mouse-wheel-progressive-speed  nil
 history-delete-duplicates      t
 custom-file                   (concat USER-EMACS-DIRECTORY "custom.el")

 ;; Performance related settings
 inhibit-compacting-font-caches t
 jit-lock-defer-time 0
 fast-but-imprecise-scrolling t
 search-upper-case              nil
 dabbrev-case-distinction nil
 scroll-step 5 ;; optimal balance between scroll-speed and line flickering
 dired-listing-switches "-lah")

(setq display-buffer-alist 
      `(("\\*compilation\\*" display-buffer-in-side-window
         (side . bottom)
         (window-height . 10)
         (window-parameters . ((no-other-window . t)
                               (no-delete-other-windows . t))))))

(when IS-WINDOWS
  (let ((powershell-exe-path (executable-find "pwsh.exe")))
    (if powershell-exe-path
        (setq shell-file-name powershell-exe-path))))

;; To avoid acidental hits on the touchpad
(dolist (binding '("<mouse-1>" "<C-mouse-1>" "<C-down-mouse-1>"))
  (define-key global-map (kbd binding)
    (lambda nil (interactive) nil)))

(require 'dabbrev)
(setq
 dabbrev-case-replace t
 dabbrev-case-fold-search t
 dabbrev-case-distinction nil
 dabbrev-upcase-means-case-search t)
(abbrev-mode 1)

(when (and (require 'ls-lisp) (require 'dired))
  (setq
   ls-lisp-dirs-first t
   ls-lisp-use-insert-directory-program nil))

(setq
 package-enable-at-startup nil
 package-check-signature nil
 package--init-file-ensured t
 package-archives '(("melpa" . "https://melpa.org/packages/")))
;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(defun package--save-selected-packages (&rest opt) nil)
(require 'package) 
(package-initialize)

(defmacro ensure-installed (package-symbol)
  `(unless (locate-library ,(symbol-name package-symbol))
     (package-install ',package-symbol)))

;; (add-to-list 'load-path (concat USER-EMACS-DIRECTORY "themes/sirthias"))
;; (require 'sirthias-theme)
;; (add-hook 'after-make-frame-functions
;;             (lambda (frame)
;;               (select-frame frame)
;;               (load-theme 'sirthias t)))
;; (load-theme 'sirthias t)

(ensure-installed evil)
(setq-default
 evil-kill-on-visual-paste nil)
(setq
 evil-default-state              'normal
 evil-default-cursor             t
 evil-want-C-u-scroll            t
 evil-want-keybinding            nil
 evil-want-integration           t
 evil-ex-substitute-global       t
 evil-ex-search-vim-style-regexp t
 evil-ex-interactive-search-highlight 'selected-window)
(require 'evil)

(evil-set-leader 'normal (kbd "SPC"))

(evil-set-initial-state 'rg 'emacs)

(evil-ex-define-cmd "e[val]" #'eval-buffer)
(evil-ex-define-cmd "we" #'(lambda () (interactive) (save-buffer) (eval-buffer)))

(evil-select-search-module 'evil-search-module 'evil-search)
(evil-mode 1)

(evil-define-key nil evil-motion-state-map
  "j" 'next-line
  "k" 'previous-line)

(evil-define-key nil evil-insert-state-map
  (kbd "C-;")    'comment-line
  (kbd "C-x \\") 'align-regexp
  (kbd "C-c r")  'revert-buffer
  (kbd "M-j")   'join-line
  (kbd "M-[")   '(lambda () (interactive) (4l/insert-block nil))
  (kbd "M-{")   '(lambda () (interactive) (4l/insert-block t))
  (kbd "C-S-d")  '4l/duplicate-line)

(evil-define-key nil evil-normal-state-map
  "$"                    'evil-end-of-visual-line
  (kbd "C-j")            'evil-forward-paragraph
  (kbd "C-k")            'evil-backward-paragraph
  (kbd "<M-wheel-up>")   'text-scale-increase
  (kbd "<M-wheel-down>") 'text-scale-decrease
  (kbd "C-S-o")          'evil-jump-forward
  "m"                    'back-to-indentation)

(evil-define-key nil global-map
  (kbd "C-c r")    #'revert-buffer
  (kbd "<f7>")     #'4l/project-compile
  (kbd "<C-f7>")   #'4l/close-compilation-buffer
  (kbd "<C-M-f7>") #'4l/open-compilation-buffer
  (kbd "<f8>")     #'next-error
  (kbd "M-2")      #'ibuffer)

(ensure-installed evil-collection)
(require 'evil-collection)
(evil-collection-init)

(ensure-installed evil-args)
(when (require 'evil-args)
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(ensure-installed helm)
(setq
 helm-buffer-max-length                 nil
 helm-idle-delay                        0.0
 helm-input-idle-delay                  0.01
 helm-quick-update                      t
 helm-split-window-inside-p             t
 helm-buffers-fuzzy-matching            t
 helm-move-to-line-cycle-in-source      t
 helm-scroll-amount                     8

 helm-follow-mode-persistent            t
 helm-show-completion-display-function  nil
 helm-grep-ag-command                   "rg --vimgrep --no-heading --smart-case"

 helm-ff-search-library-in-sexp         t
 helm-ff-file-name-history-use-recentf  t
 helm-ff-fuzzy-matching                 t)
(require 'helm-config)
(require 'helm)
(defun 4l/helm-open-dired ()
  (interactive)
  (helm-exit-and-execute-action 'helm-point-file-in-dired))

(helm-mode)
(helm-autoresize-mode)
(substitute-key-definition 'find-tag 'helm-etags-select global-map)

(ensure-installed helm-xref)

(evil-define-key nil global-map
  (kbd "C-x C-f") 'helm-find-files
  (kbd "C-x f")   'helm-find-files
  (kbd "<leader> ff") 'helm-find-files
  (kbd "C-c h a") 'helm-apropos
  (kbd "C-c h s") '4l/helm-git-grep-project-files
  (kbd "M-x")     'helm-M-x
  (kbd "M-2")     'helm-mini
  (kbd "M-4")     'helm-bookmarks
  (kbd "M-:")     'helm-eval-expression-with-eldoc
  (kbd "M-i")     'helm-occur
  (kbd "<control-i>")     'helm-imenu
  (kbd "M-y")     'helm-show-kill-ring
  (kbd "M-.")     'helm-etags-select)

(evil-define-key nil helm-find-files-map
  (kbd "C-<backspace>") 'backward-kill-word
  (kbd "C-d")           '4l/helm-open-dired)

(evil-define-key nil helm-map
  (kbd "<tab>") 'helm-execute-persistent-action
  (kbd "C-i")   'helm-execute-persistent-action
  (kbd "C-z")   'helm-select-action
  (kbd "C-o")   'helm-next-source
  (kbd "C-j")   'helm-next-line
  (kbd "C-k")   'helm-previous-line
  (kbd "C-f")   'helm-toggle-full-frame)

(ensure-installed ace-window)
(require 'ace-window)

(ensure-installed rg)
(setq rg-custom-type-aliases '())
(when (require 'rg)
  (rg-enable-default-bindings)
  (if IS-WINDOWS
      (defun rg-executable ()
        (executable-find "rg.exe"))))

(ensure-installed yaml-mode)
(require 'yaml-mode)

(ensure-installed rust-mode)
(require 'rust-mode)
(setq
 rust-indent-offset  2
 rust-format-on-save nil)

(evil-define-key nil global-map
  (kbd "M-.")     'find-function-at-point
  (kbd "C-c e r") 'eval-region)

(defconst 4l/c-lang-style ;; added later under the label '4l'
  '((c-basic-offset . 2) 
    ;; add alignment in || and && expressions
    ;;      b32 result = ((value >= 'A') && (value <= 'Z')) ||
    ;;                   ((value >= 'a') && (value <= 'z'));
    ;; instead of
    ;;         b32 result = ((value >= 'A') && (value <= 'Z')) ||
    ;;           ((value >= 'a') && (value <= 'z'));
    (c-offsets-alist . ((innamespace . [0])
                        (inextern-lang . [0])
                        (case-label . +)
                        (substatement-open . 0)))))

(c-add-style "4l" 4l/c-lang-style)
(setq c-default-style "4l")

;; ;; General bindings
(evil-define-key 'normal global-map
  ;; Project related bindings
  (kbd "<leader> pf") 'project-find-file
  (kbd "<leader> ps") 'rg-project
  (kbd "<leader> pc") '4l/project-compile
  (kbd "C-w C-w") 'ace-window
  (kbd "M-u") 'universal-argument
  (kbd "<leader> fi")  '(lambda () (interactive) (find-file (concat USER-EMACS-DIRECTORY "init.el")))
  (kbd "<leader> oc") 'org-capture
  (kbd "<leader> fl") 'find-library
  (kbd "<leader> ps") 'rg-menu)

(let* ((fg1     "#ebc78a") (fg2     "#e1d0b1") (fg3     "#c7b89d") (fg4     "#a89e89")
       (bg1     "#142732") (bg2     "#101740") (bg3     "#1B3442") (bg4     "#3e5861")
       (red     "#ea614c") (green   "#a9a931") (gray    "#a0a79e") (orange  "#aaaa22"))
   (set-face-attribute 'default                             nil :foreground fg1 :background bg1) 
   (set-face-attribute 'region                              nil :background bg3) 
   (set-face-attribute 'cursor                              nil :background fg1) 
   (set-face-attribute 'fringe                              nil :background bg1)
   (set-face-attribute 'link                                nil :foreground fg1 :underline t :weight 'bold)
   (set-face-attribute 'link-visited                        nil :foreground fg1 :underline t :weight 'normal)
   (set-face-attribute 'header-line                         nil :background bg3)
   (set-face-attribute 'trailing-whitespace                 nil :foreground bg1 :background orange)
   (set-face-attribute 'lazy-highlight                      nil :foreground fg2 :background bg3)
   (set-face-attribute 'success                             nil :foreground green)
   (set-face-attribute 'warning                             nil :foreground orange)
   (set-face-attribute 'error                               nil :foreground red)
   (set-face-attribute 'mode-line                           nil :foreground bg1 :background fg1 :box nil)
   (set-face-attribute 'mode-line-inactive                  nil :foreground bg1 :background gray :box nil)
   (set-face-attribute 'mode-line-highlight                 nil :foreground fg1 :background bg1)
   (set-face-attribute 'mode-line-emphasis                  nil :foreground bg1 :background fg1 :box nil)
   (set-face-attribute 'font-lock-keyword-face              nil :foreground red)
   (set-face-attribute 'font-lock-builtin-face              nil :foreground red)
   (set-face-attribute 'font-lock-comment-face              nil :foreground gray)
   (set-face-attribute 'font-lock-constant-face             nil :foreground fg1)
   (set-face-attribute 'font-lock-doc-face                  nil :foreground gray)
   (set-face-attribute 'font-lock-function-name-face        nil :foreground fg1)
   (set-face-attribute 'font-lock-type-face                 nil :foreground fg1)
   (set-face-attribute 'font-lock-preprocessor-face         nil :foreground red)
   (set-face-attribute 'font-lock-negation-char-face        nil :foreground fg1)
   (set-face-attribute 'font-lock-string-face               nil :foreground green)
   (set-face-attribute 'font-lock-variable-name-face        nil :foreground fg1)
   (set-face-attribute 'font-lock-warning-face              nil :foreground orange :underline t)
   (set-face-attribute 'minibuffer-prompt                   nil :foreground red :bold t)
   (set-face-attribute 'isearch                             nil :foreground red :background bg3 :underline t)
   (set-face-attribute 'isearch-fail                        nil :foreground orange :background bg3 :bold t)
   (set-face-attribute 'dired-directory                     nil :foreground red :underline t)
   (set-face-attribute 'helm-header                         nil :foreground bg1 :background fg1 :bold t)
   (set-face-attribute 'helm-source-header                  nil :foreground bg1 :background fg4 :underline nil :bold t)
   (set-face-attribute 'helm-match                          nil :foreground red :underline t)
   (set-face-attribute 'helm-visible-mark                   nil :foreground orange :background bg2)
   (set-face-attribute 'helm-selection                      nil :background bg3)
   (set-face-attribute 'helm-selection-line                 nil :background bg1)
   (set-face-attribute 'helm-candidate-number               nil :foreground bg1 :background fg1)
   (set-face-attribute 'helm-separator                      nil :foreground red)
   (set-face-attribute 'helm-buffer-not-saved               nil :foreground red)
   (set-face-attribute 'helm-buffer-process                 nil :foreground fg1)
   (set-face-attribute 'helm-buffer-saved-out               nil :foreground fg1)
   (set-face-attribute 'helm-buffer-size                    nil :foreground fg1)
   (set-face-attribute 'helm-ff-directory                   nil :foreground green :weight 'bold)
   (set-face-attribute 'helm-ff-file                        nil :foreground fg1 :weight 'normal)
   (set-face-attribute 'helm-ff-executable                  nil :foreground fg1 :weight 'normal)
   (set-face-attribute 'helm-ff-invalid-symlink             nil :foreground fg1 :weight 'bold)
   (set-face-attribute 'helm-ff-symlink                     nil :foreground red :weight 'bold)
   (set-face-attribute 'helm-ff-prefix                      nil :foreground bg1 :background red :weight 'normal)
   (set-face-attribute 'helm-ff-file-extension              nil :foreground fg1 :weight 'normal)
   (set-face-attribute 'helm-grep-cmd-line                  nil :foreground fg1)
   (set-face-attribute 'helm-grep-file                      nil :foreground fg1)
   (set-face-attribute 'helm-grep-finish                    nil :foreground fg2)
   (set-face-attribute 'helm-grep-lineno                    nil :foreground fg1)
   (set-face-attribute 'helm-grep-match                     nil :foreground red)
   (set-face-attribute 'helm-moccur-buffer                  nil :foreground fg1)
   (set-face-attribute 'evil-ex-substitute-matches          nil :foreground red :underline t :strike-through t)
   (set-face-attribute 'evil-ex-substitute-replacement      nil :foreground green :underline t)
   (set-face-attribute 'rg-match-face                       nil :foreground red :background nil :underline t :bold t))
