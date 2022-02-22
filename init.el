(require 'seq)

(setq
 gc-cons-threshold       (* 100 1024 1024)  ;; 100MB
 read-process-output-max (*   1 1024 1024)) ;;   1MB

(defconst USER-EMACS-DIRECTORY (file-name-directory (or load-file-name (buffer-file-name))))
(defconst USER-INIT-FILE       (concat USER-EMACS-DIRECTORY "init.el"))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-WINDOWS (eq system-type 'windows-nt))
(defconst IS-UNIX    (not IS-WINDOWS))

(let ((font-setting
       (pcase system-type
         ('darwin "Monaco-16")
         ('windows-nt "Iosevka SS08 Slab LtEx-18")))) ;; LtEx or Extended
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
    (compile compile-command)))

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
 initial-major-mode 'fundamental-mode
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

 compilation-ask-about-save nil
 
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

(require 'abbrev)
(setq-default abbrev-mode t)

(require 'dabbrev)
(setq
 dabbrev-case-replace t
 dabbrev-case-fold-search t
 dabbrev-case-distinction nil
 dabbrev-upcase-means-case-search t)

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

(global-set-key (kbd "M-g")            #'goto-line)
(global-set-key (kbd "<M-wheel-up>")   #'text-scale-increase)
(global-set-key (kbd "<M-wheel-down>") #'text-scale-decrease)

(global-set-key (kbd "C-c r")    #'revert-buffer)
(global-set-key (kbd "<f7>")     #'4l/project-compile)
(global-set-key (kbd "<C-f7>")   #'4l/close-compilation-buffer)
(global-set-key (kbd "<C-M-f7>") #'4l/open-compilation-buffer)
(global-set-key (kbd "<f8>")     #'next-error)

;;  This is currently replaced with helm-mini, although maybe I should use ibuffer instead?
;; (global-set-key (kbd "M-2")      #'ibuffer)

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

(evil-set-initial-state 'rg   'emacs)
(evil-set-initial-state 'info 'emacs)
(evil-set-initial-state 'calc 'emacs)

(evil-ex-define-cmd "e[val]" #'eval-buffer)
(evil-ex-define-cmd "we" #'(lambda () (interactive) (save-buffer) (eval-buffer)))

(evil-select-search-module 'evil-search-module 'evil-search)
(evil-mode 1)

(with-eval-after-load "evil-mode"
  (evil-define-key nil evil-motion-state-map
    "j" 'next-line
    "k" 'previous-line
    "gD" 'xref-find-definitions-other-window)

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
    (kbd "C-S-o")          'evil-jump-forward
    "m"                    'back-to-indentation))

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

 helm-grep-ag-command                   "rg --smart-case --no-heading --line-number %s %s %s"
 helm-grep-file-path                    'relative

 helm-ff-search-library-in-sexp         t
 helm-ff-file-name-history-use-recentf  t
 helm-ff-fuzzy-matching                 t
 helm-ff-preferred-shell-mode          'shell-mode)

(require 'helm-config)
(require 'helm)
(defun 4l/helm-open-dired ()
  (interactive)
  (helm-exit-and-execute-action 'helm-point-file-in-dired))

(helm-mode)
(helm-autoresize-mode)
(substitute-key-definition 'find-tag 'helm-etags-select global-map)

(ensure-installed helm-xref)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f")   'helm-find-files)
(global-set-key (kbd "<leader> ff") 'helm-find-files)
(global-set-key (kbd "C-c h a") 'helm-apropos)
(global-set-key (kbd "C-c h s") '4l/helm-git-grep-project-files)
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "M-2")     'helm-mini)
(global-set-key (kbd "M-4")     'helm-bookmarks)
(global-set-key (kbd "M-:")     'helm-eval-expression-with-eldoc)
(global-set-key (kbd "M-i")     'helm-occur)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "M-.")     'helm-etags-select)

(define-key helm-find-files-map (kbd "C-<backspace>") 'backward-kill-word)
(define-key helm-find-files-map (kbd "C-d")           '4l/helm-open-dired)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")   'helm-select-action)
(define-key helm-map (kbd "C-o")   'helm-next-source)
(define-key helm-map (kbd "C-j")   'helm-next-line)
(define-key helm-map (kbd "C-k")   'helm-previous-line)
(define-key helm-map (kbd "C-f")   'helm-toggle-full-frame)

(ensure-installed ace-window)
(require 'ace-window)

;; (global-set-key (kbd "C-w C-w") 'ace-window)

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

(global-set-key (kbd "M-.")     'find-function-at-point)
(global-set-key (kbd "C-c e r") 'eval-region)

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

;; MSVC compilation errors regexp
(add-to-list
 'compilation-error-regexp-alist
 (list
  (concat "^" "\\([^(]+\\)" "(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?) ?: " "\\(\\(?:error\\|\\(warning\\)\\) [^ :]+\\)")
  1 2 3 '(5) nil '(4 font-lock-keyword-face)))

(c-add-style "4l" 4l/c-lang-style)
(setq c-default-style "4l")

(evil-define-key 'normal cc-mode-map
  (kbd "g o") 'ff-find-other-file)

(font-lock-add-keywords 'c++-mode '(("\\<\\(assert\\|nullptr\\|NULL\\|consteval\\)\\>" 1 font-lock-keyword-face)))
(font-lock-add-keywords 'c++-mode '(("\\<\\(assert\\|ASSERT\\|NULL\\)\\>" 1 font-lock-keyword-face)))

;; For more info on how to write skeletons - http://www.panix.com/~tehom/my-code/skel-recipe.txt
(define-skeleton cc-mutliline-comment-skeleton
  "Insert a multi-line comment"
  > "/*" \n _ \n -1 "*/")

(define-skeleton cc-multiline-todo-comment-skeleton
  "Insert a mutli-line todo comment"
  > "/*" \n
     "TODO(aivanov, " (format-time-string "%Y-%m-%d") "):" \n
  > "  " _ \n
 -1 "*/")

;; (with-eval-after-load "cc-mode"
;;   (dolist (table '(c-mode-abbrev-table c++-mode-abbrev-table))
;;     (define-abbrev table "mcom"
;;       "" 'cc-mutliline-comment-skeleton)
;;     (define-abbrev table "mtodo"
;;       "" 'cc-multiline-todo-comment-skeleton)))

(require 'org)
(setq
 org-directory "~/Dropbox/org/"
 org-startup-with-inline-images t
 org-startup-truncated nil 

 org-id-link-to-org-use-id t

 org-agenda-files              '("~/Dropbox/org/plans.org")

 org-log-done                  'time ;; When completing a task, prompt for a closing note...
 org-src-fontify-natively       t
 org-descriptive-links          t
 org-use-tag-inheritance        nil

 org-list-description-max-indent 0
 
 org-enforce-todo-dependencies  t
 org-enforce-todo-checkbox-dependencies t
 
 org-catch-invisible-edits      'error
 
 org-clock-persist              t
 
 org-hide-leading-stars         nil
 org-line-spacing               5
 org-tags-column                0 ;; Have tags next to the title

 org-ellipsis " [...]"
 
 org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "ACTIVE" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)"))
 org-todo-keyword-faces '(("ACTIVE" . "yellow"))

 org-refile-use-outline-path 'file
 org-refile-targets '((org-agenda-files . (:tag . "project")))

 org-adapt-indentation nil

 org-archive-location "./archives/%s_archive::"

 org-capture-templates
 `(("n" "New Task"       entry (file ,(concat org-directory "inbox.org"))    "* TODO %? \n")
   ("t" "Task for Today" entry (file ,(concat org-directory "plans.org"))    "* TODO %? \nSCHEDULED: %t\n"))

 ;; Stuck project is the one that has no scheduled TODO tasks
 org-stuck-projects '("+project/-DONE-CANCELLED" ("TODO") nil "SCHEDULED:\\|DEADLINE:")
 
 org-agenda-custom-commands '(("c" . "My Custom Agendas")
                              ("cu"  "Unscheduled"
                               ((todo ""
                                      ((org-agenda-overriding-header "\nUnscheduled Tasks")
                                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline)))))
                               nil ;; general settings for the whole set
                               nil))
 
 org-agenda-span 'day ;; To myself: it's better then a week, trust me.
 org-agenda-start-on-weekday 7 ;; Sunday
 org-agenda-include-diary nil
 org-agenda-skip-deadline-if-done t
 org-agenda-log-mode-items '(closed clock state)
 org-agenda-prefix-format '((agenda . "  %?-12t ")
                            (todo   . " %i %-12:c")
                            (tags   . " %i %-12:c")
                            (search . " %i %-12:c"))
 
 ;; Display agenda in full window
 org-agenda-window-setup 'current-window)

;; Force org to open file links in the same window
(add-to-list 'org-link-frame-setup '(file . find-file))

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
  (set-face-attribute 'helm-ff-directory                   nil :foreground green :background bg1 :weight 'bold)
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



